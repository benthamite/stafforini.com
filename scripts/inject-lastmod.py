#!/usr/bin/env python3
"""Post-export front matter fixups for Hugo markdown files.

For each markdown file in content/notes/:

1. Ensures a `title` field exists (ox-hugo occasionally drops it for
   files using #+INCLUDE directives). When missing, the slug (filename
   without extension) is used as the title.

2. Sets `lastmod` in the TOML front matter from the source org file.
   Resolution order:
     (a) `#+lastmod:` keyword in the org file (written by the Elisp
         save-hook on interactive content edits).
     (b) Last git commit touching the org file.
     (c) Filesystem mtime (last-resort fallback).

Usage:
    python scripts/inject-lastmod.py            # Full run
    python scripts/inject-lastmod.py --dry-run  # Preview without writing
"""

from __future__ import annotations

import argparse
import functools
import os
import re
import subprocess
from datetime import datetime
from pathlib import Path

from lib import (
    NOTES_DIR,
    REPO_ROOT,
    atomic_write_text,
    escape_toml_string,
    extract_export_file_names,
    find_org_files,
    is_dataless,
)

# === Constants ===

CONTENT_DIR = REPO_ROOT / "content" / "notes"
ORG_DIR = NOTES_DIR

# Matches file-level #+keyword: value lines at the top of org files.
_ORG_KEYWORD_RE = re.compile(r"^#\+(\w+):\s*(.*)$")


# === Helpers ===


@functools.lru_cache(maxsize=None)
def _read_org_keywords(org_file_str: str) -> dict[str, str]:
    """Return {lowercased_keyword: value} for file-level #+keyword: lines.

    Reads the prelude of the org file up to the first heading. Cached
    per-path since each file is parsed multiple times during a full run
    (once per keyword lookup). Missing/unreadable files return {}.
    """
    org_file = Path(org_file_str)
    try:
        text = org_file.read_text(errors="replace")
    except OSError:
        return {}
    keywords: dict[str, str] = {}
    for line in text.splitlines():
        if line.startswith("*"):
            break  # first heading ends the prelude
        m = _ORG_KEYWORD_RE.match(line)
        if m:
            keywords[m.group(1).lower()] = m.group(2).strip()
    return keywords


def _org_keyword(org_file: Path, keyword: str) -> str | None:
    """Return the value of #+keyword: in *org_file*, or None if absent/empty."""
    value = _read_org_keywords(str(org_file)).get(keyword.lower())
    return value or None


def _build_git_dates() -> tuple[dict[Path, str], dict[Path, str]]:
    """Build maps of {org_file: date} for newest and oldest commits.

    Uses a single `git log` call over the entire repo for efficiency.
    Returns (newest_dates, oldest_dates) where newest = last modified,
    oldest = first committed (creation date).
    """
    newest = {}
    oldest = {}
    try:
        out = subprocess.run(
            ["git", "log", "--format=%ad", "--date=format:%Y-%m-%dT%H:%M:%S",
             "--name-only", "--diff-filter=ACMR", "--", "*.org"],
            capture_output=True, text=True, cwd=str(ORG_DIR), check=True,
        ).stdout
    except (subprocess.CalledProcessError, FileNotFoundError):
        return newest, oldest

    current_date = None
    for line in out.splitlines():
        line = line.strip()
        if not line:
            continue
        # Date lines are YYYY-MM-DDTHH:MM:SS
        if re.match(r"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}$", line):
            current_date = line
        elif current_date and line.endswith(".org"):
            path = ORG_DIR / line
            # First occurrence = most recent commit for this file
            if path not in newest:
                newest[path] = current_date
            # Always overwrite — last occurrence = oldest commit
            oldest[path] = current_date
    return newest, oldest


# Module-level caches, populated on first use
_GIT_NEWEST: dict[Path, str] | None = None
_GIT_OLDEST: dict[Path, str] | None = None


def _ensure_git_dates():
    global _GIT_NEWEST, _GIT_OLDEST
    if _GIT_NEWEST is None:
        _GIT_NEWEST, _GIT_OLDEST = _build_git_dates()


def _get_git_dates() -> dict[Path, str]:
    """Return map of {org_file: most_recent_commit_date}."""
    _ensure_git_dates()
    return _GIT_NEWEST


def _get_git_first_dates() -> dict[Path, str]:
    """Return map of {org_file: first_commit_date}."""
    _ensure_git_dates()
    return _GIT_OLDEST


def load_output_to_source_map() -> dict[str, Path]:
    """Build {output_filename: source_path} directly from note org files."""
    result = {}
    for source in find_org_files(ORG_DIR):
        if is_dataless(source):
            continue
        for output in extract_export_file_names(source):
            existing = result.get(output)
            if existing and existing != source:
                raise ValueError(
                    f"Duplicate EXPORT_FILE_NAME output {output}: "
                    f"{existing} and {source}"
                )
            result[output] = source
    return result


def find_org_file(slug, output_map=None):
    """Find the org file for a given slug.

    Checks the source-derived output map first for an exact match,
    then falls back to searching ORG_DIR recursively.
    """
    md_name = f"{slug}.md"
    if output_map and md_name in output_map:
        source = output_map[md_name]
        if source.exists():
            return source
    # Fallback: search by filename only when unambiguous. This protects
    # single-file mode from picking the wrong org source when multiple files
    # share the same basename in different subdirectories.
    matches = list(ORG_DIR.rglob(f"{slug}.org"))
    if len(matches) == 1:
        return matches[0]
    return None


def _get_single_file_git_date(org_file):
    """Get the latest git commit date for a single org file.

    Used by --file mode to avoid scanning the entire repo history.
    """
    try:
        result = subprocess.run(
            ["git", "log", "-1", "--format=%ad", "--date=format:%Y-%m-%dT%H:%M:%S",
             "--", str(org_file.name)],
            capture_output=True, text=True,
            cwd=str(org_file.parent), check=True,
        )
        date = result.stdout.strip()
        return date if date else None
    except (subprocess.CalledProcessError, FileNotFoundError):
        return None


def _get_single_file_first_date(org_file):
    """Get the earliest git commit date for a single org file."""
    try:
        result = subprocess.run(
            ["git", "log", "--format=%ad", "--date=format:%Y-%m-%dT%H:%M:%S",
             "--diff-filter=A", "--", str(org_file.name)],
            capture_output=True, text=True,
            cwd=str(org_file.parent), check=True,
        )
        # Last line = earliest commit
        lines = [l.strip() for l in result.stdout.strip().splitlines() if l.strip()]
        return lines[-1] if lines else None
    except (subprocess.CalledProcessError, FileNotFoundError):
        return None


def get_org_mtime(slug, output_map=None):
    """Get the last modification date of the org file for a given slug.

    Resolution order:
      1. `#+lastmod:` keyword in the org file (written by the Elisp
         save-hook only on interactive content edits — the authoritative
         source when present).
      2. Last git commit touching the org file. Bulk/mechanical commits
         (property migrations, cache refreshes) perturb this, which is
         why the keyword takes precedence.
      3. Filesystem mtime — clobbered by bulk operations and cloud sync,
         used only as a last resort.
    """
    org_file = find_org_file(slug, output_map)
    if not org_file:
        return None
    keyword = _org_keyword(org_file, "lastmod")
    if keyword:
        return keyword
    git_dates = _get_git_dates()
    if org_file in git_dates:
        return git_dates[org_file]
    mtime = os.path.getmtime(org_file)
    return datetime.fromtimestamp(mtime).strftime("%Y-%m-%dT%H:%M:%S")


def get_org_creation_date(slug, output_map=None):
    """Get the first commit date of the org file for a given slug.

    Used to populate the `date` field when ox-hugo didn't set it.
    Falls back to lastmod date if first-commit history is unavailable.
    """
    org_file = find_org_file(slug, output_map)
    if not org_file:
        return None
    git_first = _get_git_first_dates()
    if org_file in git_first:
        return git_first[org_file]
    # Fallback to lastmod (better than nothing)
    return get_org_mtime(slug, output_map)


def get_front_matter_date(text):
    """Extract the `date` value from TOML front matter."""
    match = re.search(r"^date = (.+)$", text, re.MULTILINE)
    if match:
        return match.group(1).strip().strip('"')
    return None


def get_org_title(slug, output_map=None):
    """Read the org source and return the heading title with markup preserved.

    Parses the first level-1 heading from the org file and converts org
    inline markup (=verbatim= and ~code~) to markdown backtick syntax.
    Returns None if the org file doesn't exist or has no heading.
    """
    org_file = find_org_file(slug, output_map)
    if not org_file or is_dataless(org_file):
        return None

    for line in org_file.read_text(errors="replace").splitlines():
        m = re.match(r"^\*\s+(.+)", line)
        if m:
            title = m.group(1)
            # Strip org tags like :note: at end of heading
            title = re.sub(r"\s+:[\w:]+:\s*$", "", title)
            # Convert org =verbatim= and ~code~ to markdown backticks
            title = re.sub(r"=([^=]+)=", r"`\1`", title)
            title = re.sub(r"~([^~]+)~", r"`\1`", title)
            return title.strip()
    return None


def apply_front_matter_fixups(md_path, output_map=None, lastmod_date=None,
                              creation_date=None, *, write=True):
    """Apply all front matter fixups in a single read/write cycle.

    Combines title injection, date injection, title markup fixing, and
    lastmod injection into one pass to avoid multiple non-atomic rewrites.

    Returns a dict of which fixups were applied:
      {"title_injected": bool, "title_markup_fixed": bool,
       "lastmod_updated": bool, "date_injected": bool}
    """
    result = {"title_injected": False, "title_markup_fixed": False,
              "lastmod_updated": False, "br_stripped": False,
              "date_injected": False}

    text = md_path.read_text()
    match = re.match(r"(\+\+\+\n)(.*?)(\n\+\+\+)", text, re.DOTALL)
    if not match:
        return result

    front_matter = match.group(2)
    rest = text[match.end():]
    changed = False

    # 1. Ensure title exists (ox-hugo sometimes drops it)
    if not re.search(r"^title\s*=", front_matter, re.MULTILINE):
        title = get_org_title(md_path.stem, output_map) or md_path.stem
        title = escape_toml_string(title)
        front_matter = f'title = "{title}"\n' + front_matter
        changed = True
        result["title_injected"] = True

    # 2. Fix title markup (restore org inline markup as markdown backticks)
    org_title = get_org_title(md_path.stem, output_map)
    if org_title is not None:
        title_match = re.search(r'^title = "(.+)"$', front_matter, re.MULTILINE)
        escaped_org_title = escape_toml_string(org_title)
        if title_match and title_match.group(1) != escaped_org_title:
            escaped_org_title = f'title = "{escaped_org_title}"'
            front_matter = re.sub(
                r'^title = "' + re.escape(title_match.group(1)) + r'"',
                lambda m: escaped_org_title,
                front_matter,
                count=1,
                flags=re.MULTILINE,
            )
            changed = True
            result["title_markup_fixed"] = True

    # 3. Inject date when missing (from first git commit of org source)
    has_date = re.search(r"^date\s*=", front_matter, re.MULTILINE)
    if not has_date and creation_date is not None:
        # Insert after author line, or at end of front matter
        if re.search(r"^author\s*=", front_matter, re.MULTILINE):
            front_matter = re.sub(
                r"(^author\s*=\s*.+)$",
                rf"\1\ndate = {creation_date}",
                front_matter,
                count=1,
                flags=re.MULTILINE,
            )
        else:
            front_matter += f"\ndate = {creation_date}"
        changed = True
        result["date_injected"] = True

    # 4. Strip trailing <br/> from body (ox-hugo artifact at paragraph ends)
    cleaned_rest = re.sub(r" <br/>$", "", rest, flags=re.MULTILINE)
    if cleaned_rest != rest:
        rest = cleaned_rest
        changed = True
        result["br_stripped"] = True

    # 5. Inject or update lastmod
    if lastmod_date is not None:
        if f"lastmod = {lastmod_date}" not in front_matter:
            front_matter = re.sub(r"lastmod = .+\n?", "", front_matter)
            if re.search(r"^date\s*=", front_matter, re.MULTILINE):
                front_matter = re.sub(
                    r"(^date\s*=\s*.+)$",
                    rf"\1\nlastmod = {lastmod_date}",
                    front_matter,
                    count=1,
                    flags=re.MULTILINE,
                )
            else:
                # No date line — append lastmod at end of front matter
                front_matter += f"\nlastmod = {lastmod_date}"
            changed = True
            result["lastmod_updated"] = True

    if changed and write:
        new_text = "+++\n" + front_matter.rstrip("\n") + "\n+++" + rest
        atomic_write_text(md_path, new_text)

    return result


# === Main ===


def process_single_file(md_path, dry_run=False):
    """Process a single markdown file (used by --file mode).

    Uses targeted git queries instead of scanning the full repo,
    making this fast enough to call after each interactive export.
    """
    md_path = Path(md_path)
    if not md_path.exists():
        print(f"Error: {md_path} not found.")
        return

    output_map = load_output_to_source_map()
    slug = md_path.stem
    org_file = find_org_file(slug, output_map)
    if not org_file:
        print(f"Warning: no org source found for {slug}")
        return

    lastmod = _org_keyword(org_file, "lastmod")
    if not lastmod:
        lastmod = _get_single_file_git_date(org_file)
    if not lastmod:
        # Fallback to filesystem mtime
        mtime = os.path.getmtime(org_file)
        lastmod = datetime.fromtimestamp(mtime).strftime("%Y-%m-%dT%H:%M:%S")

    creation = _get_single_file_first_date(org_file) or lastmod

    result = apply_front_matter_fixups(
        md_path,
        output_map,
        lastmod,
        creation,
        write=not dry_run,
    )
    prefix = "would " if dry_run else ""
    if result["lastmod_updated"]:
        print(f"  [LASTMOD] {md_path.name} -> {prefix}set lastmod = {lastmod}")
    if result["title_injected"]:
        print(f"  [TITLE] {md_path.name} -> {prefix}inject missing title")
    if result["date_injected"]:
        print(f"  [DATE] {md_path.name} -> {prefix}inject missing date")
    if result["title_markup_fixed"]:
        print(f"  [MARKUP] {md_path.name} -> {prefix}restore title markup")


def main():
    parser = argparse.ArgumentParser(
        description="Inject lastmod dates from org file modification times"
    )
    parser.add_argument(
        "--dry-run", action="store_true", help="Preview without writing"
    )
    parser.add_argument(
        "--file", metavar="MD_PATH",
        help="Process a single markdown file (fast mode for interactive exports)"
    )
    args = parser.parse_args()

    if args.file:
        process_single_file(args.file, dry_run=args.dry_run)
        return

    if not CONTENT_DIR.exists():
        print(f"Error: {CONTENT_DIR} not found. Export from org-mode first.")
        return

    md_files = sorted(CONTENT_DIR.glob("*.md"))
    print(f"Found {len(md_files)} markdown files in {CONTENT_DIR}")

    output_map = load_output_to_source_map()
    git_dates = _get_git_dates()
    print(f"Loaded git history for {len(git_dates)} org files")

    stats = {"updated": 0, "unchanged": 0, "no_org": 0, "skipped": 0,
             "title_injected": 0, "title_markup_fixed": 0, "br_stripped": 0,
             "date_injected": 0}

    for md_file in md_files:
        if md_file.name == "_index.md":
            stats["skipped"] += 1
            continue

        slug = md_file.stem
        lastmod = get_org_mtime(slug, output_map)
        creation = get_org_creation_date(slug, output_map)

        if lastmod is None:
            stats["no_org"] += 1

        result = apply_front_matter_fixups(
            md_file,
            output_map,
            lastmod,
            creation,
            write=not args.dry_run,
        )
        prefix = "would " if args.dry_run else ""
        if result["title_injected"]:
            stats["title_injected"] += 1
            print(f"  [TITLE] {md_file.name} -> {prefix}inject missing title")
        if result["date_injected"]:
            stats["date_injected"] += 1
            print(f"  [DATE] {md_file.name} -> {prefix}inject missing date" + (f" = {creation}" if args.dry_run and creation else ""))
        if result["title_markup_fixed"]:
            stats["title_markup_fixed"] += 1
            print(f"  [MARKUP] {md_file.name} -> {prefix}restore title markup")
        if result["br_stripped"]:
            stats["br_stripped"] += 1
            if args.dry_run:
                print(f"  [MARKUP] {md_file.name} -> would strip trailing <br/>")
        if result["lastmod_updated"]:
            print(f"  [LASTMOD] {md_file.name} -> {prefix}set lastmod = {lastmod}")

        if any(result.values()):
            stats["updated"] += 1
        else:
            stats["unchanged"] += 1

    if stats["title_injected"]:
        print(f"\n  Titles injected: {stats['title_injected']}")
    if stats["date_injected"]:
        print(f"  Dates injected:  {stats['date_injected']}")
    if stats["title_markup_fixed"]:
        print(f"  Title markup fixed: {stats['title_markup_fixed']}")
    if stats["br_stripped"]:
        print(f"  Trailing <br/> stripped: {stats['br_stripped']}")
    print(f"\n  Updated:     {stats['updated']}")
    print(f"  Unchanged:   {stats['unchanged']}")
    print(f"  No org file: {stats['no_org']}")
    print(f"  Skipped:     {stats['skipped']}")
    if args.dry_run:
        print("  *** DRY RUN — no files written ***")

    print("\nDone.")


if __name__ == "__main__":
    main()
