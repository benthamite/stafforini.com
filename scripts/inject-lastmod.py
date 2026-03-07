#!/usr/bin/env python3
"""Post-export front matter fixups for Hugo markdown files.

For each markdown file in content/notes/:

1. Ensures a `title` field exists (ox-hugo occasionally drops it for
   files using #+INCLUDE directives). When missing, the slug (filename
   without extension) is used as the title.

2. Sets `lastmod` in the TOML front matter to the org file's last git
   commit date. Falls back to filesystem mtime when git history is
   unavailable.

Usage:
    python scripts/inject-lastmod.py            # Full run
    python scripts/inject-lastmod.py --dry-run  # Preview without writing
"""

import argparse
import json
import os
import re
import subprocess
from datetime import datetime
from pathlib import Path

from lib import NOTES_DIR, REPO_ROOT, atomic_write_text, is_dataless

# === Constants ===

SCRIPT_DIR = REPO_ROOT / "scripts"
CONTENT_DIR = REPO_ROOT / "content" / "notes"
ORG_DIR = NOTES_DIR


# === Helpers ===


def _build_git_dates() -> dict[Path, str]:
    """Build a map of {org_file: last_commit_date} from git history.

    Uses a single `git log` call over the entire repo for efficiency.
    """
    result = {}
    try:
        out = subprocess.run(
            ["git", "log", "--format=%ad", "--date=short", "--name-only",
             "--diff-filter=ACMR", "--", "*.org"],
            capture_output=True, text=True, cwd=str(ORG_DIR), check=True,
        ).stdout
    except (subprocess.CalledProcessError, FileNotFoundError):
        return result

    current_date = None
    for line in out.splitlines():
        line = line.strip()
        if not line:
            continue
        # Date lines are YYYY-MM-DD
        if re.match(r"\d{4}-\d{2}-\d{2}$", line):
            current_date = line
        elif current_date and line.endswith(".org"):
            path = ORG_DIR / line
            # First occurrence = most recent commit for this file
            if path not in result:
                result[path] = current_date
    return result


# Module-level cache, populated on first use
_GIT_DATES: dict[Path, str] | None = None


def _get_git_dates() -> dict[Path, str]:
    global _GIT_DATES
    if _GIT_DATES is None:
        _GIT_DATES = _build_git_dates()
    return _GIT_DATES


def load_output_to_source_map() -> dict[str, Path]:
    """Build {output_filename: source_path} from the export manifest.

    Uses the manifest to resolve which org file produced each .md file,
    avoiding ambiguity when multiple org files share the same basename.
    """
    manifest_path = SCRIPT_DIR / ".export-notes-manifest.json"
    if not manifest_path.exists():
        return {}
    with open(manifest_path) as f:
        manifest = json.load(f)
    result = {}
    for relpath, info in manifest.get("files", {}).items():
        source = ORG_DIR / relpath
        for output in info.get("outputs", []):
            result[output] = source
    return result


def find_org_file(slug, output_map=None):
    """Find the org file for a given slug.

    Checks the export manifest first (via output_map) for an exact match,
    then falls back to searching ORG_DIR recursively.
    """
    md_name = f"{slug}.md"
    if output_map and md_name in output_map:
        source = output_map[md_name]
        if source.exists():
            return source
    # Fallback: search by filename
    for f in ORG_DIR.rglob(f"{slug}.org"):
        return f
    return None


def get_org_mtime(slug, output_map=None):
    """Get the last modification date of the org file for a given slug.

    Prefers git commit date (more reliable) over filesystem mtime
    (which gets clobbered by bulk operations and cloud sync).
    """
    org_file = find_org_file(slug, output_map)
    if not org_file:
        return None
    # Prefer git commit date
    git_dates = _get_git_dates()
    if org_file in git_dates:
        return git_dates[org_file]
    # Fallback to filesystem mtime
    mtime = os.path.getmtime(org_file)
    return datetime.fromtimestamp(mtime).strftime("%Y-%m-%d")


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


def apply_front_matter_fixups(md_path, output_map=None, lastmod_date=None):
    """Apply all front matter fixups in a single read/write cycle.

    Combines title injection, title markup fixing, and lastmod injection
    into one pass to avoid multiple non-atomic rewrites of the same file.

    Returns a dict of which fixups were applied:
      {"title_injected": bool, "title_markup_fixed": bool, "lastmod_updated": bool}
    """
    result = {"title_injected": False, "title_markup_fixed": False,
              "lastmod_updated": False, "br_stripped": False}

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
        title = title.replace('"', '\\"')
        front_matter = f'title = "{title}"\n' + front_matter
        changed = True
        result["title_injected"] = True

    # 2. Fix title markup (restore org inline markup as markdown backticks)
    org_title = get_org_title(md_path.stem, output_map)
    if org_title is not None:
        title_match = re.search(r'^title = "(.+)"$', front_matter, re.MULTILINE)
        if title_match and title_match.group(1) != org_title:
            front_matter = re.sub(
                r'^title = "' + re.escape(title_match.group(1)) + r'"',
                f'title = "{org_title}"',
                front_matter,
                count=1,
                flags=re.MULTILINE,
            )
            changed = True
            result["title_markup_fixed"] = True

    # 3. Strip trailing <br/> from body (ox-hugo artifact at paragraph ends)
    cleaned_rest = re.sub(r" <br/>$", "", rest, flags=re.MULTILINE)
    if cleaned_rest != rest:
        rest = cleaned_rest
        changed = True
        result["br_stripped"] = True

    # 4. Inject or update lastmod
    if lastmod_date is not None:
        if f"lastmod = {lastmod_date}" not in front_matter:
            front_matter = re.sub(r"lastmod = .+\n?", "", front_matter)
            front_matter = re.sub(
                r"(date = .+)", rf"\1\nlastmod = {lastmod_date}", front_matter
            )
            changed = True
            result["lastmod_updated"] = True

    if changed:
        new_text = "+++\n" + front_matter.rstrip("\n") + "\n+++" + rest
        atomic_write_text(md_path, new_text)

    return result


# === Main ===


def main():
    parser = argparse.ArgumentParser(
        description="Inject lastmod dates from org file modification times"
    )
    parser.add_argument(
        "--dry-run", action="store_true", help="Preview without writing"
    )
    args = parser.parse_args()

    if not CONTENT_DIR.exists():
        print(f"Error: {CONTENT_DIR} not found. Export from org-mode first.")
        return

    md_files = sorted(CONTENT_DIR.glob("*.md"))
    print(f"Found {len(md_files)} markdown files in {CONTENT_DIR}")

    output_map = load_output_to_source_map()
    git_dates = _get_git_dates()
    print(f"Loaded git history for {len(git_dates)} org files")

    stats = {"updated": 0, "unchanged": 0, "no_org": 0, "skipped": 0,
             "title_injected": 0, "title_markup_fixed": 0, "br_stripped": 0}

    for md_file in md_files:
        if md_file.name == "_index.md":
            stats["skipped"] += 1
            continue

        slug = md_file.stem
        lastmod = get_org_mtime(slug, output_map)

        if lastmod is None:
            stats["no_org"] += 1

        if args.dry_run:
            text = md_file.read_text()
            fm_match = re.match(r"(\+\+\+\n)(.*?)(\n\+\+\+)", text, re.DOTALL)
            if fm_match and not re.search(r"^title\s*=", fm_match.group(2), re.MULTILINE):
                stats["title_injected"] += 1
                print(f"  [TITLE] {md_file.name} -> would inject title = \"{md_file.stem}\"")
            if lastmod is not None:
                print(f"  [UPDATE] {md_file.name} -> lastmod = {lastmod}")
                stats["updated"] += 1
            else:
                stats["unchanged"] += 1
        else:
            result = apply_front_matter_fixups(md_file, output_map, lastmod)
            if result["title_injected"]:
                stats["title_injected"] += 1
                print(f"  [TITLE] {md_file.name} -> injected missing title")
            if result["title_markup_fixed"]:
                stats["title_markup_fixed"] += 1
                print(f"  [MARKUP] {md_file.name} -> restored title markup")
            if result["br_stripped"]:
                stats["br_stripped"] += 1
            if result["lastmod_updated"]:
                stats["updated"] += 1
            else:
                stats["unchanged"] += 1

    if stats["title_injected"]:
        print(f"\n  Titles injected: {stats['title_injected']}")
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
