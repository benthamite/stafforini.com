#!/usr/bin/env python3
"""Post-export front matter fixups for Hugo markdown files.

For each markdown file in content/notes/:

1. Ensures a `title` field exists (ox-hugo occasionally drops it for
   files using #+INCLUDE directives). When missing, the slug (filename
   without extension) is used as the title.

2. Sets `lastmod` in the TOML front matter to the org file's
   filesystem modification time. If the org file's mtime matches the
   initial bulk-metadata batch date, falls back to the creation date.

Usage:
    python scripts/inject-lastmod.py            # Full run
    python scripts/inject-lastmod.py --dry-run  # Preview without writing
"""

import argparse
import os
import re
from datetime import datetime
from pathlib import Path

# === Constants ===

REPO_ROOT = Path(__file__).resolve().parent.parent
CONTENT_DIR = REPO_ROOT / "content" / "notes"
ORG_DIR = Path.home() / "My Drive/notes"

# Date when ox-hugo metadata was bulk-added to all org files.
# Files whose mtime matches this date haven't been individually edited
# since, so we fall back to their creation date for lastmod.
BATCH_DATE = "2026-02-17"


# === Helpers ===


def get_org_mtime(slug):
    """Get the modification time of the org file for a given slug."""
    org_file = ORG_DIR / f"{slug}.org"
    if org_file.exists():
        mtime = os.path.getmtime(org_file)
        return datetime.fromtimestamp(mtime).strftime("%Y-%m-%d")
    return None


def get_front_matter_date(text):
    """Extract the `date` value from TOML front matter."""
    match = re.search(r"^date = (.+)$", text, re.MULTILINE)
    if match:
        return match.group(1).strip()
    return None


def get_org_title(slug):
    """Read the org source and return the heading title with markup preserved.

    Parses the first level-1 heading from the org file and converts org
    inline markup (=verbatim= and ~code~) to markdown backtick syntax.
    Returns None if the org file doesn't exist or has no heading.
    """
    org_file = ORG_DIR / f"{slug}.org"
    if not org_file.exists():
        return None

    for line in org_file.read_text().splitlines():
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


def ensure_title(md_path):
    """Ensure the TOML front matter contains a title field.

    ox-hugo occasionally omits the title (observed with #+INCLUDE files).
    When missing, the slug (filename without extension) is used as the title.

    Returns True if the file was modified, False otherwise.
    """
    text = md_path.read_text()

    match = re.match(r"(\+\+\+\n)(.*?)(\n\+\+\+)", text, re.DOTALL)
    if not match:
        return False

    front_matter = match.group(2)

    # Already has a title — nothing to do
    if re.search(r"^title\s*=", front_matter, re.MULTILINE):
        return False

    # Derive title from org source heading, falling back to slug
    title = get_org_title(md_path.stem) or md_path.stem
    # Escape double quotes for TOML
    title = title.replace('"', '\\"')
    front_matter = f'title = "{title}"\n' + front_matter

    new_text = "+++\n" + front_matter.rstrip("\n") + "\n+++" + text[match.end():]
    md_path.write_text(new_text)
    return True


def fix_title_markup(md_path):
    """Re-inject inline markup into the front matter title.

    ox-hugo strips org inline markup (=code=, ~code~) from the front matter
    title. This reads the org source heading, converts the markup to markdown
    backticks, and patches the exported title if they differ.

    Returns True if the file was modified, False otherwise.
    """
    slug = md_path.stem
    org_title = get_org_title(slug)
    if org_title is None:
        return False

    text = md_path.read_text()
    match = re.match(r"(\+\+\+\n)(.*?)(\n\+\+\+)", text, re.DOTALL)
    if not match:
        return False

    front_matter = match.group(2)
    title_match = re.search(r'^title = "(.+)"$', front_matter, re.MULTILINE)
    if not title_match:
        return False

    current_title = title_match.group(1)
    if current_title == org_title:
        return False

    front_matter = front_matter.replace(
        f'title = "{current_title}"',
        f'title = "{org_title}"',
    )

    new_text = "+++\n" + front_matter.rstrip("\n") + "\n+++" + text[match.end():]
    md_path.write_text(new_text)
    return True


def inject_lastmod(md_path, lastmod_date):
    """Inject or update lastmod in TOML front matter.

    Returns True if the file was modified, False otherwise.
    """
    text = md_path.read_text()

    # Match TOML front matter (between +++ delimiters)
    match = re.match(r"(\+\+\+\n)(.*?)(\n\+\+\+)", text, re.DOTALL)
    if not match:
        return False

    front_matter = match.group(2)

    # If org mtime is the batch date, use the creation date instead
    if lastmod_date == BATCH_DATE:
        creation_date = get_front_matter_date(front_matter)
        if creation_date:
            lastmod_date = creation_date

    # Check if lastmod already has the correct value
    if f"lastmod = {lastmod_date}" in front_matter:
        return False

    # Remove existing lastmod if present
    front_matter = re.sub(r"lastmod = .+\n?", "", front_matter)

    # Add lastmod after the date line
    front_matter = re.sub(
        r"(date = .+)", rf"\1\nlastmod = {lastmod_date}", front_matter
    )

    new_text = "+++\n" + front_matter.rstrip("\n") + "\n+++" + text[match.end():]
    md_path.write_text(new_text)
    return True


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

    stats = {"updated": 0, "unchanged": 0, "no_org": 0, "skipped": 0,
             "title_injected": 0, "title_markup_fixed": 0}

    for md_file in md_files:
        if md_file.name == "_index.md":
            stats["skipped"] += 1
            continue

        # Ensure title exists (ox-hugo sometimes drops it)
        if not args.dry_run:
            if ensure_title(md_file):
                stats["title_injected"] += 1
                print(f"  [TITLE] {md_file.name} -> injected missing title")
            if fix_title_markup(md_file):
                stats["title_markup_fixed"] += 1
                print(f"  [MARKUP] {md_file.name} -> restored title markup")
        else:
            text = md_file.read_text()
            fm_match = re.match(r"(\+\+\+\n)(.*?)(\n\+\+\+)", text, re.DOTALL)
            if fm_match and not re.search(r"^title\s*=", fm_match.group(2), re.MULTILINE):
                stats["title_injected"] += 1
                print(f"  [TITLE] {md_file.name} -> would inject title = \"{md_file.stem}\"")

        slug = md_file.stem
        lastmod = get_org_mtime(slug)

        if lastmod is None:
            stats["no_org"] += 1
            continue

        if args.dry_run:
            # Show what would happen
            if lastmod == BATCH_DATE:
                text = md_file.read_text()
                fm_match = re.match(r"(\+\+\+\n)(.*?)(\n\+\+\+)", text, re.DOTALL)
                creation = get_front_matter_date(fm_match.group(2)) if fm_match else None
                effective = creation or lastmod
                print(f"  [FALLBACK] {md_file.name} -> lastmod = {effective} (org mtime was batch date)")
            else:
                print(f"  [UPDATE] {md_file.name} -> lastmod = {lastmod}")
            stats["updated"] += 1
        else:
            if inject_lastmod(md_file, lastmod):
                stats["updated"] += 1
            else:
                stats["unchanged"] += 1

    if stats["title_injected"]:
        print(f"\n  Titles injected: {stats['title_injected']}")
    if stats["title_markup_fixed"]:
        print(f"  Title markup fixed: {stats['title_markup_fixed']}")
    print(f"\n  Updated:     {stats['updated']}")
    print(f"  Unchanged:   {stats['unchanged']}")
    print(f"  No org file: {stats['no_org']}")
    print(f"  Skipped:     {stats['skipped']}")
    if args.dry_run:
        print("  *** DRY RUN -- no files written ***")

    print("\nDone.")


if __name__ == "__main__":
    main()
