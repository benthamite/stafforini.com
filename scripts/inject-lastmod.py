#!/usr/bin/env python3
"""Inject lastmod dates into exported Hugo markdown files.

For each markdown file in content/notes/, finds the corresponding org
source file and sets `lastmod` in the TOML front matter to the org
file's filesystem modification time.

If the org file's mtime matches the prepare-org-notes.py batch date
(all files were touched on the same day), falls back to the creation
date from front matter, so notes that haven't been individually edited
since don't get a misleading "last updated" date.

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
ORG_DIR = Path.home() / "Library/CloudStorage/Dropbox/websites/pablos-miscellany"

# Date when prepare-org-notes.py bulk-modified all org files.
# Files with this mtime haven't been individually edited since.
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

    stats = {"updated": 0, "unchanged": 0, "no_org": 0, "skipped": 0}

    for md_file in md_files:
        if md_file.name == "_index.md":
            stats["skipped"] += 1
            continue

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

    print(f"\n  Updated:     {stats['updated']}")
    print(f"  Unchanged:   {stats['unchanged']}")
    print(f"  No org file: {stats['no_org']}")
    print(f"  Skipped:     {stats['skipped']}")
    if args.dry_run:
        print("  *** DRY RUN -- no files written ***")

    print("\nDone.")


if __name__ == "__main__":
    main()
