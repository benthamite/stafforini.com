#!/usr/bin/env python3
"""Add ox-hugo export metadata to blog org files.

Reads each .org file in the pablos-miscellany directory and adds the
ox-hugo keywords needed for per-subtree export:

  File level:
    #+hugo_base_dir: ~/Library/CloudStorage/Dropbox/repos/stafforini.com/

  Level-1 heading PROPERTIES:
    :EXPORT_FILE_NAME: <slug>
    :EXPORT_HUGO_SECTION: notes
    :EXPORT_DATE: <date>          (from :POST_DATE:)
    :EXPORT_HUGO_DRAFT: true      (if no date)

Idempotent: skips files that already have these properties.

Usage:
    python prepare-org-notes.py                # Full run
    python prepare-org-notes.py --dry-run      # Preview without writing
"""

import argparse
import re
from pathlib import Path

# === Constants ===

ORG_DIR = Path.home() / "Library/CloudStorage/Dropbox/websites/pablos-miscellany"
HUGO_BASE_DIR = "~/Library/CloudStorage/Dropbox/repos/stafforini.com/"

# Files to skip (not blog posts)
SKIP_FILES = {"pablos-miscellany.org"}


# === Helpers ===


def parse_post_date(date_str):
    """Extract YYYY-MM-DD from org timestamp like [2013-05-25 Sat 20:11]."""
    m = re.search(r"\[(\d{4}-\d{2}-\d{2})", date_str)
    if m:
        return m.group(1)
    return None


def file_to_slug(filepath):
    """Derive the Hugo slug from the org filename."""
    return filepath.stem


def has_export_metadata(text):
    """Check if the file already has ox-hugo export metadata."""
    return ":EXPORT_FILE_NAME:" in text


def add_metadata(text, slug):
    """Add ox-hugo metadata to an org file.

    Adds #+hugo_base_dir at the file level (after #+title:) and
    export properties to the level-1 heading's PROPERTIES drawer.
    """
    lines = text.split("\n")
    result = []
    added_base_dir = False
    in_properties = False
    found_post_date = None
    added_export_props = False

    i = 0
    while i < len(lines):
        line = lines[i]

        # Add #+hugo_base_dir after #+title: line
        if not added_base_dir and line.startswith("#+title:"):
            result.append(line)
            result.append(f"#+hugo_base_dir: {HUGO_BASE_DIR}")
            added_base_dir = True
            i += 1
            continue

        # Track PROPERTIES drawer under level-1 heading
        if re.match(r"^\* ", line) and not in_properties:
            result.append(line)
            i += 1
            # Look for PROPERTIES drawer start
            if i < len(lines):
                props_line = lines[i]
                if re.match(r"\s*:PROPERTIES:", props_line):
                    in_properties = True
                    result.append(props_line)
                    i += 1
                    # Scan through existing properties to find POST_DATE and :END:
                    prop_lines = []
                    while i < len(lines):
                        pline = lines[i]
                        if re.match(r"\s*:END:", pline):
                            # Found end of drawer — extract POST_DATE if present
                            for pl in prop_lines:
                                m = re.match(r"\s*:POST_DATE:\s*(.+)", pl)
                                if m:
                                    found_post_date = parse_post_date(m.group(1).strip())
                            # Add export properties before :END:
                            if not added_export_props:
                                result.extend(prop_lines)
                                result.append(f"  :EXPORT_FILE_NAME: {slug}")
                                result.append("  :EXPORT_HUGO_SECTION: notes")
                                if found_post_date:
                                    result.append(f"  :EXPORT_DATE: {found_post_date}")
                                else:
                                    result.append("  :EXPORT_HUGO_DRAFT: true")
                                added_export_props = True
                            else:
                                result.extend(prop_lines)
                            result.append(pline)  # :END:
                            in_properties = False
                            i += 1
                            break
                        else:
                            prop_lines.append(pline)
                            i += 1
                    continue
            continue

        result.append(line)
        i += 1

    # If no #+hugo_base_dir was added (no #+title: line), prepend it
    if not added_base_dir:
        result.insert(0, f"#+hugo_base_dir: {HUGO_BASE_DIR}")

    return "\n".join(result)


# === Main ===


def main():
    parser = argparse.ArgumentParser(
        description="Add ox-hugo export metadata to blog org files"
    )
    parser.add_argument("--dry-run", action="store_true", help="Preview without writing")
    args = parser.parse_args()

    org_files = sorted(ORG_DIR.glob("*.org"))
    print(f"Found {len(org_files)} org files in {ORG_DIR}")

    stats = {"modified": 0, "skipped_has_meta": 0, "skipped_other": 0}

    for org_file in org_files:
        if org_file.name in SKIP_FILES:
            stats["skipped_other"] += 1
            continue

        text = org_file.read_text()

        if has_export_metadata(text):
            stats["skipped_has_meta"] += 1
            continue

        slug = file_to_slug(org_file)
        new_text = add_metadata(text, slug)

        if new_text != text:
            stats["modified"] += 1
            if args.dry_run:
                print(f"  [MODIFY] {org_file.name} → slug={slug}")
            else:
                org_file.write_text(new_text)
                print(f"  Modified: {org_file.name}")
        else:
            stats["skipped_other"] += 1

    print(f"\n  Modified:            {stats['modified']}")
    print(f"  Skipped (has meta):  {stats['skipped_has_meta']}")
    print(f"  Skipped (other):     {stats['skipped_other']}")
    if args.dry_run:
        print("  *** DRY RUN — no files written ***")

    print("\nDone.")


if __name__ == "__main__":
    main()
