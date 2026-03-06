#!/usr/bin/env python3
"""Generate note-categories.json and category-notes.json from :CATEGORY: properties.

Scans all org files in ~/My Drive/notes/, finds headings with a :CATEGORY:
property containing [[id:UUID][name]] links, resolves UUIDs to note slugs
via id-slug-map.json, and writes two data files:

  data/note-categories.json — forward index: {note_slug: [{slug, title}]}
  data/category-notes.json  — reverse index: {category_slug: [{slug}]}

Usage:
    python generate-note-categories.py
"""

import json
import os
import re
import sys
from pathlib import Path

from lib import ID_LINK_RE, REPO_ROOT, atomic_write_json, is_dataless

# === Constants ===

OUTPUT_PATH = REPO_ROOT / "data" / "note-categories.json"
REVERSE_OUTPUT_PATH = REPO_ROOT / "data" / "category-notes.json"
ID_SLUG_MAP_PATH = REPO_ROOT / "data" / "id-slug-map.json"
NOTES_DIR = Path.home() / "My Drive" / "notes"

# Subdirectories to skip
SKIP_DIRS = {"tags", "people", "claude-logs"}


def find_org_files(notes_dir: Path) -> list[Path]:
    """Find all org files in notes_dir, skipping certain subdirectories."""
    org_files = []
    for path in sorted(notes_dir.rglob("*.org")):
        # Skip backup files
        if path.name.endswith("~") or path.name.startswith("."):
            continue
        # Skip excluded subdirectories
        rel = path.relative_to(notes_dir)
        parts = rel.parts
        if parts and parts[0] in SKIP_DIRS:
            continue
        org_files.append(path)
    return org_files


def process_file(org_path: Path, id_slug_map: dict) -> dict | None:
    """Process a single org file and return {note_slug: [{slug, title}]} or None."""
    text = org_path.read_text(errors="replace")

    # Quick check: does this file have a :CATEGORY: with [[id: links?
    if "[[id:" not in text or ":CATEGORY:" not in text:
        return None

    # Find the EXPORT_FILE_NAME for this note
    export_match = re.search(r":EXPORT_FILE_NAME:\s+(\S+)", text)
    if not export_match:
        return None
    note_slug = export_match.group(1)

    # Find the :CATEGORY: property value
    category_match = re.search(r":CATEGORY:\s+(.+)", text)
    if not category_match:
        return None

    category_str = category_match.group(1)
    category_links = ID_LINK_RE.findall(category_str)
    if not category_links:
        return None

    # Resolve UUIDs to note slugs
    categories = []
    for uuid, name in category_links:
        slug = id_slug_map.get(uuid.upper(), "")
        if slug:
            categories.append({"slug": slug, "title": name})

    if categories:
        return {note_slug: categories}
    return None


def main():
    if not NOTES_DIR.exists():
        print(f"ERROR: {NOTES_DIR} does not exist", file=sys.stderr)
        sys.exit(1)

    if not ID_SLUG_MAP_PATH.exists():
        print(f"ERROR: {ID_SLUG_MAP_PATH} does not exist — run generate-id-slug-map.py first",
              file=sys.stderr)
        sys.exit(1)

    id_slug_map = json.loads(ID_SLUG_MAP_PATH.read_text())

    org_files = find_org_files(NOTES_DIR)
    all_categories = {}
    files_scanned = 0
    files_skipped_dataless = 0

    for org_path in org_files:
        files_scanned += 1

        if is_dataless(org_path):
            files_skipped_dataless += 1
            continue

        result = process_file(org_path, id_slug_map)
        if result:
            all_categories.update(result)

        if files_scanned % 500 == 0:
            print(f"  ... {files_scanned} files scanned")

    # Build forward index (note → categories)
    forward = {}
    for note_slug in sorted(all_categories):
        forward[note_slug] = all_categories[note_slug]

    # Build reverse index (category → notes)
    reverse = {}
    for note_slug, categories in all_categories.items():
        for cat in categories:
            cat_slug = cat["slug"]
            if cat_slug not in reverse:
                reverse[cat_slug] = []
            reverse[cat_slug].append({"slug": note_slug})

    # Sort reverse index by key, and each value list by slug for stability
    sorted_reverse = {}
    for cat_slug in sorted(reverse):
        sorted_reverse[cat_slug] = sorted(reverse[cat_slug], key=lambda x: x["slug"])

    os.makedirs(os.path.dirname(OUTPUT_PATH), exist_ok=True)
    atomic_write_json(OUTPUT_PATH, forward, ensure_ascii=False)
    atomic_write_json(REVERSE_OUTPUT_PATH, sorted_reverse, ensure_ascii=False)

    total_notes = len(forward)
    total_categories = sum(len(v) for v in forward.values())
    total_category_pages = len(sorted_reverse)
    print(f"Generated categories for {total_notes} notes ({total_categories} total category links)")
    print(f"Generated reverse index for {total_category_pages} category pages")
    print(f"  Files scanned:      {files_scanned}")
    print(f"  Skipped (dataless): {files_skipped_dataless}")


if __name__ == "__main__":
    main()
