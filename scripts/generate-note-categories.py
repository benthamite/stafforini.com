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

from __future__ import annotations

import re
import sys
from pathlib import Path

from lib import (
    ID_LINK_RE,
    NOTES_DIR,
    REPO_ROOT,
    atomic_write_json,
    build_reverse_index,
    find_org_files,
    is_dataless,
    load_id_slug_map,
)

# === Constants ===

OUTPUT_PATH = REPO_ROOT / "data" / "note-categories.json"
REVERSE_OUTPUT_PATH = REPO_ROOT / "data" / "category-notes.json"


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

    id_slug_map = load_id_slug_map(REPO_ROOT)

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
    forward = {slug: all_categories[slug] for slug in sorted(all_categories)}

    # Build reverse index (category → notes)
    sorted_reverse = build_reverse_index(
        forward,
        lambda note_slug, cat: (cat["slug"], {"slug": note_slug}),
    )

    OUTPUT_PATH.parent.mkdir(parents=True, exist_ok=True)
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
