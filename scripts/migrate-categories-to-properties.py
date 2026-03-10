#!/usr/bin/env python3
"""Migrate :CATEGORY: from plain text to [[id:UUID][name]] links in org files.

One-time migration script. For each org file with a :CATEGORY: property:
- Parse comma-separated plain text values
- Filter out non-substantive values (Uncategorized, Misc, empty)
- Look up each category's org-roam UUID via id-slug-map.json
- Replace the property value with space-separated [[id:UUID][name]] links
- Remove the body "Categories:" line (and any trailing blank line)
- If no substantive values remain, remove the :CATEGORY: line entirely

Usage:
    python migrate-categories-to-properties.py            # Full run
    python migrate-categories-to-properties.py --dry-run   # Preview without writing
"""

import argparse
import json
import re
from pathlib import Path

from lib import NOTES_DIR, REPO_ROOT, atomic_write_text, find_org_files, is_dataless

ID_SLUG_MAP_PATH = REPO_ROOT / "data" / "id-slug-map.json"

# Non-substantive category values to filter out
NON_SUBSTANTIVE = {"Uncategorized", "uncategorized", "Misc", "misc", ""}

# Map from category display name → note slug (all verified)
CATEGORY_SLUG_MAP = {
    "Anki": "anki",
    "Announcements": "announcements",
    "Bibliographies": "bibliographies",
    "Books": "books",
    "Effective altruism": "effective-altruism",
    "Film": "film",
    "Learning": "learning",
    "Longtermism": "longtermism",
    "Minimalism": "minimalism",
    "Personal": "personal",
    "Philosophy": "philosophy",
    "Productivity": "productivity",
    "Self-help": "self-help",
    "Things I like": "things-i-like",
    "Translations": "translations",
    "Writing": "writing",
    "Writings by others": "writings-by-others",
}

# Match a body Categories line: "Categories: [[file:...][...]], [[file:...][...]]"
CATEGORIES_LINE_RE = re.compile(r"^Categories:\s+.+$", re.MULTILINE)


def build_slug_to_uuid(id_slug_map: dict) -> dict:
    """Build a reverse map: slug → UUID (first match wins)."""
    slug_to_uuid = {}
    for uuid, slug in id_slug_map.items():
        if slug not in slug_to_uuid:
            slug_to_uuid[slug] = uuid
    return slug_to_uuid


def process_file(org_path: Path, slug_to_uuid: dict, dry_run: bool) -> dict:
    """Process a single org file. Returns stats dict."""
    stats = {
        "category_converted": False,
        "category_removed": False,
        "body_removed": False,
    }

    text = org_path.read_text(errors="replace")

    # Quick check: does this file have a :CATEGORY: property?
    if ":CATEGORY:" not in text:
        return stats

    lines = text.split("\n")
    result_lines = []
    modified = False
    i = 0

    while i < len(lines):
        line = lines[i]
        stripped = line.strip()

        # Check if this is a :CATEGORY: property line
        if stripped.startswith(":CATEGORY:"):
            # Extract the value after :CATEGORY:
            value = stripped[len(":CATEGORY:"):].strip()

            # Already migrated (contains [[id: links)?
            if "[[id:" in value:
                result_lines.append(line)
                i += 1
                continue

            # Parse comma-separated category names
            categories = [c.strip() for c in value.split(",")]
            # Filter out non-substantive values
            substantive = [c for c in categories if c not in NON_SUBSTANTIVE]

            if not substantive:
                # Remove the :CATEGORY: line entirely
                stats["category_removed"] = True
                modified = True
                i += 1
                continue

            # Look up UUIDs for each category
            links = []
            for cat_name in substantive:
                slug = CATEGORY_SLUG_MAP.get(cat_name)
                if not slug:
                    print(f"  WARNING: Unknown category '{cat_name}' in {org_path.name}")
                    continue
                uuid = slug_to_uuid.get(slug)
                if not uuid:
                    print(f"  WARNING: No UUID for slug '{slug}' in {org_path.name}")
                    continue
                links.append(f"[[id:{uuid}][{cat_name}]]")

            if links:
                # Preserve indentation from original line
                indent = line[:len(line) - len(line.lstrip())]
                new_line = f"{indent}:CATEGORY: {' '.join(links)}"
                result_lines.append(new_line)
                stats["category_converted"] = True
                modified = True
            else:
                # All categories were unknown — remove the line
                stats["category_removed"] = True
                modified = True

            i += 1
            continue

        # Check if this is a body "Categories:" line
        categories_match = CATEGORIES_LINE_RE.match(line)
        if categories_match:
            # Remove this line
            stats["body_removed"] = True
            modified = True
            i += 1
            # Also skip trailing blank line if present
            if i < len(lines) and lines[i].strip() == "":
                i += 1
            continue

        result_lines.append(line)
        i += 1

    if modified and not dry_run:
        new_text = "\n".join(result_lines)
        atomic_write_text(org_path, new_text)

    return stats


def main():
    parser = argparse.ArgumentParser(
        description="Migrate :CATEGORY: from plain text to [[id:UUID][name]] links"
    )
    parser.add_argument("--dry-run", action="store_true", help="Preview without writing")
    args = parser.parse_args()

    if not NOTES_DIR.exists():
        print(f"ERROR: {NOTES_DIR} does not exist")
        return

    if not ID_SLUG_MAP_PATH.exists():
        print(f"ERROR: {ID_SLUG_MAP_PATH} does not exist — run generate-id-slug-map.py first")
        return

    id_slug_map = json.loads(ID_SLUG_MAP_PATH.read_text())
    slug_to_uuid = build_slug_to_uuid(id_slug_map)

    org_files = find_org_files(NOTES_DIR)
    total_stats = {
        "files_scanned": 0,
        "files_converted": 0,
        "files_removed_category": 0,
        "files_body_removed": 0,
        "files_skipped_dataless": 0,
    }

    for org_path in org_files:
        total_stats["files_scanned"] += 1

        if is_dataless(org_path):
            total_stats["files_skipped_dataless"] += 1
            continue

        stats = process_file(org_path, slug_to_uuid, args.dry_run)

        if stats["category_converted"]:
            total_stats["files_converted"] += 1
            if args.dry_run and total_stats["files_converted"] <= 15:
                print(f"  [CONVERT] {org_path.name}")
        if stats["category_removed"]:
            total_stats["files_removed_category"] += 1
            if args.dry_run and total_stats["files_removed_category"] <= 15:
                print(f"  [REMOVE]  {org_path.name}")
        if stats["body_removed"]:
            total_stats["files_body_removed"] += 1

    print(f"  Files scanned:              {total_stats['files_scanned']}")
    print(f"  Skipped (dataless):         {total_stats['files_skipped_dataless']}")
    print(f"  Categories converted:       {total_stats['files_converted']}")
    print(f"  Category lines removed:     {total_stats['files_removed_category']}")
    print(f"  Body 'Categories:' removed: {total_stats['files_body_removed']}")
    if args.dry_run:
        print("  *** DRY RUN — no files written ***")


if __name__ == "__main__":
    main()
