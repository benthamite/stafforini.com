#!/usr/bin/env python3
"""Migrate Topics from body text to :TOPICS: property in org headings.

One-time migration script. For each heading with a `Topics:` body line:
- Parse the [[id:UUID][name]] links from the line
- Add a :TOPICS: property to the heading's property drawer
- Remove the `Topics:` line (and any trailing blank line it leaves) from the body
- Skip headings that already have a :TOPICS: property (idempotent)

Usage:
    python migrate-topics-to-properties.py            # Full run
    python migrate-topics-to-properties.py --dry-run   # Preview without writing
"""

import argparse
import os
import re
from pathlib import Path

from lib import is_dataless

BIBLIO_NOTES_DIR = Path.home() / "My Drive" / "bibliographic-notes"

# Match a Topics line: "Topics: [[id:UUID][name]] · [[id:UUID][name]]"
TOPICS_LINE_RE = re.compile(r"^Topics:\s+(.+)$", re.MULTILINE)

# Match individual [[id:UUID][name]] links
ID_LINK_RE = re.compile(r"\[\[id:([^\]]+)\]\[([^\]]*)\]\]")

# Match a property drawer
DRAWER_RE = re.compile(r"(:PROPERTIES:\s*\n)(.*?)(:END:)", re.DOTALL)


def process_file(org_path: Path, dry_run: bool) -> dict:
    """Process a single org file. Returns stats dict."""
    stats = {"headings_migrated": 0, "headings_skipped": 0}

    text = org_path.read_text(errors="replace")

    # Quick check: does this file have any Topics body lines?
    if not TOPICS_LINE_RE.search(text):
        return stats

    # Split into heading sections for processing
    # We process the entire file as text, finding Topics lines and their
    # nearest preceding property drawer
    lines = text.split("\n")
    result_lines = []
    i = 0

    while i < len(lines):
        line = lines[i]

        # Check if this is a Topics line
        topics_match = TOPICS_LINE_RE.match(line)
        if topics_match:
            topics_str = topics_match.group(1)
            links = ID_LINK_RE.findall(topics_str)

            if not links:
                result_lines.append(line)
                i += 1
                continue

            # Build the :TOPICS: property value (space-separated links)
            topics_value = " ".join(
                f"[[id:{uuid}][{name}]]" for uuid, name in links
            )

            # Find the nearest preceding :END: to insert the property before it
            # Walk backwards through result_lines to find it
            end_idx = None
            for j in range(len(result_lines) - 1, -1, -1):
                if result_lines[j].strip() == ":END:":
                    end_idx = j
                    break

            if end_idx is None:
                # No property drawer found — keep the line as-is
                result_lines.append(line)
                i += 1
                continue

            # Check if :TOPICS: already exists in this drawer
            has_topics = False
            for j in range(end_idx - 1, -1, -1):
                stripped = result_lines[j].strip()
                if stripped == ":PROPERTIES:":
                    break
                if stripped.startswith(":TOPICS:"):
                    has_topics = True
                    break

            if has_topics:
                # Already migrated — remove the body Topics line but don't add property
                stats["headings_skipped"] += 1
                # Skip the Topics line and any following blank line
                i += 1
                if i < len(lines) and lines[i].strip() == "":
                    i += 1
                continue

            # Insert :TOPICS: property before :END:
            result_lines.insert(end_idx, f":TOPICS:  {topics_value}")
            stats["headings_migrated"] += 1

            # Skip the Topics line (don't add it to result)
            i += 1
            # Also skip trailing blank line if present
            if i < len(lines) and lines[i].strip() == "":
                i += 1
            continue

        result_lines.append(line)
        i += 1

    if stats["headings_migrated"] > 0 and not dry_run:
        new_text = "\n".join(result_lines)
        org_path.write_text(new_text)

    return stats


def main():
    parser = argparse.ArgumentParser(
        description="Migrate Topics from body text to :TOPICS: property"
    )
    parser.add_argument("--dry-run", action="store_true", help="Preview without writing")
    args = parser.parse_args()

    if not BIBLIO_NOTES_DIR.exists():
        print(f"ERROR: {BIBLIO_NOTES_DIR} does not exist")
        return

    org_files = sorted(BIBLIO_NOTES_DIR.glob("*.org"))
    total_stats = {
        "files_scanned": 0,
        "files_modified": 0,
        "headings_migrated": 0,
        "headings_skipped": 0,
        "files_skipped_dataless": 0,
    }

    for org_path in org_files:
        total_stats["files_scanned"] += 1

        if is_dataless(org_path):
            total_stats["files_skipped_dataless"] += 1
            continue

        stats = process_file(org_path, args.dry_run)
        total_stats["headings_migrated"] += stats["headings_migrated"]
        total_stats["headings_skipped"] += stats["headings_skipped"]
        if stats["headings_migrated"] > 0:
            total_stats["files_modified"] += 1
            if args.dry_run and total_stats["files_modified"] <= 10:
                print(f"  [MIGRATE] {org_path.name}: {stats['headings_migrated']} headings")

        if total_stats["files_scanned"] % 500 == 0:
            print(f"  ... {total_stats['files_scanned']} files scanned")

    print(f"  Files scanned:       {total_stats['files_scanned']}")
    print(f"  Skipped (dataless):  {total_stats['files_skipped_dataless']}")
    print(f"  Files modified:      {total_stats['files_modified']}")
    print(f"  Headings migrated:   {total_stats['headings_migrated']}")
    print(f"  Headings skipped:    {total_stats['headings_skipped']}")
    if args.dry_run:
        print("  *** DRY RUN — no files written ***")


if __name__ == "__main__":
    main()
