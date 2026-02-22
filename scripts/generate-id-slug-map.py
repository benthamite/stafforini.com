#!/usr/bin/env python3
"""Generate a JSON mapping from org-roam IDs to Hugo slugs.

Scans org-roam tag stub files in notes/tags/ and people/tags/, extracts
the :ID: property from each, and writes a {org-id: slug} mapping to
/tmp/id-slug-map.json.  Both tag types map to /notes/{slug}/.
"""

import json
import os
import re
import stat
import sys
from pathlib import Path

NOTES_TAGS_DIR = Path.home() / "Library/CloudStorage/Dropbox/notes/tags"
PEOPLE_TAGS_DIR = Path.home() / "Library/CloudStorage/Dropbox/people/tags"

OUTPUT_PATH = Path("/tmp/id-slug-map.json")

ID_RE = re.compile(r"^:ID:\s+(\S+)", re.MULTILINE)

# macOS SF_DATALESS flag — set by FileProvider on dehydrated (cloud-only) files.
# Reading a dataless file blocks indefinitely waiting for Dropbox to hydrate it.
SF_DATALESS = 0x40000000


def _is_dataless(path: Path) -> bool:
    """Check if a file has the macOS SF_DATALESS flag (Dropbox dehydrated)."""
    try:
        return bool(os.stat(path).st_flags & SF_DATALESS)
    except (OSError, AttributeError):
        return False


def scan_directory(directory: Path) -> dict[str, str]:
    """Extract {org-id: slug} pairs from all .org files in *directory*."""
    mapping = {}
    if not directory.is_dir():
        print(f"  Warning: directory not found: {directory}", file=sys.stderr)
        return mapping

    org_files = sorted(directory.glob("*.org"))
    total = len(org_files)
    skipped_dataless = 0
    for i, org_file in enumerate(org_files, 1):
        if i % 100 == 0 or i == total:
            print(f"  Scanning {directory.name}: {i}/{total}", flush=True)
        if _is_dataless(org_file):
            skipped_dataless += 1
            continue
        try:
            text = org_file.read_text(encoding="utf-8")
        except (OSError, UnicodeDecodeError) as exc:
            print(f"  Warning: skipping {org_file.name}: {exc}", file=sys.stderr)
            continue

        match = ID_RE.search(text)
        if not match:
            continue

        org_id = match.group(1)  # preserve original case
        slug = org_file.stem      # filename without .org
        mapping[org_id] = slug

    if skipped_dataless:
        print(f"  Skipped {skipped_dataless} dataless (Dropbox dehydrated) file(s)")
    return mapping


def main():
    notes_map = scan_directory(NOTES_TAGS_DIR)
    people_map = scan_directory(PEOPLE_TAGS_DIR)

    # Merge (people tags also live under /notes/)
    combined = {}
    combined.update(notes_map)
    combined.update(people_map)

    OUTPUT_PATH.write_text(
        json.dumps(combined, indent=2, ensure_ascii=False) + "\n",
        encoding="utf-8",
    )

    print(f"ID-slug map written to {OUTPUT_PATH}")
    print(f"  notes/tags:  {len(notes_map)} IDs")
    print(f"  people/tags: {len(people_map)} IDs")
    print(f"  total:       {len(combined)} IDs")


if __name__ == "__main__":
    main()
