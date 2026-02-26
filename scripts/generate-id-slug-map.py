#!/usr/bin/env python3
"""Generate a JSON mapping from org-roam IDs to Hugo slugs.

Scans org-roam tag stub files in notes/tags/ and people/tags/, extracts
the :ID: property from each, and writes a {org-id: slug} mapping to
/tmp/id-slug-map.json.  Both tag types map to /notes/{slug}/.
"""

import json
import os
import re
import sys
import tempfile
from pathlib import Path

from lib import is_dataless

NOTES_TAGS_DIR = Path.home() / "My Drive/notes/tags"
PEOPLE_TAGS_DIR = Path.home() / "My Drive/people/tags"

OUTPUT_PATH = Path("/tmp/id-slug-map.json")

ID_RE = re.compile(r"^:ID:\s+(\S+)", re.MULTILINE)


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
        if is_dataless(org_file):
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
        print(f"  Skipped {skipped_dataless} dataless (cloud-evicted) file(s)")
    return mapping


def main():
    notes_map = scan_directory(NOTES_TAGS_DIR)
    people_map = scan_directory(PEOPLE_TAGS_DIR)

    # Merge (people tags also live under /notes/)
    combined = {}
    combined.update(notes_map)
    combined.update(people_map)

    tmp_fd, tmp_path = tempfile.mkstemp(dir=str(OUTPUT_PATH.parent), suffix=".tmp")
    try:
        with os.fdopen(tmp_fd, "w", encoding="utf-8") as f:
            json.dump(combined, f, indent=2, ensure_ascii=False)
            f.write("\n")
        os.replace(tmp_path, str(OUTPUT_PATH))
    except BaseException:
        os.unlink(tmp_path)
        raise

    print(f"ID-slug map written to {OUTPUT_PATH}")
    print(f"  notes/tags:  {len(notes_map)} IDs")
    print(f"  people/tags: {len(people_map)} IDs")
    print(f"  total:       {len(combined)} IDs")


if __name__ == "__main__":
    main()
