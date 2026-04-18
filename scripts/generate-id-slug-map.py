#!/usr/bin/env python3
"""Generate a JSON mapping from org-roam IDs to Hugo slugs.

Covers two sources of published notes:

1. Published notes under ~/My Drive/notes/ — slug = EXPORT_FILE_NAME value.
   Discovered by querying the org-roam SQLite database for all nodes whose
   files live under the notes directory, then reading each unique file to
   check for :EXPORT_FILE_NAME:.  Sub-heading node IDs are mapped to their
   parent file's slug.
2. Direct filesystem scan — catches files not yet tracked by org-roam.

Writes data/id-slug-map.json.
"""

from __future__ import annotations

import os
import re
import sqlite3
import sys
from pathlib import Path

from lib import (
    NOTES_DIR,
    ORGROAM_DB_PATH,
    REPO_ROOT,
    atomic_write_json,
    is_dataless,
    strip_elisp_quotes,
)

OUTPUT_PATH = REPO_ROOT / "data" / "id-slug-map.json"
URL_OVERRIDES_PATH = REPO_ROOT / "data" / "id-url-overrides.json"
SLUG_URL_OVERRIDES_PATH = REPO_ROOT / "data" / "slug-url-overrides.json"

ID_RE = re.compile(r"^:ID:\s+(\S+)", re.MULTILINE)
# Allow leading whitespace — notes use indented property drawers under headings
ID_INDENTED_RE = re.compile(r"^\s*:ID:\s+(\S+)", re.MULTILINE)
EXPORT_FILE_NAME_RE = re.compile(r"^\s*:EXPORT_FILE_NAME:\s+(\S+)", re.MULTILINE)
EXPORT_HUGO_URL_RE = re.compile(r"^\s*:EXPORT_HUGO_URL:\s+(\S+)", re.MULTILINE)


def scan_published_notes() -> tuple[dict[str, str], dict[str, str]]:
    """Query org-roam DB and map node IDs to slugs for published notes.

    A note is "published" if its org file contains :EXPORT_FILE_NAME:.
    The slug comes from that property value (not the filename).
    Sub-heading nodes inherit their parent file's slug.

    Returns (slug_map, url_overrides) where url_overrides maps ids to the
    note's :EXPORT_HUGO_URL: value when that property is set.
    """
    if not ORGROAM_DB_PATH.exists():
        print(f"  Warning: org-roam DB not found at {ORGROAM_DB_PATH}", file=sys.stderr)
        return {}, {}

    notes_prefix = str(NOTES_DIR) + "/"

    conn = sqlite3.connect(f"file:{ORGROAM_DB_PATH}?mode=ro", uri=True)
    conn.row_factory = sqlite3.Row

    # Collect all nodes from the DB, grouped by file
    # file_nodes: {file_path: [(node_id, level), ...]}
    file_nodes: dict[str, list[tuple[str, int]]] = {}
    cursor = conn.execute("SELECT id, file, level FROM nodes")
    for row in cursor:
        node_id = strip_elisp_quotes(row["id"]).upper()
        file_path = strip_elisp_quotes(row["file"])
        level = row["level"]
        # Only include files under the notes directory
        if not file_path.startswith(notes_prefix):
            continue
        file_nodes.setdefault(file_path, []).append((node_id, level))

    conn.close()

    # For each unique file, check if it's published (has EXPORT_FILE_NAME)
    mapping = {}
    url_overrides: dict[str, str] = {}
    files_checked = 0
    files_published = 0
    skipped_dataless = 0

    unique_files = sorted(file_nodes.keys())
    total = len(unique_files)

    for i, file_path in enumerate(unique_files, 1):
        if i % 100 == 0 or i == total:
            print(f"  Scanning published notes: {i}/{total}", flush=True)

        path = Path(file_path)
        if not path.exists():
            continue
        if is_dataless(path):
            skipped_dataless += 1
            continue

        files_checked += 1
        try:
            text = path.read_text(encoding="utf-8")
        except (OSError, UnicodeDecodeError) as exc:
            print(f"  Warning: skipping {path.name}: {exc}", file=sys.stderr)
            continue

        match = EXPORT_FILE_NAME_RE.search(text)
        if not match:
            continue  # not published

        slug = match.group(1)
        files_published += 1

        url_match = EXPORT_HUGO_URL_RE.search(text)
        override_url = url_match.group(1) if url_match else None

        # Map every node ID in this file to the slug
        for node_id, _level in file_nodes[file_path]:
            mapping[node_id] = slug
            if override_url:
                url_overrides[node_id] = override_url

    if skipped_dataless:
        print(f"  Skipped {skipped_dataless} dataless (cloud-evicted) file(s)")
    print(f"  Published notes: {files_published}/{files_checked} files checked")

    return mapping, url_overrides


def scan_notes_filesystem() -> tuple[dict[str, str], dict[str, str]]:
    """Direct filesystem scan for published notes not tracked by org-roam.

    Recursively scans ~/My Drive/notes/ for .org files with EXPORT_FILE_NAME,
    extracts all :ID: properties from each, and maps them to the slug.

    Returns (slug_map, url_overrides), matching scan_published_notes.
    """
    mapping: dict[str, str] = {}
    url_overrides: dict[str, str] = {}
    if not NOTES_DIR.is_dir():
        print(f"  Warning: directory not found: {NOTES_DIR}", file=sys.stderr)
        return mapping, url_overrides

    org_files = sorted(
        p for p in NOTES_DIR.rglob("*.org") if not p.name.startswith(".")
    )
    total = len(org_files)
    skipped_dataless = 0
    files_published = 0

    for i, org_file in enumerate(org_files, 1):
        if i % 500 == 0 or i == total:
            print(f"  Filesystem scan: {i}/{total}", flush=True)

        if is_dataless(org_file):
            skipped_dataless += 1
            continue

        try:
            text = org_file.read_text(encoding="utf-8")
        except (OSError, UnicodeDecodeError):
            continue

        efn_match = EXPORT_FILE_NAME_RE.search(text)
        if not efn_match:
            continue

        slug = efn_match.group(1)
        files_published += 1

        url_match = EXPORT_HUGO_URL_RE.search(text)
        override_url = url_match.group(1) if url_match else None

        # Map ALL :ID: properties in this file to the slug
        for id_match in ID_INDENTED_RE.finditer(text):
            org_id = id_match.group(1).upper()
            mapping[org_id] = slug
            if override_url:
                url_overrides[org_id] = override_url

    if skipped_dataless:
        print(f"  Skipped {skipped_dataless} dataless (cloud-evicted) file(s)")
    print(f"  Published (filesystem): {files_published} files")
    return mapping, url_overrides


def main():
    print("--- Scanning published notes via org-roam DB ---")
    published_map, published_urls = scan_published_notes()

    print("--- Scanning notes filesystem (catch files not in org-roam) ---")
    filesystem_map, filesystem_urls = scan_notes_filesystem()

    # Merge: filesystem first, then org-roam DB (DB entries are more authoritative).
    combined = {}
    combined.update(filesystem_map)
    combined.update(published_map)

    combined_urls: dict[str, str] = {}
    combined_urls.update(filesystem_urls)
    combined_urls.update(published_urls)

    # Derive a slug → URL override map from the ID → URL overrides: every ID
    # in an overridden note maps to the same URL, so collapsing by slug gives
    # a stable slug → URL map used by file: link resolution.
    slug_url_overrides: dict[str, str] = {}
    for node_id, override_url in combined_urls.items():
        slug = combined.get(node_id)
        if slug:
            slug_url_overrides[slug] = override_url

    atomic_write_json(OUTPUT_PATH, combined, ensure_ascii=False)
    atomic_write_json(URL_OVERRIDES_PATH, combined_urls, ensure_ascii=False)
    atomic_write_json(SLUG_URL_OVERRIDES_PATH, slug_url_overrides, ensure_ascii=False)

    print(f"\nID-slug map written to {OUTPUT_PATH}")
    print(f"  published (DB):    {len(published_map)} IDs")
    print(f"  published (files): {len(filesystem_map)} IDs")
    print(f"  total (merged):    {len(combined)} IDs")
    print(f"URL overrides written to {URL_OVERRIDES_PATH}")
    print(f"  total overrides:   {len(combined_urls)} IDs")
    print(f"Slug URL overrides written to {SLUG_URL_OVERRIDES_PATH}")
    print(f"  total slug overrides: {len(slug_url_overrides)}")


if __name__ == "__main__":
    main()
