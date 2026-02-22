#!/usr/bin/env python3
"""Generate backlinks.json from the org-roam SQLite database.

Reads the org-roam database, extracts id-type links between nodes in the
notes, people, and bibliographic-notes directories, maps sub-heading nodes
back to their parent page-level node, builds a reverse index, and writes
data/backlinks.json.
"""

import json
import os
import re
import sqlite3
import sys

DB_PATH = os.environ.get(
    "ORGROAM_DB",
    os.path.expanduser("~/.config/emacs-profiles/var/org/org-roam.db"),
)
NOTES_DIR = os.environ.get(
    "NOTES_DIR",
    os.path.expanduser("~/Library/CloudStorage/Dropbox/notes/"),
)
PEOPLE_DIR = os.environ.get(
    "PEOPLE_DIR",
    os.path.expanduser("~/Library/CloudStorage/Dropbox/people/"),
)
BIBNOTES_DIR = os.environ.get(
    "BIBNOTES_DIR",
    os.path.expanduser("~/Library/CloudStorage/Dropbox/bibliographic-notes/"),
)
# Escape SQL LIKE wildcards in path, then wrap with % for substring match
def _escape_like(s):
    return s.replace("\\", "\\\\").replace("%", "\\%").replace("_", "\\_")

NOTES_LIKE = "%" + _escape_like(NOTES_DIR) + "%"
PEOPLE_LIKE = "%" + _escape_like(PEOPLE_DIR) + "%"
BIBNOTES_LIKE = "%" + _escape_like(BIBNOTES_DIR) + "%"
OUTPUT_PATH = os.path.join(os.path.dirname(os.path.dirname(__file__)), "data", "backlinks.json")


def strip_elisp_quotes(value):
    """Strip Emacs Lisp string quoting from a value (e.g. '"id"' -> 'id')."""
    if isinstance(value, str):
        return value.strip('"')
    return value


def file_to_slug(filepath):
    """Convert an org file path to a Hugo-compatible slug.

    e.g. "/path/to/notes/effective-altruism.org" -> effective-altruism
    Handles Elisp-quoted paths.
    """
    filepath = strip_elisp_quotes(filepath)
    basename = os.path.splitext(os.path.basename(filepath))[0]
    return basename


def main():
    if not os.path.exists(DB_PATH):
        print(f"Error: org-roam database not found at {DB_PATH}", file=sys.stderr)
        sys.exit(1)

    conn = sqlite3.connect(f"file:{DB_PATH}?mode=ro", uri=True)
    conn.row_factory = sqlite3.Row

    # Get all page-level nodes (level 0 or 1) in the notes, people, and
    # bibliographic-notes directories.  These correspond to actual Hugo pages.
    page_nodes = {}
    cursor = conn.execute(
        "SELECT id, file, level, title FROM nodes"
        " WHERE (file LIKE ? OR file LIKE ? OR file LIKE ?) AND level <= 1",
        (NOTES_LIKE, PEOPLE_LIKE, BIBNOTES_LIKE),
    )
    for row in cursor:
        node_id = strip_elisp_quotes(row["id"])
        page_nodes[node_id] = {
            "file": row["file"],
            "title": strip_elisp_quotes(row["title"]),
            "slug": file_to_slug(row["file"]),
        }

    # Get all sub-heading nodes so we can map them back to their parent
    # page-level node (by file).
    subheading_to_page = {}
    cursor = conn.execute(
        "SELECT id, file FROM nodes"
        " WHERE (file LIKE ? OR file LIKE ? OR file LIKE ?) AND level > 1",
        (NOTES_LIKE, PEOPLE_LIKE, BIBNOTES_LIKE),
    )
    for row in cursor:
        node_id = strip_elisp_quotes(row["id"])
        subheading_to_page[node_id] = row["file"]

    # Build a file-to-page-node lookup for resolving sub-heading parents.
    file_to_page = {}
    for node_id, info in page_nodes.items():
        file_to_page[info["file"]] = node_id

    # Get all id-type links where source is in any of the three directories.
    links = conn.execute(
        """
        SELECT l.source, l.dest
        FROM links l
        JOIN nodes n_src ON l.source = n_src.id
        WHERE l.type = '"id"'
        AND (n_src.file LIKE ? OR n_src.file LIKE ? OR n_src.file LIKE ?)
        """,
        (NOTES_LIKE, PEOPLE_LIKE, BIBNOTES_LIKE),
    )

    # Build the reverse index.
    # For each destination, collect the set of source pages that link to it.
    backlinks = {}  # dest_slug -> set of (src_slug, src_title)

    for row in links:
        src_id = strip_elisp_quotes(row["source"])
        dest_id = strip_elisp_quotes(row["dest"])

        # Resolve source to a page-level node.
        if src_id in page_nodes:
            src_info = page_nodes[src_id]
        elif src_id in subheading_to_page:
            src_file = subheading_to_page[src_id]
            parent_id = file_to_page.get(src_file)
            if parent_id is None:
                continue
            src_info = page_nodes[parent_id]
        else:
            continue

        # Resolve destination to a page-level node.
        if dest_id in page_nodes:
            dest_info = page_nodes[dest_id]
        elif dest_id in subheading_to_page:
            dest_file = subheading_to_page[dest_id]
            parent_id = file_to_page.get(dest_file)
            if parent_id is None:
                continue
            dest_info = page_nodes[parent_id]
        else:
            continue

        dest_slug = dest_info["slug"]
        src_slug = src_info["slug"]
        src_title = src_info["title"]

        # Don't add self-links.
        if src_slug == dest_slug:
            continue

        if dest_slug not in backlinks:
            backlinks[dest_slug] = set()
        backlinks[dest_slug].add((src_slug, src_title))

    conn.close()

    # Convert sets to sorted lists of dicts for JSON serialization.
    result = {}
    for slug, sources in sorted(backlinks.items()):
        result[slug] = sorted(
            [{"slug": s, "title": t} for s, t in sources],
            key=lambda x: x["title"].lower(),
        )

    # Write output.
    os.makedirs(os.path.dirname(OUTPUT_PATH), exist_ok=True)
    with open(OUTPUT_PATH, "w") as f:
        json.dump(result, f, indent=2, ensure_ascii=False)

    print(f"Generated backlinks for {len(result)} notes ({sum(len(v) for v in result.values())} total backlinks)")


if __name__ == "__main__":
    main()
