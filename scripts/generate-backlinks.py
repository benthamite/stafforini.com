#!/usr/bin/env python3
"""Generate backlinks.json from the org-roam SQLite database.

Reads the org-roam database, extracts all id-type links, maps sub-heading
nodes back to their parent page-level node, builds a reverse index, and
filters to only include pages that have been exported to Hugo.

Writes data/backlinks.json.
"""

import json
import os
import sqlite3
import sys
import tempfile

DB_PATH = os.environ.get(
    "ORGROAM_DB",
    os.path.expanduser("~/.config/emacs-profiles/var/org/org-roam.db"),
)
REPO_ROOT = os.path.dirname(os.path.dirname(__file__))
OUTPUT_PATH = os.path.join(REPO_ROOT, "data", "backlinks.json")
CONTENT_DIRS = [
    os.path.join(REPO_ROOT, "content", "notes"),
    os.path.join(REPO_ROOT, "content", "quotes"),
]


def strip_elisp_quotes(value):
    """Strip Emacs Lisp string quoting from a value (e.g. '\"id\"' -> 'id').

    The org-roam SQLite database stores strings with Elisp-style double-quote
    delimiters, so we must strip them to get the raw value.
    """
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

    # Build set of exported slugs from Hugo content directories.
    exported_slugs = set()
    for d in CONTENT_DIRS:
        if os.path.isdir(d):
            for f in os.listdir(d):
                if f.endswith(".md") and f != "_index.md":
                    exported_slugs.add(f[:-3])  # strip .md

    if not exported_slugs:
        print("WARNING: no exported pages found in content directories", file=sys.stderr)

    conn = sqlite3.connect(f"file:{DB_PATH}?mode=ro", uri=True)
    conn.row_factory = sqlite3.Row

    # Get ALL page-level nodes.  In org-roam, level 0 = file-level node (most
    # files), level 1 = first heading (used when the file node is a "tags" file
    # or the note's ID sits on the first heading instead of the file property
    # drawer).  Both levels represent "the page" for backlink purposes.
    # No directory filter — we use exported_slugs to filter the output instead.
    page_nodes = {}
    cursor = conn.execute(
        "SELECT id, file, level, title FROM nodes WHERE level <= 1",
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
        "SELECT id, file FROM nodes WHERE level > 1",
    )
    for row in cursor:
        node_id = strip_elisp_quotes(row["id"])
        subheading_to_page[node_id] = row["file"]

    # Build a file-to-page-node lookup for resolving sub-heading parents.
    file_to_page = {}
    for node_id, info in page_nodes.items():
        file_to_page[info["file"]] = node_id

    # Get all id-type links.
    # The nested quoting ('"id"') is because org-roam stores the link type
    # with Elisp string delimiters in SQLite, so the raw value is literally "id".
    links = conn.execute(
        """SELECT source, dest FROM links WHERE type = '"id"'""",
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

    # Only keep backlinks where BOTH source and destination have exported pages.
    for dest_slug in list(backlinks):
        if dest_slug not in exported_slugs:
            del backlinks[dest_slug]
            continue
        backlinks[dest_slug] = {
            (s, t) for s, t in backlinks[dest_slug] if s in exported_slugs
        }
        if not backlinks[dest_slug]:
            del backlinks[dest_slug]

    # Convert sets to sorted lists of dicts for JSON serialization.
    result = {}
    for slug, sources in sorted(backlinks.items()):
        result[slug] = sorted(
            [{"slug": s, "title": t} for s, t in sources],
            key=lambda x: x["title"].lower(),
        )

    # Write output atomically.
    from lib import atomic_write_json
    os.makedirs(os.path.dirname(OUTPUT_PATH), exist_ok=True)
    atomic_write_json(OUTPUT_PATH, result, ensure_ascii=False)

    print(f"Generated backlinks for {len(result)} notes ({sum(len(v) for v in result.values())} total backlinks)")


if __name__ == "__main__":
    main()
