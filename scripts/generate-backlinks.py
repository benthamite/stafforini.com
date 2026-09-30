#!/usr/bin/env python3
"""Generate backlinks.json from the org-roam SQLite database.

Reads the org-roam database, extracts all id-type links, maps sub-heading
nodes back to their parent page-level node, builds a reverse index, and
filters to only include pages that have been exported to Hugo.

Writes data/backlinks.json.
"""

import sqlite3
import sys
from pathlib import Path

from lib import (
    ORGROAM_DB_PATH,
    REPO_ROOT,
    atomic_write_json,
    markdown_page_is_unlisted,
    slug_title_sets_to_sorted_json,
    strip_elisp_quotes,
)

OUTPUT_PATH = REPO_ROOT / "data" / "backlinks.json"
CONTENT_DIRS = [
    REPO_ROOT / "content" / "notes",
    REPO_ROOT / "content" / "quotes",
]


def file_to_slug(filepath):
    """Convert an org file path to a Hugo-compatible slug.

    e.g. "/path/to/notes/effective-altruism.org" -> effective-altruism
    Handles Elisp-quoted paths.
    """
    filepath = strip_elisp_quotes(filepath)
    return Path(filepath).stem


def discover_exported_slugs(content_dirs):
    """Return (exported_slugs, listed_source_slugs) for generated pages."""
    exported_slugs = set()
    listed_source_slugs = set()
    for d in content_dirs:
        if d.is_dir():
            for f in d.iterdir():
                if f.suffix == ".md" and f.name != "_index.md":
                    exported_slugs.add(f.stem)
                    if not markdown_page_is_unlisted(f):
                        listed_source_slugs.add(f.stem)
    return exported_slugs, listed_source_slugs


def resolve_page_nodes(rows):
    """Map every org-roam node id to the page-level node of its file.

    ROWS are (id, file, level, title, pos) tuples.  A file's page node is
    its level-0 (file-level) node when it has one; otherwise it is the
    first level-1 heading, which is where the note's ID sits in "tags"
    files and in notes whose ID is on the first heading.  Later level-1
    headings with their own IDs (e.g. a "Footnotes" heading) are sections
    of that page, not the page itself, so they must not supply its title.

    Returns {node_id: {"file", "title", "slug"}}.
    """
    rows = [
        (strip_elisp_quotes(node_id), file, level, strip_elisp_quotes(title), pos)
        for node_id, file, level, title, pos in rows
    ]
    page_rows = {}
    for row in rows:
        _, file, level, _, pos = row
        if level > 1:
            continue
        best = page_rows.get(file)
        if best is None or (level, pos) < (best[2], best[4]):
            page_rows[file] = row
    pages = {
        file: {"file": file, "title": title, "slug": file_to_slug(file)}
        for file, (_, _, _, title, _) in page_rows.items()
    }
    return {
        node_id: pages[file]
        for node_id, file, _, _, _ in rows
        if file in pages
    }


def main():
    if not ORGROAM_DB_PATH.exists():
        print(f"Error: org-roam database not found at {ORGROAM_DB_PATH}", file=sys.stderr)
        sys.exit(1)

    # Build sets of exported slugs from Hugo content directories.
    exported_slugs, listed_source_slugs = discover_exported_slugs(CONTENT_DIRS)

    if not exported_slugs:
        print("WARNING: no exported pages found in content directories", file=sys.stderr)

    conn = sqlite3.connect(f"file:{ORGROAM_DB_PATH}?mode=ro", uri=True)
    conn.row_factory = sqlite3.Row

    cursor = conn.execute("SELECT id, file, level, title, pos FROM nodes")
    node_to_page = resolve_page_nodes(
        (row["id"], row["file"], row["level"], row["title"], row["pos"])
        for row in cursor
    )

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

        # Resolve both ends to their file's page-level node.
        src_info = node_to_page.get(src_id)
        dest_info = node_to_page.get(dest_id)
        if src_info is None or dest_info is None:
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

    # Only keep backlinks where the destination has an exported page and the
    # source is a listed page.  Unlisted notes remain directly accessible, but
    # relationship indexes must not expose them as source entries.
    for dest_slug in list(backlinks):
        if dest_slug not in exported_slugs:
            del backlinks[dest_slug]
            continue
        backlinks[dest_slug] = {
            (s, t) for s, t in backlinks[dest_slug] if s in listed_source_slugs
        }
        if not backlinks[dest_slug]:
            del backlinks[dest_slug]

    # Convert sets to sorted lists of dicts for JSON serialization.
    result = slug_title_sets_to_sorted_json(backlinks)

    # Write output atomically.
    OUTPUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    atomic_write_json(OUTPUT_PATH, result, ensure_ascii=False)

    print(f"Generated backlinks for {len(result)} notes ({sum(len(v) for v in result.values())} total backlinks)")


if __name__ == "__main__":
    main()
