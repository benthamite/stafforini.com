#!/usr/bin/env python3
"""Generate quote-topics.json from :TOPICS: properties in org files.

Scans all org files in ~/My Drive/bibliographic-notes/, finds headings with
a :TOPICS: property, resolves topic UUIDs to note slugs via id-slug-map.json,
and writes data/quote-topics.json.

Each entry maps a quote slug to its list of topics:
    {
      "schelling-identifiable-victim": [
        {"slug": "identifiable-victim", "title": "identifiable victim"}
      ]
    }

Usage:
    python generate-quote-topics.py
"""

import sys
from pathlib import Path

from lib import (
    BIBLIO_NOTES_DIR,
    ID_LINK_RE,
    REPO_ROOT,
    atomic_write_json,
    build_reverse_index,
    cite_key_to_slug,
    extract_roam_refs,
    find_ancestor_with_export,
    is_dataless,
    load_id_slug_map,
    make_non_diary_slug,
    parse_org_headings,
)

# === Constants ===

OUTPUT_PATH = REPO_ROOT / "data" / "quote-topics.json"
REVERSE_OUTPUT_PATH = REPO_ROOT / "data" / "topic-quotes.json"


def process_file(org_path: Path, id_slug_map: dict) -> dict:
    """Process a single org file and return {quote_slug: {topics, work_slug}}."""
    text = org_path.read_text(errors="replace")

    cite_key = extract_roam_refs(text)
    if not cite_key:
        return {}

    work_slug = cite_key_to_slug(cite_key)
    headings = parse_org_headings(text)
    result = {}

    for i, h in enumerate(headings):
        # Skip top-level heading
        if h["level"] == 1:
            continue

        topics_str = h["properties"].get("TOPICS", "")
        if not topics_str:
            continue

        # Parse topic links
        topic_links = ID_LINK_RE.findall(topics_str)
        if not topic_links:
            continue

        # Determine the quote slug
        export_name = h["properties"].get("EXPORT_FILE_NAME", "")
        if export_name:
            # Diary quote
            quote_slug = export_name
        else:
            # Non-diary quote: need heading ID for slug generation
            heading_id = h["properties"].get("ID", "")
            if not heading_id:
                continue
            # Skip if nested under an ancestor with EXPORT_FILE_NAME
            if find_ancestor_with_export(headings, i):
                continue
            quote_slug = make_non_diary_slug(work_slug, heading_id)

        # Resolve topic UUIDs to note slugs
        topics = []
        for uuid, name in topic_links:
            slug = id_slug_map.get(uuid.upper(), "")
            if slug:
                topics.append({"slug": slug, "title": name})

        if topics:
            result[quote_slug] = {"topics": topics, "work_slug": work_slug}

    return result


def main():
    if not BIBLIO_NOTES_DIR.exists():
        print(f"ERROR: {BIBLIO_NOTES_DIR} does not exist", file=sys.stderr)
        sys.exit(1)

    id_slug_map = load_id_slug_map(REPO_ROOT)

    org_files = sorted(BIBLIO_NOTES_DIR.glob("*.org"))
    all_topics = {}
    files_scanned = 0
    files_skipped_dataless = 0

    for org_path in org_files:
        files_scanned += 1

        if is_dataless(org_path):
            files_skipped_dataless += 1
            continue

        file_topics = process_file(org_path, id_slug_map)
        all_topics.update(file_topics)

        if files_scanned % 500 == 0:
            print(f"  ... {files_scanned} files scanned")

    # Build forward index (quote → topics) preserving the flat list format
    # that the Hugo template expects, and a reverse index (topic → quotes).
    forward = {}
    work_slugs = {}  # quote_slug -> work_slug (for reverse index)

    for quote_slug, info in sorted(all_topics.items()):
        forward[quote_slug] = info["topics"]
        work_slugs[quote_slug] = info["work_slug"]

    sorted_reverse = build_reverse_index(
        forward,
        lambda quote_slug, topic: (
            topic["slug"],
            {"slug": quote_slug, "work_slug": work_slugs[quote_slug]},
        ),
    )

    OUTPUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    atomic_write_json(OUTPUT_PATH, forward, ensure_ascii=False)
    atomic_write_json(REVERSE_OUTPUT_PATH, sorted_reverse, ensure_ascii=False)

    total_quotes = len(forward)
    total_topics = sum(len(v) for v in forward.values())
    total_topic_pages = len(sorted_reverse)
    print(f"Generated topics for {total_quotes} quotes ({total_topics} total topic links)")
    print(f"Generated reverse index for {total_topic_pages} topic pages")
    print(f"  Files scanned:      {files_scanned}")
    print(f"  Skipped (dataless): {files_skipped_dataless}")


if __name__ == "__main__":
    main()
