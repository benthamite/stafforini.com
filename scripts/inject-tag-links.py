#!/usr/bin/env python3
"""Inject org-roam tag links into quote subheadings in bibliographic notes.

For each quote that has WordPress tags, adds a "Topics:" line with org-roam
links after the citation line. This creates backlinks from quotes to topic
and person notes, enabling the backlinks-as-taxonomy system.

Usage:
    python inject-tag-links.py                  # Process all quotes
    python inject-tag-links.py --dry-run        # Preview without writing
    python inject-tag-links.py --limit 10       # Process first 10 cite keys
"""

import argparse
import json
import re
from pathlib import Path

SCRIPTS_DIR = Path(__file__).parent
MATCHED_JSON = SCRIPTS_DIR / "wp-quotes-matched.json"
TAGS_JSON = SCRIPTS_DIR / "wp-quotes-tags-linked.json"

BIB_NOTES_DIR = Path.home() / "Library/CloudStorage/Dropbox/bibliographic-notes"
NOTES_DIR = Path.home() / "Library/CloudStorage/Dropbox/notes"
PEOPLE_DIR = Path.home() / "Library/CloudStorage/Dropbox/people"
NOTES_TAGS_DIR = NOTES_DIR / "tags"
PEOPLE_TAGS_DIR = PEOPLE_DIR / "tags"


def tag_to_filename(tag: str) -> str:
    """Convert a tag to a kebab-case .org filename."""
    slug = tag.lower().rstrip(".")
    slug = re.sub(r"[^a-z0-9\u00e0-\u00ff-]+", "-", slug)
    slug = re.sub(r"-+", "-", slug).strip("-")
    return slug + ".org"


def read_org_id(path: Path) -> str | None:
    """Read the first :ID: property from an org file."""
    try:
        for line in path.open(encoding="utf-8"):
            if ":ID:" in line:
                m = re.search(r":ID:\s+(\S+)", line)
                if m:
                    return m.group(1)
    except (FileNotFoundError, PermissionError):
        pass
    return None


def build_tag_index(tags_data: dict) -> dict[str, tuple[Path, str]]:
    """Build a mapping from tag name to (file_path, org_roam_id).

    Covers both linked tags (existing org files) and unlinked tags (stubs).
    """
    index = {}
    missing = []

    # Linked tags: known file and directory
    for item in tags_data["linked"]:
        tag = item["tag"]
        filename = item["file"]
        directory = item["directory"]

        if directory == "notes":
            path = NOTES_DIR / filename
        else:
            path = PEOPLE_DIR / filename

        org_id = read_org_id(path)
        if org_id:
            index[tag] = (path, org_id)
        else:
            missing.append((tag, str(path)))

    # Unlinked tags: stubs in tags/ subdirectories
    for item in tags_data["unlinked"]:
        tag = item["tag"]
        filename = tag_to_filename(tag)

        # Try both directories
        path = None
        for d in [NOTES_TAGS_DIR, PEOPLE_TAGS_DIR]:
            candidate = d / filename
            if candidate.exists():
                path = candidate
                break

        if path:
            org_id = read_org_id(path)
            if org_id:
                index[tag] = (path, org_id)
            else:
                missing.append((tag, str(path)))
        else:
            missing.append((tag, "no file found"))

    return index, missing


def build_topics_line(tags: list[str], tag_index: dict) -> str | None:
    """Build a Topics: line with org-roam links for the given tags.

    Returns None if no tags have IDs.
    """
    links = []
    for tag in sorted(tags):
        if tag in tag_index:
            _, org_id = tag_index[tag]
            links.append(f"[[id:{org_id}][{tag}]]")

    if not links:
        return None
    return "Topics: " + " · ".join(links)


def inject_topics_into_file(
    file_path: Path,
    post_id_to_topics: dict[str, str],
    dry_run: bool = False,
) -> dict:
    """Inject Topics lines into a single org file.

    Returns stats dict with counts of injected/skipped/missing.
    """
    stats = {"injected": 0, "skipped_existing": 0, "skipped_no_cite": 0}

    content = file_path.read_text(encoding="utf-8")
    lines = content.split("\n")
    new_lines = []
    i = 0

    while i < len(lines):
        line = lines[i]
        new_lines.append(line)

        # Look for :WP_POST_ID: property
        m = re.match(r"^:WP_POST_ID:\s+(\S+)", line)
        if m:
            post_id = m.group(1)
            topics_line = post_id_to_topics.get(post_id)

            if topics_line:
                # Scan forward from current position to find the citation line
                # First, continue copying lines until we find [cite:@
                i += 1
                cite_found = False
                while i < len(lines):
                    current = lines[i]
                    new_lines.append(current)

                    if current.startswith("[cite:@"):
                        cite_found = True
                        # Check if Topics already exists after this cite line
                        # Look ahead past blank lines
                        j = i + 1
                        while j < len(lines) and lines[j].strip() == "":
                            j += 1
                        if j < len(lines) and lines[j].startswith("Topics:"):
                            stats["skipped_existing"] += 1
                        else:
                            # Inject Topics line after the citation
                            new_lines.append("")
                            new_lines.append(topics_line)
                            stats["injected"] += 1
                        break

                    # Stop scanning if we hit the next heading (no cite found)
                    if current.startswith("** ") or current.startswith("* "):
                        stats["skipped_no_cite"] += 1
                        break

                    i += 1

                if not cite_found and i >= len(lines):
                    # Reached end of file without finding cite
                    stats["skipped_no_cite"] += 1

        i += 1

    if stats["injected"] > 0 and not dry_run:
        file_path.write_text("\n".join(new_lines), encoding="utf-8")

    return stats


def main():
    parser = argparse.ArgumentParser(description="Inject tag links into quote org files")
    parser.add_argument("--dry-run", action="store_true", help="Preview without writing")
    parser.add_argument("--limit", type=int, help="Process only N cite keys")
    args = parser.parse_args()

    # Load data
    print("Loading data...")
    quotes = json.loads(MATCHED_JSON.read_text())
    tags_data = json.loads(TAGS_JSON.read_text())
    print(f"  {len(quotes)} quotes")
    print(f"  {tags_data['meta']['linked']} linked tags, {tags_data['meta']['unlinked']} unlinked tags")

    # Build tag -> ID index
    print("\nBuilding tag index...")
    tag_index, missing_tags = build_tag_index(tags_data)
    print(f"  {len(tag_index)} tags indexed with IDs")
    if missing_tags:
        print(f"  {len(missing_tags)} tags missing (no file or no ID)")

    # Group quotes by cite key; build post_id -> topics_line mapping per file
    print("\nPreparing injections...")
    files_to_process = {}  # cite_key -> {post_id: topics_line}

    quotes_with_tags = 0
    quotes_without_tags = 0
    quotes_no_cite_key = 0

    for quote in quotes:
        tags = quote.get("tags", [])
        if not tags:
            quotes_without_tags += 1
            continue

        # Get cite key
        bib_match = quote.get("bib_match", {})
        cite_key = ""
        if bib_match.get("status") == "matched" and bib_match.get("matches"):
            cite_key = bib_match["matches"][0]["cite_key"]
        if not cite_key:
            cite_key = quote.get("new_cite_key", "")
        if not cite_key:
            quotes_no_cite_key += 1
            continue

        topics_line = build_topics_line(tags, tag_index)
        if not topics_line:
            continue

        post_id = quote["post_id"]
        if cite_key not in files_to_process:
            files_to_process[cite_key] = {}
        files_to_process[cite_key][post_id] = topics_line
        quotes_with_tags += 1

    print(f"  {quotes_with_tags} quotes with tags to inject")
    print(f"  {quotes_without_tags} quotes without tags (skipped)")
    print(f"  {quotes_no_cite_key} quotes without cite key (skipped)")
    print(f"  {len(files_to_process)} org files to process")

    # Process files
    if args.limit:
        keys = sorted(files_to_process.keys())[:args.limit]
        files_to_process = {k: files_to_process[k] for k in keys}
        print(f"  Limited to {len(files_to_process)} cite keys")

    print("\nInjecting tag links...")
    total_stats = {"injected": 0, "skipped_existing": 0, "skipped_no_cite": 0,
                   "files_modified": 0, "files_not_found": 0}

    sorted_keys = sorted(files_to_process.keys())
    for idx, cite_key in enumerate(sorted_keys):
        post_id_to_topics = files_to_process[cite_key]
        org_path = BIB_NOTES_DIR / f"{cite_key}.org"

        if not org_path.exists():
            total_stats["files_not_found"] += 1
            continue

        stats = inject_topics_into_file(org_path, post_id_to_topics, dry_run=args.dry_run)
        total_stats["injected"] += stats["injected"]
        total_stats["skipped_existing"] += stats["skipped_existing"]
        total_stats["skipped_no_cite"] += stats["skipped_no_cite"]
        if stats["injected"] > 0:
            total_stats["files_modified"] += 1

        if (idx + 1) % 100 == 0:
            print(f"  ... {idx + 1}/{len(sorted_keys)} files processed")

    # Summary
    print(f"\n{'=' * 60}")
    print("RESULTS")
    print(f"{'=' * 60}")
    print(f"  Topics lines injected:  {total_stats['injected']:>5}")
    print(f"  Already present:        {total_stats['skipped_existing']:>5}")
    print(f"  No citation line:       {total_stats['skipped_no_cite']:>5}")
    print(f"  Files modified:         {total_stats['files_modified']:>5}")
    print(f"  Files not found:        {total_stats['files_not_found']:>5}")

    if args.dry_run:
        print("\n  *** DRY RUN — no files were modified ***")


if __name__ == "__main__":
    main()
