#!/usr/bin/env python3
"""Inject tags into Hugo markdown front matter from org :TOPICS: properties.

Data flow:
  - Quotes: :TOPICS: in bibliographic-notes org files → tags in content/quotes/*.md
  - Notes:  :TOPICS: in notes org files               → tags in content/notes/*.md
  - Works:  data/work-tags.json (optional)            → tags in content/works/*.md

Usage:
    python inject-tags.py                # Full run
    python inject-tags.py --dry-run      # Preview without writing
    python inject-tags.py --file PATH    # Process a single markdown file
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

from lib import (
    BIBLIO_NOTES_DIR,
    ID_LINK_RE,
    NOTES_DIR,
    REPO_ROOT,
    atomic_write_text,
    cite_key_to_slug,
    escape_toml_string,
    escape_yaml_string,
    extract_roam_refs,
    find_ancestor_with_export,
    find_org_files,
    is_dataless,
    make_non_diary_slug,
    parse_org_headings,
)

# === Constants ===

CONTENT_QUOTES = REPO_ROOT / "content" / "quotes"
CONTENT_NOTES = REPO_ROOT / "content" / "notes"
CONTENT_WORKS = REPO_ROOT / "content" / "works"
WORK_TAGS_PATH = REPO_ROOT / "data" / "work-tags.json"

TOML_FM_RE = re.compile(r"(\+\+\+\n)(.*?)(\n\+\+\+)", re.DOTALL)
YAML_FM_RE = re.compile(r"(---\n)(.*?)(\n---)", re.DOTALL)
TOML_TAGS_RE = re.compile(r"^tags = \[.*\]$", re.MULTILINE)
YAML_TAGS_RE = re.compile(r"^tags:.*$", re.MULTILINE)


# === Tag extraction from org files ===


def extract_tag_titles(text: str) -> list[str]:
    """Extract tag titles from a :TOPICS: property value.

    Handles two formats:
    - Legacy ID links: [[id:UUID][name]] (middot-separated)
    - Plain text: name1 · name2 (middot-separated)
    """
    # First try to extract from ID links
    titles = [name for _uuid, name in ID_LINK_RE.findall(text) if name]
    if titles:
        return sorted(set(titles))
    # Fall back to middot-separated plain text
    parts = [t.strip() for t in text.split("·")]
    return sorted(set(p for p in parts if p))


def build_work_author_names() -> dict[str, set[str]]:
    """Build {work_slug: {author_name, ...}} from work page front matter.

    Used to filter out author-as-tag from quote topics: a person tag should
    indicate a quote is *about* that person, not merely *by* them.
    """
    result: dict[str, set[str]] = {}
    if not CONTENT_WORKS.exists():
        return result
    for md_file in CONTENT_WORKS.glob("*.md"):
        if md_file.name == "_index.md":
            continue
        text = md_file.read_text()
        m = re.search(r'^author:\s*"([^"]+)"', text, re.MULTILINE)
        if not m:
            m = re.search(r'^author\s*=\s*"([^"]+)"', text, re.MULTILINE)
        if not m:
            continue
        author_str = m.group(1)
        # Normalize Oxford comma, then split on " and " and ", "
        normalized = re.sub(r",\s+and\s+", ", ", author_str)
        names: set[str] = set()
        for part in re.split(r"\s+and\s+|,\s+", normalized):
            part = part.strip()
            if part:
                names.add(part)
        result[md_file.stem] = names
    return result


def build_quote_tags(
    work_authors: dict[str, set[str]] | None = None,
) -> dict[str, list[str]]:
    """Build {quote_slug: [tag_titles]} from :TOPICS: in bibliographic-notes.

    If WORK_AUTHORS is provided, any tag that matches a work's author
    name is excluded -- person tags should mark what a quote is about,
    not who wrote it.
    """
    result: dict[str, list[str]] = {}
    org_files = sorted(BIBLIO_NOTES_DIR.glob("*.org"))

    for org_path in org_files:
        if is_dataless(org_path):
            continue
        text = org_path.read_text(errors="replace")
        cite_key = extract_roam_refs(text)
        if not cite_key:
            continue

        work_slug = cite_key_to_slug(cite_key)
        headings = parse_org_headings(text)
        author_names = work_authors.get(work_slug, set()) if work_authors else set()

        for i, h in enumerate(headings):
            if h["level"] == 1:
                continue
            topics_str = h["properties"].get("TOPICS", "")
            if not topics_str:
                continue
            titles = extract_tag_titles(topics_str)
            if author_names:
                titles = [t for t in titles if t not in author_names]
            if not titles:
                continue

            export_name = h["properties"].get("EXPORT_FILE_NAME", "")
            if export_name:
                quote_slug = export_name
            else:
                heading_id = h["properties"].get("ID", "")
                if not heading_id:
                    continue
                if find_ancestor_with_export(headings, i):
                    continue
                quote_slug = make_non_diary_slug(work_slug, heading_id)

            result[quote_slug] = titles

    return result


def build_note_tags() -> dict[str, list[str]]:
    """Build {note_slug: [tag_titles]} from :TOPICS: in notes."""
    result: dict[str, list[str]] = {}

    for org_path in find_org_files(NOTES_DIR):
        if is_dataless(org_path):
            continue
        text = org_path.read_text(errors="replace")
        if "[[id:" not in text or ":TOPICS:" not in text:
            continue
        export_match = re.search(r":EXPORT_FILE_NAME:\s+(\S+)", text)
        if not export_match:
            continue
        note_slug = export_match.group(1)
        topics_match = re.search(r":TOPICS:\s+(.+)", text)
        if not topics_match:
            continue
        titles = extract_tag_titles(topics_match.group(1))
        if titles:
            result[note_slug] = titles

    return result


def load_work_tags() -> dict[str, list[str]]:
    """Load work tags from data/work-tags.json (empty dict if missing)."""
    if not WORK_TAGS_PATH.exists():
        return {}
    with open(WORK_TAGS_PATH) as f:
        return json.load(f)


# === Front matter injection ===


def format_toml_tags(tags: list[str]) -> str:
    """Format tags as a TOML array, e.g. tags = ["foo", "bar"]."""
    items = ", ".join(f'"{escape_toml_string(t)}"' for t in tags)
    return f"tags = [{items}]"


def format_yaml_tags(tags: list[str]) -> str:
    """Format tags as a YAML inline array, e.g. tags: ["foo", "bar"]."""
    items = ", ".join(f'"{escape_yaml_string(t)}"' for t in tags)
    return f"tags: [{items}]"


def inject_toml(text: str, tags: list[str]) -> str | None:
    """Inject or update tags in TOML front matter. Return new text or None if unchanged."""
    match = TOML_FM_RE.match(text)
    if not match:
        return None
    fm = match.group(2)
    rest = text[match.end():]
    tags_line = format_toml_tags(tags)

    existing = TOML_TAGS_RE.search(fm)
    if existing:
        if existing.group(0) == tags_line:
            return None
        fm = TOML_TAGS_RE.sub(tags_line, fm)
    else:
        fm = fm.rstrip("\n") + "\n" + tags_line

    return "+++\n" + fm.rstrip("\n") + "\n+++" + rest


def inject_yaml(text: str, tags: list[str]) -> str | None:
    """Inject or update tags in YAML front matter. Return new text or None if unchanged."""
    match = YAML_FM_RE.match(text)
    if not match:
        return None
    fm = match.group(2)
    rest = text[match.end():]
    tags_line = format_yaml_tags(tags)

    existing = YAML_TAGS_RE.search(fm)
    if existing:
        if existing.group(0) == tags_line:
            return None
        fm = YAML_TAGS_RE.sub(tags_line, fm)
    else:
        fm = fm.rstrip("\n") + "\n" + tags_line

    return "---\n" + fm.rstrip("\n") + "\n---" + rest


def detect_format(text: str) -> str:
    """Detect front matter format: 'toml', 'yaml', or 'unknown'."""
    if text.startswith("+++\n"):
        return "toml"
    if text.startswith("---\n"):
        return "yaml"
    return "unknown"


def process_directory(
    content_dir: Path, tag_map: dict[str, list[str]], dry_run: bool
) -> dict[str, int]:
    """Process all .md files in a directory, injecting tags. Return stats."""
    stats = {"updated": 0, "unchanged": 0, "removed": 0}

    if not content_dir.exists():
        return stats

    for md_file in sorted(content_dir.glob("*.md")):
        if md_file.name == "_index.md":
            continue
        slug = md_file.stem
        tags = tag_map.get(slug)

        text = md_file.read_text()
        fmt = detect_format(text)

        if tags:
            injector = inject_toml if fmt == "toml" else inject_yaml if fmt == "yaml" else None
            if not injector:
                continue
            new_text = injector(text, tags)
            if new_text is None:
                stats["unchanged"] += 1
                continue
            if dry_run:
                print(f"  [TAG] {md_file.name} -> {tags}")
            else:
                atomic_write_text(md_file, new_text)
            stats["updated"] += 1
        else:
            changed = remove_tags(md_file, text, fmt, dry_run)
            if changed:
                stats["removed"] += 1
            else:
                stats["unchanged"] += 1

    return stats


def remove_tags(md_file: Path, text: str, fmt: str, dry_run: bool) -> bool:
    """Remove stale tags line from a file that no longer has tags. Return True if changed."""
    if fmt == "toml":
        match = TOML_FM_RE.match(text)
        tag_re = TOML_TAGS_RE
    elif fmt == "yaml":
        match = YAML_FM_RE.match(text)
        tag_re = YAML_TAGS_RE
    else:
        return False
    if not match:
        return False
    fm = match.group(2)
    if not tag_re.search(fm):
        return False
    new_fm = tag_re.sub("", fm)
    new_fm = re.sub(r"\n{2,}", "\n", new_fm)
    delim = "+++" if fmt == "toml" else "---"
    rest = text[match.end():]
    new_text = f"{delim}\n" + new_fm.rstrip("\n") + f"\n{delim}" + rest
    if not dry_run:
        atomic_write_text(md_file, new_text)
    else:
        print(f"  [REMOVE] {md_file.name} -> tags removed")
    return True


ALL_TAGS_PATH = REPO_ROOT / "data" / "all-tags.json"


def write_all_tags(
    quote_tags: dict[str, list[str]],
    note_tags: dict[str, list[str]],
    work_tags: dict[str, list[str]],
) -> None:
    """Write the sorted union of all tags to data/all-tags.json."""
    all_tags: set[str] = set()
    for tag_map in (quote_tags, note_tags, work_tags):
        for tags in tag_map.values():
            all_tags.update(tags)
    sorted_tags = sorted(all_tags, key=str.casefold)
    ALL_TAGS_PATH.parent.mkdir(parents=True, exist_ok=True)
    atomic_write_text(ALL_TAGS_PATH, json.dumps(sorted_tags, ensure_ascii=False, indent=2) + "\n")
    print(f"  Wrote {len(sorted_tags)} tags to {ALL_TAGS_PATH.relative_to(REPO_ROOT)}")


# === Main ===


def main():
    parser = argparse.ArgumentParser(
        description="Inject tags into Hugo markdown front matter"
    )
    parser.add_argument("--dry-run", action="store_true", help="Preview without writing")
    parser.add_argument(
        "--file", metavar="MD_PATH",
        help="Process a single markdown file (fast mode for interactive exports)",
    )
    args = parser.parse_args()

    print("=" * 60)
    print("Injecting tags into Hugo front matter")
    print("=" * 60)

    if args.file:
        process_single_file(Path(args.file))
        return

    print("\n  Building work author map...")
    work_authors = build_work_author_names()
    print(f"  Found authors for {len(work_authors)} works")

    print("  Building quote tags from :TOPICS:...")
    quote_tags = build_quote_tags(work_authors)
    print(f"  Found tags for {len(quote_tags)} quotes")

    print("  Building note tags from :TOPICS:...")
    note_tags = build_note_tags()
    print(f"  Found tags for {len(note_tags)} notes")

    print("  Loading work tags...")
    work_tags = load_work_tags()
    print(f"  Found tags for {len(work_tags)} works")

    print("\n  Processing quotes...")
    q_stats = process_directory(CONTENT_QUOTES, quote_tags, args.dry_run)
    print(f"    Updated: {q_stats['updated']}  Unchanged: {q_stats['unchanged']}  Removed: {q_stats['removed']}")

    print("  Processing notes...")
    n_stats = process_directory(CONTENT_NOTES, note_tags, args.dry_run)
    print(f"    Updated: {n_stats['updated']}  Unchanged: {n_stats['unchanged']}  Removed: {n_stats['removed']}")

    print("  Processing works...")
    w_stats = process_directory(CONTENT_WORKS, work_tags, args.dry_run)
    print(f"    Updated: {w_stats['updated']}  Unchanged: {w_stats['unchanged']}  Removed: {w_stats['removed']}")

    total_updated = q_stats["updated"] + n_stats["updated"] + w_stats["updated"]
    print(f"\n  Total updated: {total_updated}")

    if not args.dry_run:
        write_all_tags(quote_tags, note_tags, work_tags)
    else:
        print("  *** DRY RUN — no files written ***")
    print("\nDone.")


def process_single_file(md_path: Path):
    """Process a single markdown file (used by --file mode)."""
    if not md_path.exists():
        print(f"Error: {md_path} not found")
        return

    slug = md_path.stem
    section = md_path.parent.name

    tags: list[str] = []
    if section == "quotes":
        work_authors = build_work_author_names()
        tags = build_quote_tags(work_authors).get(slug, [])
    elif section == "notes":
        tags = build_note_tags().get(slug, [])
    elif section == "works":
        tags = load_work_tags().get(slug, [])

    if not tags:
        print(f"  No tags found for {slug}")
        return

    text = md_path.read_text()
    fmt = detect_format(text)
    injector = inject_toml if fmt == "toml" else inject_yaml if fmt == "yaml" else None
    if not injector:
        print(f"  Unknown front matter format in {md_path.name}")
        return

    new_text = injector(text, tags)
    if new_text is None:
        print(f"  Tags already up to date for {slug}")
        return

    atomic_write_text(md_path, new_text)
    print(f"  [TAG] {md_path.name} -> {tags}")


if __name__ == "__main__":
    main()
