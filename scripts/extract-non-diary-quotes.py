#!/usr/bin/env python3
"""Extract non-diary blockquotes from org files and write Hugo markdown.

Parses all org files in ~/My Drive/bibliographic-notes/, finds blockquotes
that are NOT already exported as diary quotes (i.e. their enclosing heading
lacks :EXPORT_FILE_NAME:), and generates markdown files in content/quotes/.

Usage:
    python extract-non-diary-quotes.py            # Full run
    python extract-non-diary-quotes.py --dry-run   # Preview without writing
    python extract-non-diary-quotes.py --full       # Ignore manifest, re-extract all
"""

import argparse
import hashlib
import json
import os
import re
import tempfile
from pathlib import Path

from lib import MTIME_EPSILON, cite_key_to_slug, escape_toml_string, is_dataless

# === Constants ===

SCRIPTS_DIR = Path(__file__).parent
HUGO_ROOT = SCRIPTS_DIR.parent
QUOTES_DIR = HUGO_ROOT / "content" / "quotes"
MANIFEST_PATH = SCRIPTS_DIR / ".extract-quotes-manifest.json"
BIBLIO_NOTES_DIR = Path.home() / "My Drive" / "bibliographic-notes"


# === Org parsing helpers ===


def extract_roam_refs(text: str) -> str:
    """Extract the cite key from :ROAM_REFS: property in the top-level heading."""
    m = re.search(r":ROAM_REFS:\s+@(\S+)", text)
    return m.group(1) if m else ""


def parse_org_headings(text: str) -> list[dict]:
    """Parse org headings into a list of dicts with properties and content.

    Returns a flat list of headings. Each dict has:
    - level: heading level (number of *)
    - title: heading text
    - tags: set of tags from the heading line
    - properties: dict of :PROP: values from the property drawer
    - content: text between end of properties and next heading
    - parent_props: properties of the nearest ancestor heading (for nesting)
    """
    headings = []
    # Match heading lines: stars, optional priority, title, optional tags
    heading_re = re.compile(
        r"^(\*+)\s+(?:\[#\d\]\s+)?(.+?)(?:\s+(:[:\w]+:))?\s*$", re.MULTILINE
    )

    matches = list(heading_re.finditer(text))
    for i, m in enumerate(matches):
        level = len(m.group(1))
        title = m.group(2).strip()
        tag_str = m.group(3) or ""
        tags = set(tag_str.strip(":").split(":")) if tag_str else set()

        # Extract region between this heading and the next
        start = m.end()
        end = matches[i + 1].start() if i + 1 < len(matches) else len(text)
        region = text[start:end]

        # Parse property drawer
        props = {}
        prop_re = re.compile(r":(\w+):\s+(.*)")
        drawer_match = re.search(
            r":PROPERTIES:\s*\n(.*?):END:", region, re.DOTALL
        )
        if drawer_match:
            for pm in prop_re.finditer(drawer_match.group(1)):
                props[pm.group(1)] = pm.group(2).strip()
            content_start = drawer_match.end()
        else:
            content_start = 0

        content = region[content_start:]

        headings.append({
            "level": level,
            "title": title,
            "tags": tags,
            "properties": props,
            "content": content,
        })

    return headings


def find_ancestor_with_export(headings: list[dict], idx: int) -> bool:
    """Check if any ancestor heading of headings[idx] has EXPORT_FILE_NAME."""
    target_level = headings[idx]["level"]
    # Walk backwards to find ancestors (headings with lower level)
    for j in range(idx - 1, -1, -1):
        if headings[j]["level"] < target_level:
            if "EXPORT_FILE_NAME" in headings[j]["properties"]:
                return True
            # Continue checking higher ancestors
            target_level = headings[j]["level"]
    return False


def extract_blockquotes(content: str) -> list[str]:
    """Extract all #+begin_quote...#+end_quote blocks from content."""
    blocks = []
    for m in re.finditer(
        r"#\+begin_quote\s*\n(.*?)#\+end_quote",
        content,
        re.DOTALL | re.IGNORECASE,
    ):
        blocks.append(m.group(1).strip())
    return blocks


def org_to_markdown(text: str) -> str:
    """Convert basic org-mode markup to markdown.

    Handles: /italic/ -> _italic_, *bold* -> **bold**, =code= -> `code`,
    ~code~ -> `code`, [[link][desc]] -> desc, [[link]] -> link
    """
    # Links with description
    text = re.sub(r"\[\[(?:[^\]]*)\]\[([^\]]*)\]\]", r"\1", text)
    # Links without description
    text = re.sub(r"\[\[([^\]]*)\]\]", r"\1", text)
    # Bold: *text* -> **text** (but not heading stars at line start)
    text = re.sub(r"(?<=\s)\*([^\s*][^*]*?[^\s*])\*(?=[\s.,;:!?)]|$)", r"**\1**", text, flags=re.MULTILINE)
    # Italic: /text/ -> _text_ (avoid matching URL paths by requiring word boundaries)
    text = re.sub(r"(?<!\w)/([^/\n]{2,}?)/(?!\w)", r"_\1_", text)
    # Code: =text= or ~text~
    text = re.sub(r"[=~]([^=~\n]+?)[=~]", r"`\1`", text)
    return text


def extract_page_number(noter_page: str) -> str:
    """Extract page number from NOTER_PAGE property.

    Formats:
    - "3" -> "3"
    - "(2 . 0.129)" -> "2"
    - "" -> ""
    """
    if not noter_page:
        return ""
    # Tuple format: (N . ...)
    m = re.match(r"\((\d+)", noter_page)
    if m:
        return m.group(1)
    # Plain number
    m = re.match(r"(\d+)", noter_page)
    if m:
        return m.group(1)
    return ""


def make_title(quote_text: str, max_len: int = 60) -> str:
    """Create a title from the first ~60 chars of the quote text."""
    # Strip markdown formatting for title
    clean = re.sub(r"[_*`]", "", quote_text)
    # Collapse whitespace
    clean = re.sub(r"\s+", " ", clean).strip()
    if len(clean) <= max_len:
        return clean
    # Truncate at word boundary
    truncated = clean[:max_len]
    last_space = truncated.rfind(" ")
    if last_space > max_len // 2:
        truncated = truncated[:last_space]
    return truncated + "..."


def make_slug(work_slug: str, heading_id: str, quote_text: str) -> str:
    """Generate a stable slug for a non-diary quote.

    Format: {work-slug}-q-{8-char-hash}
    Hash is derived from the heading :ID: for stability, falling back to
    content hash if no ID is available.
    """
    source = heading_id if heading_id else quote_text
    h = hashlib.sha256(source.encode()).hexdigest()[:8]
    return f"{work_slug}-q-{h}"


# === Main extraction logic ===


def process_org_file(org_path: Path) -> list[dict]:
    """Process a single org file and return extracted non-diary quotes.

    Each returned dict has: slug, title, work_slug, locator, quote_md
    """
    text = org_path.read_text(errors="replace")

    cite_key = extract_roam_refs(text)
    if not cite_key:
        return []

    work_slug = cite_key_to_slug(cite_key)
    headings = parse_org_headings(text)
    quotes = []

    for i, h in enumerate(headings):
        # Skip top-level heading (the file's main heading)
        if h["level"] == 1:
            continue

        # Skip headings that are diary quotes (have EXPORT_FILE_NAME)
        if "EXPORT_FILE_NAME" in h["properties"]:
            continue

        # Skip if an ancestor has EXPORT_FILE_NAME (quote is nested under a diary entry)
        if find_ancestor_with_export(headings, i):
            continue

        # Extract blockquotes from this heading's content
        blockquotes = extract_blockquotes(h["content"])
        if not blockquotes:
            continue

        heading_id = h["properties"].get("ID", "")
        noter_page = h["properties"].get("NOTER_PAGE", "")
        page_num = extract_page_number(noter_page)

        for bq_idx, bq_text in enumerate(blockquotes):
            # If multiple quotes under one heading, make slug unique per quote
            if len(blockquotes) > 1:
                id_source = f"{heading_id}-{bq_idx}" if heading_id else bq_text
            else:
                id_source = heading_id if heading_id else bq_text

            quote_md = org_to_markdown(bq_text)
            slug = make_slug(work_slug, id_source, bq_text)
            title = make_title(quote_md)
            locator = f"p. {page_num}" if page_num else ""

            quotes.append({
                "slug": slug,
                "title": title,
                "work_slug": work_slug,
                "locator": locator,
                "quote_md": quote_md,
            })

    return quotes


def write_quote_markdown(quote: dict, output_dir: Path) -> Path:
    """Write a single non-diary quote to a markdown file."""
    path = output_dir / f"{quote['slug']}.md"

    lines = [
        "+++",
        f'title = "{escape_toml_string(quote["title"])}"',
        'author = ["Pablo Stafforini"]',
        "diary = false",
        "draft = false",
        f'work = "{quote["work_slug"]}"',
    ]
    if quote["locator"]:
        lines.append(f'locator = "{quote["locator"]}"')
    lines.append("+++")

    # Format blockquote: prefix each line with >
    bq_lines = []
    for line in quote["quote_md"].split("\n"):
        if line.strip():
            bq_lines.append(f"> {line}")
        else:
            bq_lines.append(">")
    lines.append("\n".join(bq_lines))
    lines.append("")

    path.write_text("\n".join(lines))
    return path


def load_manifest() -> dict:
    """Load the extraction manifest (org file -> mtime mapping)."""
    if MANIFEST_PATH.exists():
        return json.loads(MANIFEST_PATH.read_text())
    return {}


def save_manifest(manifest: dict) -> None:
    """Save the extraction manifest atomically."""
    from lib import atomic_write_json
    atomic_write_json(MANIFEST_PATH, manifest)


def main():
    parser = argparse.ArgumentParser(
        description="Extract non-diary blockquotes from org files"
    )
    parser.add_argument("--dry-run", action="store_true", help="Preview without writing")
    parser.add_argument("--full", action="store_true", help="Ignore manifest, re-extract all")
    args = parser.parse_args()

    if not BIBLIO_NOTES_DIR.exists():
        print(f"ERROR: {BIBLIO_NOTES_DIR} does not exist")
        return

    if not args.dry_run:
        QUOTES_DIR.mkdir(parents=True, exist_ok=True)

    manifest = {} if args.full else load_manifest()
    new_manifest = {}

    org_files = sorted(BIBLIO_NOTES_DIR.glob("*.org"))
    stats = {"files_scanned": 0, "files_skipped_dataless": 0, "files_skipped_cached": 0,
             "quotes_extracted": 0, "files_written": 0}

    # Collect all slugs we generate to track which files are ours
    generated_slugs = set()
    # Also collect slugs from cached files (not re-processed this run)
    cached_slugs: dict[str, list[str]] = {}

    for org_path in org_files:
        stats["files_scanned"] += 1

        if is_dataless(org_path):
            stats["files_skipped_dataless"] += 1
            continue

        org_key = org_path.name
        mtime = org_path.stat().st_mtime

        # Check manifest for incremental skip
        if org_key in manifest and abs(manifest[org_key].get("mtime", 0) - mtime) <= MTIME_EPSILON:
            stats["files_skipped_cached"] += 1
            new_manifest[org_key] = manifest[org_key]
            # Remember slugs from cached entries
            for s in manifest[org_key].get("slugs", []):
                generated_slugs.add(s)
            continue

        quotes = process_org_file(org_path)
        slugs_for_file = []

        for q in quotes:
            stats["quotes_extracted"] += 1
            generated_slugs.add(q["slug"])
            slugs_for_file.append(q["slug"])

            if args.dry_run:
                if stats["quotes_extracted"] <= 10:
                    print(f"  [EXTRACT] {q['slug']}.md  ({q['work_slug']})")
            else:
                write_quote_markdown(q, QUOTES_DIR)
                stats["files_written"] += 1

        new_manifest[org_key] = {"mtime": mtime, "slugs": slugs_for_file}

        if stats["files_scanned"] % 500 == 0:
            print(f"  ... {stats['files_scanned']} files scanned")

    # Clean up stale non-diary quote files (generated by us but no longer produced)
    stats["stale_removed"] = 0
    if not args.dry_run and QUOTES_DIR.exists():
        # Non-diary quotes use the slug format "{work-slug}-q-{hash}" (see
        # make_slug above).  The "-q-" infix identifies them as ours for cleanup.
        for md_file in sorted(QUOTES_DIR.glob("*-q-*.md")):
            slug = md_file.stem
            if slug not in generated_slugs:
                md_file.unlink()
                stats["stale_removed"] += 1

    if not args.dry_run:
        save_manifest(new_manifest)

    print(f"  Files scanned:       {stats['files_scanned']}")
    print(f"  Skipped (dataless):  {stats['files_skipped_dataless']}")
    print(f"  Skipped (cached):    {stats['files_skipped_cached']}")
    print(f"  Quotes extracted:    {stats['quotes_extracted']}")
    print(f"  Files written:       {stats['files_written']}")
    print(f"  Stale files removed: {stats['stale_removed']}")
    if args.dry_run:
        print("  *** DRY RUN — no files written ***")


if __name__ == "__main__":
    main()
