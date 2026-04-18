#!/usr/bin/env python3
"""Extract non-diary blockquotes from org files and write Hugo markdown.

Parses all org files in ~/My Drive/bibliographic-notes/, finds blockquotes
that are NOT already exported as diary quotes (i.e. their enclosing heading
lacks :EXPORT_FILE_NAME:), and generates markdown files in content/quotes/.

Usage:
    python extract-non-diary-quotes.py            # Full run
    python extract-non-diary-quotes.py --dry-run   # Preview without writing
    python extract-non-diary-quotes.py --full       # Compatibility no-op
"""

import argparse
import re
from pathlib import Path

from lib import (
    BIBLIO_NOTES_DIR,
    REPO_ROOT,
    atomic_write_text,
    cite_key_to_slug,
    escape_toml_string,
    extract_roam_refs,
    find_ancestor_with_export,
    is_dataless,
    make_non_diary_slug,
    parse_org_headings,
    safe_remove,
)

# === Constants ===

QUOTES_DIR = REPO_ROOT / "content" / "quotes"


def extract_blockquotes(content: str) -> list[tuple[str, str]]:
    """Extract all #+begin_quote...#+end_quote blocks from content.

    Returns a list of (quote_text, cite_locator) tuples.  The locator is
    parsed from a ``[cite:@Key, LOCATOR]`` line that immediately follows
    the blockquote (possibly separated by blank lines).  If no citation
    with a locator is found, cite_locator is "".
    """
    blocks = []
    for m in re.finditer(
        r"#\+begin_quote\s*\n(.*?)#\+end_quote",
        content,
        re.DOTALL | re.IGNORECASE,
    ):
        quote_text = m.group(1).strip()
        # Look for [cite:@Key, LOCATOR] after the end_quote
        after = content[m.end():]
        cite_match = re.match(
            r"\s*\[cite:@[^\],]+,\s*(.+?)\]", after
        )
        locator = cite_match.group(1).strip() if cite_match else ""
        blocks.append((quote_text, locator))
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
    text = re.sub(r"=([^=\n]+?)=", r"`\1`", text)
    text = re.sub(r"~([^~\n]+?)~", r"`\1`", text)
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
    return make_non_diary_slug(work_slug, source)


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

        # Skip headings not tagged :public: (opt-in extraction)
        if "public" not in h["tags"]:
            continue

        # Extract blockquotes from this heading's content
        blockquotes = extract_blockquotes(h["content"])
        if not blockquotes:
            continue

        heading_id = h["properties"].get("ID", "")
        noter_page = h["properties"].get("NOTER_PAGE", "")
        page_num = extract_page_number(noter_page)

        for bq_idx, (bq_text, cite_locator) in enumerate(blockquotes):
            # If multiple quotes under one heading, make slug unique per quote
            if len(blockquotes) > 1:
                id_source = f"{heading_id}-{bq_idx}" if heading_id else bq_text
            else:
                id_source = heading_id if heading_id else bq_text

            quote_md = org_to_markdown(bq_text)
            slug = make_slug(work_slug, id_source, bq_text)
            title = make_title(quote_md)
            # Prefer NOTER_PAGE, fall back to citation locator
            locator = f"p. {page_num}" if page_num else cite_locator

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
        f'work = "{escape_toml_string(quote["work_slug"])}"',
    ]
    if quote["locator"]:
        lines.append(f'locator = "{escape_toml_string(quote["locator"])}"')
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

    atomic_write_text(path, "\n".join(lines))
    return path


def main():
    parser = argparse.ArgumentParser(
        description="Extract non-diary blockquotes from org files"
    )
    parser.add_argument("--dry-run", action="store_true", help="Preview without writing")
    parser.add_argument("--full", action="store_true", help=argparse.SUPPRESS)
    args = parser.parse_args()

    if not BIBLIO_NOTES_DIR.exists():
        print(f"ERROR: {BIBLIO_NOTES_DIR} does not exist")
        return

    if not args.dry_run:
        QUOTES_DIR.mkdir(parents=True, exist_ok=True)

    org_files = sorted(BIBLIO_NOTES_DIR.glob("*.org"))
    stats = {"files_scanned": 0, "files_skipped_dataless": 0,
             "quotes_extracted": 0, "files_written": 0}

    # Collect all slugs we generate to track which files are ours
    generated_slugs = set()

    for org_path in org_files:
        stats["files_scanned"] += 1

        if is_dataless(org_path):
            stats["files_skipped_dataless"] += 1
            continue

        quotes = process_org_file(org_path)

        for q in quotes:
            stats["quotes_extracted"] += 1
            generated_slugs.add(q["slug"])

            if args.dry_run:
                if stats["quotes_extracted"] <= 10:
                    print(f"  [EXTRACT] {q['slug']}.md  ({q['work_slug']})")
            else:
                write_quote_markdown(q, QUOTES_DIR)
                stats["files_written"] += 1

        if stats["files_scanned"] % 500 == 0:
            print(f"  ... {stats['files_scanned']} files scanned")

    # Clean up stale non-diary quote files (generated by us but no longer produced)
    stats["stale_removed"] = 0
    if (
        not args.dry_run
        and QUOTES_DIR.exists()
        and stats["files_skipped_dataless"] == 0
    ):
        # Non-diary quotes use the slug format "{work-slug}-q-{hash}" (see
        # make_slug above).  The "-q-" infix identifies them as ours for cleanup.
        for md_file in sorted(QUOTES_DIR.glob("*-q-*.md")):
            slug = md_file.stem
            if slug not in generated_slugs:
                safe_remove(md_file)
                stats["stale_removed"] += 1
    elif stats["files_skipped_dataless"]:
        print("  Stale cleanup skipped because dataless source files were skipped.")

    print(f"  Files scanned:       {stats['files_scanned']}")
    print(f"  Skipped (dataless):  {stats['files_skipped_dataless']}")
    print(f"  Quotes extracted:    {stats['quotes_extracted']}")
    print(f"  Files written:       {stats['files_written']}")
    print(f"  Stale files removed: {stats['stale_removed']}")
    if args.dry_run:
        print("  *** DRY RUN — no files written ***")


if __name__ == "__main__":
    main()
