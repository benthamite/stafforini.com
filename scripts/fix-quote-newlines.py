#!/usr/bin/env python3
"""Restore lost line breaks in quote org files.

During the WordPress migration, re.sub(r"\s+", " ", text) collapsed all
whitespace in blockquotes to single spaces. This script extracts the original
blockquote HTML from WP XML (with newlines preserved), cleans it up, and
replaces the quote blocks in the org files.

For multiline quotes:
- Verse-like content (short lines, ≥80% under 100 chars): uses #+begin_verse
- Prose with paragraphs: keeps #+begin_quote but restores blank lines

Usage:
    python fix-quote-newlines.py                  # Process all quotes
    python fix-quote-newlines.py --dry-run        # Preview changes
    python fix-quote-newlines.py --limit 10       # Process first 10
"""

import argparse
import html
import json
import re
from pathlib import Path

from lxml import etree

from lib import escape_org_text, markdown_to_org_emphasis


# === Constants ===

SCRIPTS_DIR = Path(__file__).parent
MATCHED_JSON = SCRIPTS_DIR / "wp-quotes-matched.json"
WP_XML = Path.home() / "Downloads/notatudignum.WordPress.2026-02-15.xml"
NOTES_DIR = Path.home() / "My Drive/bibliographic-notes"


# === WP XML parsing ===


def parse_wp_xml(xml_path: Path) -> dict[str, str]:
    """Extract blockquote content from WP XML, preserving newlines.

    Returns: dict mapping post_id → blockquote_html
    """
    with open(xml_path, 'rb') as f:
        tree = etree.parse(f, etree.XMLParser(recover=True))
    ns = {
        'wp': 'http://wordpress.org/export/1.2/',
        'content': 'http://purl.org/rss/1.0/modules/content/',
    }

    blockquotes = {}
    items = tree.findall('.//item')

    for item in items:
        post_id_el = item.find('wp:post_id', ns)
        content_el = item.find('content:encoded', ns)

        if post_id_el is None or content_el is None:
            continue

        post_id = post_id_el.text
        content_html = content_el.text or ""

        # Extract blockquote content
        match = re.search(r'<blockquote>(.*?)</blockquote>', content_html, re.DOTALL)
        if match:
            blockquotes[post_id] = match.group(1)

    return blockquotes


# === HTML cleaning ===


def clean_blockquote_html(html_text: str) -> str:
    """Convert blockquote HTML to plain text with newlines preserved.

    Handles:
    - <p>...</p> → paragraph text + blank line after
    - <br />, <br> → newline
    - <em>, <i> → /text/ (org italic)
    - <strong>, <b> → *text* (org bold)
    - <a href="...">text</a> → just text
    - HTML entities (&amp;, &lt;, etc.)
    """
    text = html_text

    # Convert emphasis tags to org-mode markers
    # Do this before stripping tags to preserve emphasis
    text = re.sub(r'<(?:em|i)>(.*?)</(?:em|i)>', r'/\1/', text)
    text = re.sub(r'<(?:strong|b)>(.*?)</(?:strong|b)>', r'*\1*', text)

    # Convert links to just their text
    text = re.sub(r'<a[^>]*>(.*?)</a>', r'\1', text)

    # Convert <br> tags to newlines
    text = re.sub(r'<br\s*/?>', '\n', text)

    # Convert <p> tags to paragraphs with blank lines
    # First, close any unclosed <p> tags before </p>
    text = re.sub(r'<p>(.*?)</p>', r'\1\n\n', text, flags=re.DOTALL)

    # Strip any remaining HTML tags
    text = re.sub(r'<[^>]+>', '', text)

    # Decode HTML entities
    text = html.unescape(text)

    # Clean up excessive blank lines (more than 2 consecutive)
    text = re.sub(r'\n{3,}', '\n\n', text)

    # Strip leading/trailing whitespace
    text = text.strip()

    return text


# === Verse classification ===


# Verse detection thresholds — tuned to distinguish poetry/verse from prose.
VERSE_MIN_LINES = 3        # need at least this many non-empty lines
VERSE_MAX_LINE_LEN = 100   # lines shorter than this count as "short"
VERSE_SHORT_RATIO = 0.8    # fraction of lines that must be short
VERSE_MAX_AVG_LEN = 80     # average line length must be under this


def is_verse(text: str) -> bool:
    """Determine if text should be formatted as verse.

    Verse criteria:
    - ≥VERSE_MIN_LINES non-empty lines
    - ≥VERSE_SHORT_RATIO of non-empty lines under VERSE_MAX_LINE_LEN chars
    - Average line length < VERSE_MAX_AVG_LEN chars
    """
    lines = text.split('\n')
    non_empty_lines = [l for l in lines if l.strip()]

    if len(non_empty_lines) < VERSE_MIN_LINES:
        return False

    short_count = sum(1 for l in non_empty_lines if len(l) < VERSE_MAX_LINE_LEN)
    avg_len = sum(len(l) for l in non_empty_lines) / len(non_empty_lines)

    return (short_count / len(non_empty_lines) >= VERSE_SHORT_RATIO) and (avg_len < VERSE_MAX_AVG_LEN)


# === WP_POST_ID index ===


def build_wp_post_id_index(notes_dir: Path) -> dict[str, list[tuple[Path, int]]]:
    """Build index of WP_POST_ID → [(file_path, line_number), ...].

    A single file may have multiple WP_POST_IDs (multiple quote subheadings).
    """
    index = {}

    for org_file in notes_dir.glob("*.org"):
        try:
            content = org_file.read_text()
        except Exception:
            continue  # Skip dehydrated / unreadable files
        lines = content.split('\n')

        for i, line in enumerate(lines):
            match = re.match(r':WP_POST_ID:\s*(\d+)', line)
            if match:
                post_id = match.group(1)
                index.setdefault(post_id, []).append((org_file, i))

    return index


# === Org file modification ===


def find_quote_block(lines: list[str], wp_post_id_line: int) -> tuple[int, int, str] | None:
    """Find the quote block after a WP_POST_ID property.

    Returns: (begin_line, end_line, block_type) where block_type is 'quote' or 'verse'
             or None if not found.

    The block starts at the #+begin_quote or #+begin_verse line and ends at #+end_quote
    or #+end_verse.
    """
    # Find the :END: line that closes the properties drawer
    end_line = None
    for i in range(wp_post_id_line + 1, len(lines)):
        if lines[i].strip() == ':END:':
            end_line = i
            break

    if end_line is None:
        return None

    # Find the next #+begin_quote or #+begin_verse line
    for i in range(end_line + 1, len(lines)):
        line = lines[i].strip()
        if line == '#+begin_quote':
            # Find the matching #+end_quote
            for j in range(i + 1, len(lines)):
                if lines[j].strip() == '#+end_quote':
                    return (i, j, 'quote')
            return None
        elif line == '#+begin_verse':
            # Find the matching #+end_verse
            for j in range(i + 1, len(lines)):
                if lines[j].strip() == '#+end_verse':
                    return (i, j, 'verse')
            return None
        elif line.startswith('**') or line.startswith('*'):
            # Hit another heading, stop searching
            return None

    return None


def replace_quote_block(
    org_file: Path,
    wp_post_id: str,
    wp_post_id_line: int,
    new_text: str,
    dry_run: bool = False,
) -> bool:
    """Replace the quote block content in an org file.

    Returns: True if successful, False otherwise.
    """
    content = org_file.read_text()
    lines = content.split('\n')

    block_info = find_quote_block(lines, wp_post_id_line)
    if block_info is None:
        print(f"  WARNING: Could not find quote block for WP_POST_ID {wp_post_id} in {org_file.name}")
        return False

    begin_line, end_line, old_block_type = block_info

    # Check if this is multiline
    old_content = '\n'.join(lines[begin_line + 1:end_line])
    is_multiline = '\n' in new_text.strip()

    # Determine new block type
    if is_multiline and is_verse(new_text):
        new_block_type = 'verse'
    else:
        new_block_type = 'quote'

    # Build new block
    processed_text = escape_org_text(markdown_to_org_emphasis(new_text))

    new_lines = []
    new_lines.append(f'#+begin_{new_block_type}')
    new_lines.append(processed_text)
    new_lines.append(f'#+end_{new_block_type}')

    # Replace the block
    new_content = (
        '\n'.join(lines[:begin_line])
        + '\n'
        + '\n'.join(new_lines)
        + '\n'
        + '\n'.join(lines[end_line + 1:])
    )

    if dry_run:
        print(f"  [DRY RUN] Would update {org_file.name} WP_POST_ID={wp_post_id}")
        print(f"    Old block type: {old_block_type}, New block type: {new_block_type}")
        if is_multiline:
            line_count = len(new_text.strip().split('\n'))
            print(f"    Restoring {line_count} lines")
        return True
    else:
        # Atomic write using temp file
        import tempfile
        import os

        tmp_fd, tmp_path = tempfile.mkstemp(dir=org_file.parent, suffix=".tmp")
        try:
            with os.fdopen(tmp_fd, "w") as f:
                f.write(new_content)
            Path(tmp_path).replace(org_file)
        except BaseException:
            Path(tmp_path).unlink(missing_ok=True)
            raise

        return True


# === Main processing ===


def main():
    parser = argparse.ArgumentParser(
        description="Restore lost line breaks in quote org files"
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Preview changes without modifying files",
    )
    parser.add_argument(
        "--limit",
        type=int,
        help="Process only first N quotes",
    )
    args = parser.parse_args()

    # Check required files
    if not WP_XML.exists():
        print(f"ERROR: WP XML not found at {WP_XML}")
        return 1

    if not MATCHED_JSON.exists():
        print(f"ERROR: Matched JSON not found at {MATCHED_JSON}")
        return 1

    print("Loading quotes from matched JSON...")
    quotes = json.loads(MATCHED_JSON.read_text())
    print(f"  {len(quotes)} quotes loaded")

    if args.limit:
        quotes = quotes[:args.limit]
        print(f"  Limited to {len(quotes)} quotes")

    print(f"\nParsing WP XML from {WP_XML.name}...")
    blockquotes = parse_wp_xml(WP_XML)
    print(f"  {len(blockquotes)} blockquotes extracted")

    print(f"\nBuilding WP_POST_ID index from {NOTES_DIR}...")
    wp_index = build_wp_post_id_index(NOTES_DIR)
    print(f"  {len(wp_index)} WP_POST_IDs indexed")

    # Process quotes
    stats = {
        "processed": 0,
        "multiline_restored": 0,
        "verse_blocks": 0,
        "quote_blocks": 0,
        "single_line": 0,
        "no_blockquote": 0,
        "no_org_file": 0,
        "errors": 0,
    }

    print("\nProcessing quotes...")
    for quote in quotes:
        post_id = quote["post_id"]

        # Get blockquote HTML from WP XML
        if post_id not in blockquotes:
            stats["no_blockquote"] += 1
            continue

        # Clean HTML to plain text
        cleaned_text = clean_blockquote_html(blockquotes[post_id])

        # Only process multiline blockquotes — single-line quotes had no
        # newlines to lose, and re-extracting from XML could introduce
        # subtle differences in emphasis/entity handling.
        if '\n' not in cleaned_text.strip():
            stats["single_line"] += 1
            continue

        # Find org file location
        if post_id not in wp_index:
            stats["no_org_file"] += 1
            continue

        # A post_id may appear in multiple files (shouldn't happen, but handle it)
        for org_file, wp_line in wp_index[post_id]:
            success = replace_quote_block(
                org_file=org_file,
                wp_post_id=post_id,
                wp_post_id_line=wp_line,
                new_text=cleaned_text,
                dry_run=args.dry_run,
            )

            if success:
                stats["processed"] += 1
                stats["multiline_restored"] += 1
                if is_verse(cleaned_text):
                    stats["verse_blocks"] += 1
                else:
                    stats["quote_blocks"] += 1
            else:
                stats["errors"] += 1

    # Print summary
    print(f"\n{'='*60}")
    print("RESULTS")
    print(f"{'='*60}")
    print(f"  Processed:              {stats['processed']:>5}")
    print(f"  Multiline restored:     {stats['multiline_restored']:>5}")
    print(f"    → Verse blocks:       {stats['verse_blocks']:>5}")
    print(f"    → Quote blocks:       {stats['quote_blocks']:>5}")
    print(f"  Single-line (no change): {stats['single_line']:>5}")
    print(f"  No blockquote in XML:   {stats['no_blockquote']:>5}")
    print(f"  No org file found:      {stats['no_org_file']:>5}")
    print(f"  Errors:                 {stats['errors']:>5}")

    if args.dry_run:
        print("\n  *** DRY RUN — no files were modified ***")

    return 0


if __name__ == "__main__":
    exit(main())
