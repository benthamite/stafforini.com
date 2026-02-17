#!/usr/bin/env python3
"""Import WordPress blog posts as Hugo notes.

Parses a WordPress WXR XML export and generates Hugo markdown files
in content/notes/. Published posts get draft: false; drafts and
private posts get draft: true.

Usage:
    python import-blog-posts.py <xml-file>
    python import-blog-posts.py <xml-file> --dry-run
    python import-blog-posts.py <xml-file> --limit 5
"""

import argparse
import html
import re
import unicodedata
from pathlib import Path

from markdownify import markdownify as md

from lib import escape_yaml_string

# === Constants ===

SCRIPTS_DIR = Path(__file__).parent
HUGO_ROOT = SCRIPTS_DIR.parent
NOTES_DIR = HUGO_ROOT / "content" / "notes"


# === Helpers ===


def clean_xml(text: str) -> str:
    """Remove invalid XML control characters."""
    return re.sub(r"[\x00-\x08\x0b\x0c\x0e-\x1f]", "", text)


def strip_html(text: str) -> str:
    """Strip HTML tags from a string."""
    return re.sub(r"<[^>]+>", "", text)


def slugify(title: str) -> str:
    """Generate a URL-safe slug from a title."""
    s = strip_html(title)
    s = unicodedata.normalize("NFKD", s)
    s = s.encode("ascii", "ignore").decode("ascii")
    s = s.lower()
    s = re.sub(r"[^a-z0-9]+", "-", s)
    s = s.strip("-")
    return s or "untitled"


def convert_html_to_markdown(html_content: str) -> str:
    """Convert WordPress HTML content to markdown."""
    if not html_content:
        return ""

    # Normalize WordPress paragraph breaks (double newlines become <p> tags)
    # WordPress stores content with double newlines instead of <p> tags
    # unless it was exported from org-mode (which already has proper HTML)
    if "<p>" not in html_content and "<div" not in html_content:
        # Plain WordPress content: wrap paragraphs
        paragraphs = re.split(r"\n{2,}", html_content)
        html_content = "".join(f"<p>{p.strip()}</p>" for p in paragraphs if p.strip())

    # Handle <!--more--> tag (WordPress read-more marker)
    html_content = html_content.replace("<!--more-->", "")

    # Handle WordPress shortcodes
    # [table id=N /] -> placeholder
    html_content = re.sub(
        r"\[table\s+id=(\d+)\s*/?\]",
        r"*(Table \1 — not available in this format)*",
        html_content,
    )
    # [gallery ...] -> placeholder
    html_content = re.sub(
        r"\[gallery[^\]]*\]",
        "*(Gallery — not available in this format)*",
        html_content,
    )
    # [caption ...] ... [/caption] -> just the inner content
    html_content = re.sub(
        r"\[caption[^\]]*\](.*?)\[/caption\]",
        r"\1",
        html_content,
        flags=re.DOTALL,
    )

    # Convert to markdown
    result = md(
        html_content,
        heading_style="atx",
        bullets="-",
        strip=["script", "style"],
    )

    # Clean up excessive blank lines
    result = re.sub(r"\n{3,}", "\n\n", result)

    return result.strip()


def parse_posts(xml_path: Path) -> list[dict]:
    """Parse WordPress WXR XML and extract all posts."""
    import xml.etree.ElementTree as ET

    xml_text = clean_xml(xml_path.read_text())
    root = ET.fromstring(xml_text)

    ns = {
        "content": "http://purl.org/rss/1.0/modules/content/",
        "wp": "http://wordpress.org/export/1.2/",
        "dc": "http://purl.org/dc/elements/1.1/",
        "excerpt": "http://wordpress.org/export/1.2/excerpt/",
    }

    posts = []
    for item in root.findall(".//item"):
        post_type = item.find("wp:post_type", ns)
        if post_type is None or post_type.text != "post":
            continue

        title_el = item.find("title")
        slug_el = item.find("wp:post_name", ns)
        date_el = item.find("wp:post_date", ns)
        status_el = item.find("wp:status", ns)
        content_el = item.find("content:encoded", ns)

        title = strip_html(title_el.text) if title_el is not None and title_el.text else ""
        slug = slug_el.text if slug_el is not None and slug_el.text else ""
        date = date_el.text if date_el is not None and date_el.text else ""
        status = status_el.text if status_el is not None and status_el.text else ""
        content = content_el.text if content_el is not None and content_el.text else ""

        # Collect categories
        categories = []
        for cat in item.findall("category"):
            if cat.get("domain") == "category" and cat.text:
                categories.append(cat.text)

        # Generate slug from title if missing (common for drafts)
        if not slug:
            slug = slugify(title)

        posts.append({
            "title": title,
            "slug": slug,
            "date": date,
            "status": status,
            "content": content,
            "categories": categories,
        })

    return posts


def generate_note(post: dict) -> str:
    """Generate Hugo markdown content for a blog post."""
    title = post["title"]
    date = post["date"]
    status = post["status"]
    categories = post["categories"]

    # Convert date to Hugo format (YYYY-MM-DDTHH:MM:SS)
    # WP format: 2013-05-24 16:12:13
    if date and " " in date:
        date = date.replace(" ", "T")

    is_draft = status != "publish"

    # Build front matter
    lines = [
        "---",
        f'title: "{escape_yaml_string(title)}"',
        f"date: {date}",
        f"lastmod: {date}",
    ]
    if is_draft:
        lines.append("draft: true")
    if categories:
        cats = [f'"{escape_yaml_string(c)}"' for c in categories if c != "Uncategorized"]
        if cats:
            lines.append(f"categories: [{', '.join(cats)}]")
    lines.append("---")
    lines.append("")

    # Convert content
    body = convert_html_to_markdown(post["content"])
    if body:
        lines.append(body)
        lines.append("")

    return "\n".join(lines)


# === Main ===


def main():
    parser = argparse.ArgumentParser(description="Import WordPress blog posts as Hugo notes")
    parser.add_argument("xml_file", type=Path, help="Path to WordPress WXR XML export")
    parser.add_argument("--dry-run", action="store_true", help="Preview without writing files")
    parser.add_argument("--limit", type=int, default=0, help="Limit number of posts to import")
    args = parser.parse_args()

    if not args.xml_file.exists():
        print(f"ERROR: {args.xml_file} not found")
        return

    print("=" * 60)
    print("Importing WordPress blog posts as Hugo notes")
    print("=" * 60)

    # Parse XML
    print("\nParsing XML export...")
    posts = parse_posts(args.xml_file)
    print(f"  Found {len(posts)} posts")

    from collections import Counter
    statuses = Counter(p["status"] for p in posts)
    for status, count in sorted(statuses.items()):
        print(f"    {status}: {count}")

    # Ensure output directory exists
    NOTES_DIR.mkdir(parents=True, exist_ok=True)

    # Check for existing files
    existing = {f.stem for f in NOTES_DIR.glob("*.md") if f.name != "_index.md"}

    stats = {"created": 0, "skipped_exists": 0, "skipped_empty": 0}
    processed = 0

    for post in posts:
        if args.limit and processed >= args.limit:
            break

        slug = post["slug"]

        # Skip if file already exists
        if slug in existing:
            stats["skipped_exists"] += 1
            processed += 1
            continue

        # Skip posts with no content
        if not post["content"].strip():
            stats["skipped_empty"] += 1
            processed += 1
            if not args.dry_run:
                print(f"  SKIP (empty): {post['title']}")
            continue

        note_content = generate_note(post)
        note_path = NOTES_DIR / f"{slug}.md"

        if args.dry_run:
            if processed < 10:
                draft_marker = " [DRAFT]" if post["status"] != "publish" else ""
                print(f"  [CREATE] {slug}.md{draft_marker} — {post['title']}")
        else:
            note_path.write_text(note_content)

        stats["created"] += 1
        processed += 1

        if processed % 50 == 0:
            print(f"  ... {processed} posts processed")

    print(f"\n  Notes created:     {stats['created']}")
    print(f"  Skipped (exists):  {stats['skipped_exists']}")
    print(f"  Skipped (empty):   {stats['skipped_empty']}")

    if args.dry_run:
        print("  *** DRY RUN — no files written ***")

    print("\nDone.")


if __name__ == "__main__":
    main()
