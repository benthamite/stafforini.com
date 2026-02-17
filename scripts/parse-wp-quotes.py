#!/usr/bin/env python3
"""Parse WordPress WXR export of notatu dignum quotes into structured JSON.

Extracts quote text, attribution, author, work title, year, locator,
categories, and tags from each published post.
"""

import json
import re
import sys
import xml.etree.ElementTree as ET
from html import unescape
from pathlib import Path

WXR_FILE = Path(__file__).parent.parent.parent.parent / "Downloads" / "notatudignum.WordPress.2026-02-15.xml"
OUTPUT_FILE = Path(__file__).parent / "wp-quotes-parsed.json"

NS = {
    "content": "http://purl.org/rss/1.0/modules/content/",
    "wp": "http://wordpress.org/export/1.2/",
    "dc": "http://purl.org/dc/elements/1.1/",
    "excerpt": "http://wordpress.org/export/1.2/excerpt/",
}


def clean_xml(data: bytes) -> bytes:
    """Remove control characters that are invalid in XML."""
    return re.sub(rb"[\x00-\x08\x0b\x0c\x0e-\x1f]", b"", data)


def strip_html(html: str) -> str:
    """Remove HTML tags and decode entities."""
    text = re.sub(r"<[^>]+>", "", html)
    text = unescape(text)
    # Normalize whitespace
    text = re.sub(r"\s+", " ", text).strip()
    return text


def html_to_text_preserving_emphasis(html: str) -> str:
    """Convert HTML to plain text, preserving emphasis as markdown.

    Converts <em>/<i> → *...* and <strong>/<b> → **...**
    before stripping remaining tags.
    """
    text = html
    # Bold first (so ** doesn't get caught by italic conversion)
    text = re.sub(
        r"<(strong|b)\b[^>]*>(.*?)</\1>",
        r"**\2**",
        text,
        flags=re.DOTALL | re.IGNORECASE,
    )
    # Italic
    text = re.sub(
        r"<(em|i)\b[^>]*>(.*?)</\1>",
        r"*\2*",
        text,
        flags=re.DOTALL | re.IGNORECASE,
    )
    # Strip remaining HTML tags
    text = re.sub(r"<[^>]+>", "", text)
    text = unescape(text)
    # Normalize whitespace
    text = re.sub(r"\s+", " ", text).strip()
    return text


def extract_quote_and_attribution(content_html: str) -> tuple[str, str, str]:
    """Split HTML content into quote text and attribution.

    Returns (quote_text, attribution_text, attribution_html).
    """
    if not content_html:
        return "", "", ""

    # Find blockquote content
    bq_match = re.search(
        r"<blockquote[^>]*>(.*?)</blockquote>", content_html, re.DOTALL | re.IGNORECASE
    )
    if bq_match:
        quote_html = bq_match.group(1)
        quote_text = html_to_text_preserving_emphasis(quote_html)

        # Everything after the last </blockquote> is the attribution
        parts = re.split(r"</blockquote>", content_html, flags=re.IGNORECASE)
        attr_html = parts[-1].strip() if len(parts) > 1 else ""
        attr_text = strip_html(attr_html)
    else:
        # No blockquote — treat entire content as quote, no attribution
        quote_text = html_to_text_preserving_emphasis(content_html)
        attr_html = ""
        attr_text = ""

    return quote_text, attr_text, attr_html


def extract_work_title(attr_html: str) -> str:
    """Extract the work title from <i>, <em>, or <cite> tags in the attribution."""
    # Find all italic/em/cite contents
    matches = re.findall(r"<(?:i|em|cite)[^>]*>(.*?)</(?:i|em|cite)>", attr_html, re.DOTALL | re.IGNORECASE)
    if matches:
        return strip_html(matches[0])
    return ""


def extract_article_title(attr_html: str) -> str:
    """Extract the article/chapter title from quoted text in the attribution.

    In WP attributions, article and chapter titles appear in single quotes:
      Author, 'Article Title', in <i>Book Title</i>, ...
    """
    attr_text = strip_html(attr_html)
    # Match text in single quotes, curly quotes, or double quotes
    # after the author name (skip past the first comma)
    match = re.search(
        r",\s*['\u2018\u2019\u201c\u201d\"](.*?)['\u2018\u2019\u201c\u201d\"]",
        attr_text,
    )
    if match:
        return match.group(1).strip()
    return ""


def extract_year(attr_text: str) -> str:
    """Extract a 4-digit year from the attribution."""
    # Look for years in the range 1400-2029
    years = re.findall(r"\b(1[4-9]\d{2}|20[0-2]\d)\b", attr_text)
    if years:
        # Return the last year found (usually the publication year comes last,
        # or at least after the original date in reprints)
        return years[-1]
    return ""


def extract_locator(attr_text: str) -> str:
    """Extract page/chapter locator from the attribution."""
    # Match patterns like: p. 27, pp. 44-45, ch. 2, vol. 28, sec. 3
    locator_match = re.search(
        r"((?:pp?\.|ch\.|sec\.|§)\s*[\d\w]+(?:\s*[-–—]\s*[\d\w]+)?)",
        attr_text,
    )
    if locator_match:
        return locator_match.group(1).strip()
    return ""


def extract_author_from_attribution(attr_text: str) -> str:
    """Extract the author name from the beginning of the attribution string."""
    if not attr_text:
        return ""
    # The attribution typically starts with "Author Name, " followed by
    # a work title (in italics) or a quoted chapter title
    # Take everything before the first comma that's followed by typical citation markers
    match = re.match(r"^(.+?),\s*(?:['\"\u2018\u2019\u201c\u201d]|in\s|(?:quoted in\s)|Review of\s|<)", attr_text)
    if match:
        return match.group(1).strip()
    # Fallback: take text before first comma
    parts = attr_text.split(",", 1)
    if parts:
        candidate = parts[0].strip()
        # Sanity check: should look like a name (not too long)
        if len(candidate) < 80:
            return candidate
    return ""


def parse_item(item: ET.Element) -> dict | None:
    """Parse a single WXR <item> element into a structured dict."""
    post_type_el = item.find("wp:post_type", NS)
    if post_type_el is None or post_type_el.text != "post":
        return None

    status_el = item.find("wp:status", NS)
    if status_el is None or status_el.text != "publish":
        return None

    content_el = item.find("content:encoded", NS)
    content_html = content_el.text if content_el is not None else ""

    quote_text, attr_text, attr_html = extract_quote_and_attribution(content_html or "")

    # Categories (author names) and tags
    categories = []
    tags = []
    for cat in item.findall("category"):
        domain = cat.get("domain", "")
        name = cat.text or ""
        if domain == "category":
            categories.append(name)
        elif domain == "post_tag":
            tags.append(name)

    # Post metadata
    title = item.find("title").text or ""
    post_id = item.find("wp:post_id", NS).text or ""
    post_date = item.find("wp:post_date", NS).text or ""
    link = item.find("link").text or ""

    # Derived fields
    work_title = extract_work_title(attr_html)
    article_title = extract_article_title(attr_html)
    year = extract_year(attr_text)
    locator = extract_locator(attr_text)
    author = categories[0] if categories else extract_author_from_attribution(attr_text)

    return {
        "post_id": post_id,
        "date": post_date,
        "link": link,
        "wp_title": title,
        "author": author,
        "quote_text": quote_text,
        "article_title": article_title,
        "attribution_text": attr_text,
        "attribution_html": attr_html,
        "work_title": work_title,
        "year": year,
        "locator": locator,
        "categories": categories,
        "tags": tags,
    }


def main():
    wxr_path = Path(sys.argv[1]) if len(sys.argv) > 1 else WXR_FILE

    if not wxr_path.exists():
        print(f"Error: {wxr_path} not found", file=sys.stderr)
        sys.exit(1)

    print(f"Reading {wxr_path}...")
    raw = wxr_path.read_bytes()
    cleaned = clean_xml(raw)

    print("Parsing XML...")
    root = ET.fromstring(cleaned)

    quotes = []
    skipped = 0
    for item in root.findall(".//item"):
        result = parse_item(item)
        if result is None:
            skipped += 1
            continue
        quotes.append(result)

    # Sort by date
    quotes.sort(key=lambda q: q["date"])

    print(f"\nParsed {len(quotes)} quotes (skipped {skipped} non-post items)")

    # Summary statistics
    with_title = sum(1 for q in quotes if q["work_title"])
    with_year = sum(1 for q in quotes if q["year"])
    with_locator = sum(1 for q in quotes if q["locator"])
    no_quote = sum(1 for q in quotes if not q["quote_text"])
    no_attr = sum(1 for q in quotes if not q["attribution_text"])

    print(f"  Work title extracted: {with_title}/{len(quotes)}")
    print(f"  Year extracted:       {with_year}/{len(quotes)}")
    print(f"  Locator extracted:    {with_locator}/{len(quotes)}")
    print(f"  Missing quote text:   {no_quote}/{len(quotes)}")
    print(f"  Missing attribution:  {no_attr}/{len(quotes)}")

    # Unique authors
    authors = set(q["author"] for q in quotes if q["author"])
    print(f"  Unique authors:       {len(authors)}")

    output_path = Path(sys.argv[2]) if len(sys.argv) > 2 else OUTPUT_FILE
    output_path.write_text(json.dumps(quotes, indent=2, ensure_ascii=False))
    print(f"\nOutput written to {output_path}")

    # Show a few examples
    print("\n--- Sample entries ---")
    for q in quotes[:3]:
        print(f"\n  Author:  {q['author']}")
        print(f"  Title:   {q['work_title']}")
        print(f"  Year:    {q['year']}")
        print(f"  Locator: {q['locator']}")
        print(f"  Quote:   {q['quote_text'][:80]}...")
        print(f"  Tags:    {', '.join(q['tags'])}")


if __name__ == "__main__":
    main()
