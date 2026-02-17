#!/usr/bin/env python3
"""Write WordPress quotes as :public: subheadings in org-roam bibliographic notes.

Phase 4 of the quote migration pipeline. Each quote becomes a level-2
subheading in the bibliographic note file for its cite key, with ox-hugo
export properties so quotes can be published via org-hugo-export-wim-to-md.

Usage:
    python write-quotes-to-org.py                  # Process all quotes
    python write-quotes-to-org.py --limit 10       # Process first 10
    python write-quotes-to-org.py --dry-run        # Print without writing
    python write-quotes-to-org.py --dry-run --limit 10  # Preview first 10
"""

import argparse
import json
import re
import uuid
import unicodedata
from datetime import datetime
from pathlib import Path

from lib import BIB_FILES
from lib import STOP_WORDS as _BASE_STOP_WORDS
from lib import cite_key_to_slug, parse_bib_entries

# === Constants ===

SCRIPTS_DIR = Path(__file__).parent
MATCHED_JSON = SCRIPTS_DIR / "wp-quotes-matched.json"
PROGRESS_FILE = SCRIPTS_DIR / "write-quotes-progress.json"
REPORT_FILE = SCRIPTS_DIR / "write-quotes-report.txt"

NOTES_DIR = Path.home() / "Library/CloudStorage/Dropbox/bibliographic-notes"
HUGO_BASE_DIR = "~/Library/CloudStorage/Dropbox/repos/stafforini.com/"
NOTER_PDF_DIR = "~/My Drive/library-pdf"

STOP_WORDS = _BASE_STOP_WORDS | {
    "it", "all", "any", "each", "every", "no", "nor", "only", "own", "same",
    "such", "too", "very", "can", "will", "just", "should", "now",
    "i", "me", "my", "we", "us", "our", "you", "your", "he", "him",
    "his", "she", "her", "they", "them", "their", "who", "whom", "which",
    "what", "when", "where", "how", "why", "there", "here", "then",
    "about", "into", "over", "after", "before", "between", "under",
    "more", "most", "other", "some", "would", "could", "may", "might",
    "must", "shall",
}


# === Bib file parsing ===


def _parse_bib_entries_for_org(bib_path: Path) -> list[dict]:
    """Parse bib entries with brace-stripped title but raw author/editor.

    Keeps braces in author/editor for apa_format_authors_abbrev.
    """
    return parse_bib_entries(bib_path, strip_braces=["title"])


# === Author formatting ===


def apa_format_authors_abbrev(value: str) -> str:
    """Replicate bibtex-completion-apa-format-authors-abbrev.

    Source: helm-bibtex/bibtex-completion.el lines 1251-1289.
    """
    # Strip outer braces from the raw bib field value
    value = value.strip()
    if not value:
        return ""

    authors = []
    for a in re.split(r"\s+and\s+", value):
        a = a.strip()
        if not a:
            continue
        if "{" in a:
            # Brace-protected name: strip braces, use as-is
            authors.append(re.sub(r"[{}]", "", a))
        elif "," in a:
            # "Last, First" or "Last, First Middle" format
            parts = re.split(r"\s*,\s*", a, maxsplit=1)
            surname = re.sub(r"[{}]", "", parts[0])
            given = re.sub(r"[{}]", "", parts[1]) if len(parts) > 1 else ""
            initials = " ".join(g[0] + "." for g in given.split() if g)
            authors.append(f"{surname}, {initials}" if initials else surname)
        else:
            # "First Last" format
            a_clean = re.sub(r"[{}]", "", a)
            parts = a_clean.split()
            if parts:
                surname = parts[-1]
                initials = " ".join(p[0] + "." for p in parts[:-1])
                authors.append(f"{surname}, {initials}" if initials else surname)

    n = len(authors)
    if n == 0:
        return ""
    elif n == 1:
        return authors[0]
    elif n == 2:
        return " & ".join(authors)
    else:
        return f"{authors[0]} et al."


def get_author_abbrev(entry: dict) -> str:
    """Get abbreviated author (or editor) for a bib entry."""
    if entry["author"]:
        return apa_format_authors_abbrev(entry["author"])
    if entry["editor"]:
        return apa_format_authors_abbrev(entry["editor"])
    return ""


def get_primary_surname(entry: dict) -> str:
    """Extract the primary author/editor surname for slug generation."""
    value = entry["author"] or entry["editor"]
    if not value:
        return ""
    first_author = re.split(r"\s+and\s+", value)[0].strip()
    first_author = re.sub(r"[{}]", "", first_author)
    if "," in first_author:
        return first_author.split(",")[0].strip()
    parts = first_author.split()
    return parts[-1] if parts else ""


# === Entry type tag mapping ===

ENTRY_TYPE_TAGS = {
    "book": "book",
    "mvbook": "book",
    "collection": "book",
    "mvcollection": "book",
    "reference": "book",
    "mvreference": "book",
    "proceedings": "book",
    "mvproceedings": "book",
    "article": "article",
    "incollection": "incollection",
    "inbook": "incollection",
    "inproceedings": "incollection",
    "inreference": "incollection",
    "thesis": "thesis",
    "phdthesis": "thesis",
    "mastersthesis": "thesis",
    "online": "online",
    "misc": "misc",
    "unpublished": "misc",
    "report": "report",
    "techreport": "report",
    "video": "video",
    "movie": "video",
    "music": "music",
    "audio": "music",
    "letter": "letter",
    "speech": "speech",
}


def get_entry_type_tag(entry_type: str) -> str:
    """Map bib entry type to org tag."""
    return ENTRY_TYPE_TAGS.get(entry_type, entry_type)


# === Subheading title generation ===


def slugify(text: str) -> str:
    """Convert text to a URL-friendly slug."""
    text = unicodedata.normalize("NFD", text)
    text = "".join(c for c in text if unicodedata.category(c) != "Mn")
    text = text.lower()
    text = re.sub(r"[^\w\s-]", "", text)
    text = re.sub(r"[\s_]+", "-", text).strip("-")
    return text


def generate_subheading_title(quote: dict) -> str:
    """Generate a subheading title from WP tags or quote text."""
    # Use first WP tag if available
    if quote.get("tags"):
        return quote["tags"][0]

    # Fallback: first 5-6 significant words of the quote
    text = quote["quote_text"]
    words = re.findall(r"[a-zA-Z]+", text)
    significant = [w.lower() for w in words if w.lower() not in STOP_WORDS]
    return " ".join(significant[:5]) if significant else "untitled"


# === Quote text escaping ===


def markdown_to_org_emphasis(text: str) -> str:
    """Convert markdown emphasis to org-mode emphasis.

    **text** → *text* (bold)
    *text*  → /text/ (italic)

    Bold must be converted before italic so that ** markers
    are consumed first and don't get partially matched as *.
    """
    # Bold: **text** → *text*
    text = re.sub(r"\*\*(.+?)\*\*", r"*\1*", text)
    # Italic: *text* → /text/
    text = re.sub(r"(?<!\*)\*([^*]+?)\*(?!\*)", r"/\1/", text)
    return text


def escape_org_text(text: str) -> str:
    """Escape text for safe inclusion in an org-mode quote block."""
    lines = text.split("\n")
    escaped = []
    for line in lines:
        # Escape * at line start (would be interpreted as heading)
        if line.startswith("*"):
            line = "\u200B" + line  # zero-width space prefix
        # Escape #+ sequences that look like org keywords
        if line.startswith("#+"):
            line = "\u200B" + line
        escaped.append(line)
    return "\n".join(escaped)


# === Export filename generation ===


def generate_export_filename(surname: str, title: str, global_slugs: dict) -> str:
    """Generate a globally unique export filename slug.

    Format: {author-surname}-{title-slug}
    Deduplicates with numeric suffix if collision.
    """
    surname_slug = slugify(surname) if surname else "unknown"
    title_slug = slugify(title)
    if not title_slug:
        title_slug = "untitled"

    base = f"{surname_slug}-{title_slug}"
    candidate = base
    counter = 2
    while candidate in global_slugs:
        candidate = f"{base}-{counter}"
        counter += 1

    global_slugs[candidate] = True
    return candidate


# === Org file operations ===


def new_uuid() -> str:
    """Generate a new UUID string for org-roam :ID: property."""
    return str(uuid.uuid4()).upper()


def build_new_org_file(cite_key: str, entry: dict) -> str:
    """Build the content of a new org-roam bibliographic note file."""
    author_abbrev = get_author_abbrev(entry)
    title = entry["title"]
    entry_tag = get_entry_type_tag(entry["entry_type"])
    node_id = new_uuid()

    heading = f"{author_abbrev} — {title}" if author_abbrev else title

    lines = [
        f"#+hugo_base_dir: {HUGO_BASE_DIR}",
        f"* {heading}",
        ":PROPERTIES:",
        f":ROAM_REFS: [cite:@{cite_key}]",
        f":Custom_ID: {cite_key}",
        f":NOTER_DOCUMENT: {NOTER_PDF_DIR}/{cite_key}.pdf",
        ":NOTER_PAGE:",
        f":ID:       {node_id}",
        ":END:",
    ]

    # Add tags to the heading line
    # Calculate padding: org convention is tags right-aligned to column 72
    heading_line = lines[1]
    tag_str = f":{entry_tag}:biblio:"
    pad_needed = max(1, 72 - len(heading_line) - len(tag_str))
    lines[1] = heading_line + " " * pad_needed + tag_str

    return "\n".join(lines) + "\n"


def build_subheading(
    quote: dict,
    cite_key: str,
    title: str,
    export_filename: str,
    work_slug: str,
) -> str:
    """Build the org-mode subheading for a quote."""
    node_id = new_uuid()

    # Parse date from WP date string
    date_str = quote["date"].split(" ")[0]  # "2003-01-01 00:00:00" -> "2003-01-01"

    # Build locator string for front matter
    locator = quote.get("locator", "")

    # Build EXPORT_HUGO_CUSTOM_FRONT_MATTER
    custom_fm_parts = [f':work "{work_slug}"']
    if locator:
        custom_fm_parts.append(f':locator "{locator}"')
    custom_fm = " ".join(custom_fm_parts)

    # Tag line with padding (right-aligned to ~72)
    heading_text = f"** {title}"
    tag_str = ":public:"
    pad_needed = max(1, 72 - len(heading_text) - len(tag_str))
    heading_line = heading_text + " " * pad_needed + tag_str

    # Build the subheading
    lines = [
        "",
        heading_line,
        ":PROPERTIES:",
        f":EXPORT_FILE_NAME: {export_filename}",
        ":EXPORT_HUGO_SECTION: quotes",
        f":EXPORT_DATE: {date_str}",
        f":EXPORT_HUGO_CUSTOM_FRONT_MATTER: {custom_fm}",
        f":ID:       {node_id}",
        f":WP_POST_ID: {quote['post_id']}",
        f":WP_LINK: {quote['link']}",
        ":END:",
        "#+begin_quote",
        escape_org_text(markdown_to_org_emphasis(quote["quote_text"])),
        "#+end_quote",
    ]

    # Add citation line
    cite_ref = f"[cite:@{cite_key}"
    if locator:
        cite_ref += f", {locator}"
    cite_ref += "]"
    lines.append("")
    lines.append(cite_ref)

    return "\n".join(lines) + "\n"


def ensure_hugo_base_dir(content: str) -> str:
    """Ensure #+hugo_base_dir: is present at the top of an org file."""
    hugo_line = f"#+hugo_base_dir: {HUGO_BASE_DIR}"
    if "#+hugo_base_dir:" in content.lower():
        return content
    # Add before first heading
    return hugo_line + "\n" + content


def _normalize_for_dedup(text: str) -> str:
    """Normalize text for duplicate comparison.

    Strips org block delimiters (including comma-escaped variants like
    ,#+begin_quote), org inline markup (e.g. /italics/, *bold*, ~code~),
    and non-alphanumeric characters so that formatting differences between
    existing org content and plain WordPress text don't prevent matching.
    """
    # Strip org block delimiters (including comma-escaped variants)
    # before general cleanup, so they don't leave artifact words like
    # "begin_quote" that break substring matching.
    text = re.sub(r"^,?#\+(?:begin|end)_\w+\s*$", "", text, flags=re.MULTILINE)
    text = re.sub(r"\s+", " ", text.strip())
    text = re.sub(r"[^\w\s]", "", text)
    return text.lower()


def check_duplicate_quote(content: str, quote_text: str) -> bool:
    """Check if the same quote text already exists in the file."""
    return _normalize_for_dedup(quote_text) in _normalize_for_dedup(content)


def deduplicate_title_in_file(content: str, title: str) -> str:
    """Ensure the subheading title is unique within the file."""
    # Check if this exact title already exists as a heading
    pattern = re.compile(r"^\*+ " + re.escape(title) + r"\s", re.MULTILINE)
    if not pattern.search(content):
        return title

    counter = 2
    while True:
        candidate = f"{title} {counter}"
        pattern = re.compile(r"^\*+ " + re.escape(candidate) + r"\s", re.MULTILINE)
        if not pattern.search(content):
            return candidate
        counter += 1


# === Main processing ===


def get_cite_key(quote: dict) -> str:
    """Determine the cite key for a quote."""
    if quote["bib_match"]["status"] == "matched":
        return quote["bib_match"]["matches"][0]["cite_key"]
    return quote.get("new_cite_key", "")


def main():
    parser = argparse.ArgumentParser(description="Write quotes to org-roam bibliographic notes")
    parser.add_argument("--dry-run", action="store_true", help="Print what would be done without writing files")
    parser.add_argument("--limit", type=int, help="Process only N quotes")
    args = parser.parse_args()

    print("Loading quotes...")
    quotes = json.loads(MATCHED_JSON.read_text())
    print(f"  {len(quotes)} quotes loaded")

    if args.limit:
        quotes = quotes[:args.limit]
        print(f"  Limited to {len(quotes)} quotes")

    # Load progress
    progress = {}
    if PROGRESS_FILE.exists() and not args.dry_run:
        progress = json.loads(PROGRESS_FILE.read_text())
        print(f"  Resuming: {len(progress.get('processed', []))} already processed")

    processed_ids = set(progress.get("processed", []))

    # Parse all bib files
    print("\nParsing bib files...")
    bib_by_key = {}
    for bib_path in BIB_FILES:
        if not bib_path.exists():
            print(f"  WARNING: {bib_path} not found, skipping")
            continue
        entries = _parse_bib_entries_for_org(bib_path)
        for entry in entries:
            bib_by_key[entry["cite_key"]] = entry
        print(f"  {bib_path.name}: {len(entries)} entries")
    print(f"  Total unique keys: {len(bib_by_key)}")

    # Group quotes by cite key for per-file processing
    quotes_by_key = {}
    skipped_no_key = 0
    for quote in quotes:
        cite_key = get_cite_key(quote)
        if not cite_key:
            skipped_no_key += 1
            continue
        quotes_by_key.setdefault(cite_key, []).append(quote)

    if skipped_no_key:
        print(f"\n  WARNING: {skipped_no_key} quotes have no cite key, skipping")

    print(f"\n  {len(quotes_by_key)} unique cite keys to process")

    # Track global state
    global_export_slugs = {}  # for deduplication of export filenames
    stats = {
        "files_created": 0,
        "files_modified": 0,
        "subheadings_added": 0,
        "duplicates_skipped": 0,
        "already_processed": 0,
        "missing_bib_entry": 0,
    }
    report_lines = []
    newly_processed = []

    # Process quotes grouped by cite key
    print("\nProcessing quotes...")
    sorted_keys = sorted(quotes_by_key.keys())

    for key_idx, cite_key in enumerate(sorted_keys):
        key_quotes = quotes_by_key[cite_key]

        # Look up bib entry
        bib_entry = bib_by_key.get(cite_key)
        if not bib_entry:
            stats["missing_bib_entry"] += len(key_quotes)
            for q in key_quotes:
                report_lines.append(
                    f"[MISSING_BIB] post_id={q['post_id']}, cite_key={cite_key}: "
                    f"no bib entry found in any bib file"
                )
            continue

        org_path = NOTES_DIR / f"{cite_key}.org"
        surname = get_primary_surname(bib_entry)
        work_slug = cite_key_to_slug(cite_key)

        # Read or create org file
        file_existed = org_path.exists()
        if file_existed:
            content = org_path.read_text()
        else:
            content = build_new_org_file(cite_key, bib_entry)

        # Ensure hugo_base_dir is present
        content = ensure_hugo_base_dir(content)

        # Track what we add to this file
        additions = []

        # Sort quotes by date for consistent ordering
        key_quotes.sort(key=lambda q: q["date"])

        for quote in key_quotes:
            post_id = quote["post_id"]

            # Skip if already processed
            if post_id in processed_ids:
                stats["already_processed"] += 1
                continue

            # Check for duplicate quote text
            if check_duplicate_quote(content, quote["quote_text"]):
                stats["duplicates_skipped"] += 1
                report_lines.append(
                    f"[DUPLICATE] post_id={post_id}, cite_key={cite_key}: "
                    f"quote text already exists in file"
                )
                newly_processed.append(post_id)
                continue

            # Generate subheading title
            raw_title = generate_subheading_title(quote)
            # Check combined content (original + all additions so far) for title dedup
            combined = content + "".join(additions)
            title = deduplicate_title_in_file(combined, raw_title)

            # Generate export filename
            export_filename = generate_export_filename(surname, title, global_export_slugs)

            # Build subheading
            subheading = build_subheading(
                quote=quote,
                cite_key=cite_key,
                title=title,
                export_filename=export_filename,
                work_slug=work_slug,
            )

            additions.append(subheading)
            stats["subheadings_added"] += 1
            newly_processed.append(post_id)

        if not additions:
            continue

        # Combine content
        final_content = content + "".join(additions)

        if args.dry_run:
            if not file_existed:
                print(f"\n  [CREATE] {org_path.name}")
                stats["files_created"] += 1
            else:
                print(f"\n  [MODIFY] {org_path.name}")
                stats["files_modified"] += 1
            for addition in additions:
                # Show just the heading line
                for line in addition.split("\n"):
                    if line.startswith("** "):
                        print(f"    + {line.strip()}")
                        break
        else:
            org_path.write_text(final_content)
            if not file_existed:
                stats["files_created"] += 1
            else:
                stats["files_modified"] += 1

        # Progress update every 100 keys
        if (key_idx + 1) % 100 == 0:
            print(f"  ... {key_idx + 1}/{len(sorted_keys)} cite keys processed")

    # Save progress
    if not args.dry_run and newly_processed:
        all_processed = list(processed_ids | set(newly_processed))
        PROGRESS_FILE.write_text(json.dumps({"processed": all_processed}, indent=2))
        print(f"\nProgress saved: {len(all_processed)} total processed")

    # Print summary
    print(f"\n{'='*60}")
    print("RESULTS")
    print(f"{'='*60}")
    print(f"  Files created:       {stats['files_created']:>5}")
    print(f"  Files modified:      {stats['files_modified']:>5}")
    print(f"  Subheadings added:   {stats['subheadings_added']:>5}")
    print(f"  Duplicates skipped:  {stats['duplicates_skipped']:>5}")
    print(f"  Already processed:   {stats['already_processed']:>5}")
    print(f"  Missing bib entry:   {stats['missing_bib_entry']:>5}")

    if args.dry_run:
        print("\n  *** DRY RUN — no files were modified ***")

    # Write report
    report_content = (
        f"Write Quotes to Org Report\n"
        f"Generated: {datetime.now().isoformat()}\n"
        f"{'='*60}\n\n"
        f"Files created: {stats['files_created']}\n"
        f"Files modified: {stats['files_modified']}\n"
        f"Subheadings added: {stats['subheadings_added']}\n"
        f"Duplicates skipped: {stats['duplicates_skipped']}\n"
        f"Already processed: {stats['already_processed']}\n"
        f"Missing bib entry: {stats['missing_bib_entry']}\n\n"
    )

    if report_lines:
        report_content += f"Issues ({len(report_lines)}):\n"
        report_content += "\n".join(report_lines) + "\n"
    else:
        report_content += "No issues.\n"

    if not args.dry_run:
        REPORT_FILE.write_text(report_content)
        print(f"\nReport written to {REPORT_FILE}")
    else:
        print(f"\nReport preview ({len(report_lines)} issues)")


if __name__ == "__main__":
    main()
