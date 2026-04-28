#!/usr/bin/env python3
"""Generate Hugo work pages and post-process exported quote markdown files.

Creates or updates work pages under content/works/ from BibTeX data for
every entry across all bib files. Re-running after a BibTeX change
will update any existing work pages whose data has changed.

Usage:
    python generate-work-pages.py                    # Full run
    python generate-work-pages.py --dry-run          # Preview without writing
    python generate-work-pages.py --limit 10         # Process first 10
    python generate-work-pages.py --skip-postprocess # Skip citation stripping
    python generate-work-pages.py --skip-works       # Skip work page generation
"""

import argparse
import re
from pathlib import Path

from lib import (
    BIB_FILES,
    atomic_write_json,
    atomic_write_text,
    cite_key_to_slug,
    escape_yaml_string,
    load_excluded_works,
    parse_bib_entries,
    safe_remove,
)

# === Constants ===

SCRIPTS_DIR = Path(__file__).parent
HUGO_ROOT = SCRIPTS_DIR.parent
QUOTES_DIR = HUGO_ROOT / "content" / "quotes"
WORKS_DIR = HUGO_ROOT / "content" / "works"
WORK_METADATA_PATH = HUGO_ROOT / "data" / "works.json"
COLLECTION_LIKE_TYPES = frozenset({"collection", "proceedings"})


# === Bib file parsing ===


def _parse_bib_entries_for_works(bib_path: Path) -> list[dict]:
    """Parse bib entries with extra fields needed for work page generation."""
    return parse_bib_entries(
        bib_path,
        strip_braces=False,
        extra_fields=[
            "location", "booktitle", "journaltitle",
            "volume", "number", "pages", "bookauthor", "crossref", "abstract",
            "url", "date", "shorttitle",
        ],
        field_fallbacks={"location": "address"},
    )


def resolve_crossrefs(bib_by_key: dict) -> None:
    """Resolve crossref fields: inherit missing fields from parent entries.

    Runs multiple passes to handle chained crossrefs (e.g. A → B → C),
    stopping when no further changes occur.
    """
    inherit_fields = ["booktitle", "journaltitle", "location", "editor", "volume", "number"]
    changed = True
    while changed:
        changed = False
        for entry in bib_by_key.values():
            crossref_key = entry.get("crossref", "")
            if not crossref_key:
                continue
            parent = bib_by_key.get(crossref_key)
            if not parent:
                continue
            for field in inherit_fields:
                if not entry.get(field) and parent.get(field):
                    entry[field] = parent[field]
                    changed = True
            if not entry.get("booktitle") and parent.get("title"):
                entry["booktitle"] = parent["title"]
                changed = True
            if not entry.get("title") and parent.get("title"):
                entry["title"] = parent["title"]
                changed = True
            if not entry.get("author") and parent.get("author"):
                entry["author"] = parent["author"]
                changed = True
            if not entry.get("year") and parent.get("year"):
                entry["year"] = parent["year"]
                changed = True
            if not entry.get("editor") and parent.get("editor") and entry.get("author"):
                entry["editor"] = parent["editor"]
                changed = True
            # Capture parent's author as bookauthor for chapter-like entries
            # whose parent is the containing book (e.g. an inbook with its own
            # author, crossref'd to the @book). Only kicks in when the parent
            # has a different author -- if they match, the bookauthor is
            # redundant with the section author.
            if (
                entry.get("entry_type") in ("inbook", "incollection", "inproceedings")
                and entry.get("author")
                and not entry.get("bookauthor")
                and parent.get("author")
                and parent["author"] != entry["author"]
            ):
                entry["bookauthor"] = parent["author"]
                changed = True


# === Author formatting ===


def bib_author_to_display(value: str) -> str:
    """Convert bib author field to 'First Last' display format.

    Handles:
    - 'Last, First' -> 'First Last'
    - 'Last, First and Last2, First2' -> 'First Last and First2 Last2'
    - Brace-protected names like '{World Health Organization}'
    - Multi-word names like 'de Tocqueville, Alexis' -> 'Alexis de Tocqueville'
    """
    if not value.strip():
        return ""

    # Strip outer braces
    value = value.replace("{", "").replace("}", "")

    authors = []
    for a in re.split(r"\s+and\s+", value):
        a = a.strip()
        if not a:
            continue
        if "," in a:
            parts = re.split(r"\s*,\s*", a, maxsplit=1)
            surname = parts[0].strip()
            given = parts[1].strip() if len(parts) > 1 else ""
            if given:
                authors.append(f"{given} {surname}")
            else:
                authors.append(surname)
        else:
            authors.append(a)

    n = len(authors)
    if n == 0:
        return ""
    elif n == 1:
        return authors[0]
    elif n == 2:
        return f"{authors[0]} and {authors[1]}"
    elif n == 3:
        return f"{authors[0]}, {authors[1]}, and {authors[2]}"
    else:
        return f"{authors[0]} et al."


# === Citation stripping ===


def strip_citation_from_content(text: str) -> str:
    """Strip the CSL citation line that ox-hugo appends after blockquotes.

    ox-hugo renders org-cite references as a plain-text citation line below the
    blockquote (e.g. "(Author, Year, p. XX)").  Hugo's cite shortcode system
    replaces this with a richer citation, so we strip the ox-hugo version to
    avoid duplication.

    The exported markdown looks like:
        > Quote text here.
        >
        > More quote text.

        (??, ????, p. XX)

    or sometimes with actual CSL-rendered citation text. We strip everything
    after the last blockquote line.
    """
    lines = text.split("\n")

    # Find the last line that starts with '>'
    last_blockquote_idx = -1
    for i, line in enumerate(lines):
        if line.startswith(">"):
            last_blockquote_idx = i

    if last_blockquote_idx == -1:
        # No blockquote found, return as-is
        return text

    # Keep everything up to and including the last blockquote line
    result_lines = lines[:last_blockquote_idx + 1]

    # Scan remaining lines for a Topics line to preserve
    for line in lines[last_blockquote_idx + 1:]:
        if line.startswith("Topics:"):
            result_lines.append("")
            result_lines.append(line)
            break

    # Ensure the file ends with a single newline
    return "\n".join(result_lines) + "\n"


def _work_slug_from_front_matter(front_matter: str) -> str:
    """Extract the ``work`` slug from a quote markdown file's front matter."""
    m = re.search(r'^work\s*[=:]\s*"([^"]+)"', front_matter, re.MULTILINE)
    return m.group(1) if m else ""


def postprocess_quotes(dry_run: bool = False) -> dict:
    """Strip citation placeholders and remove takedown-blocked quotes.

    Also removes any quote markdown whose ``work`` front matter points at
    a slug in the takedown blocklist.  This runs regardless of which pipeline
    generated the quote (diary export vs. non-diary extraction), so adding a
    cite key to ``data/excluded-works.json`` and re-running this script is
    enough to pull the quotes offline even without a full re-export.
    """
    stats = {
        "processed": 0, "modified": 0, "skipped_no_blockquote": 0,
        "already_clean": 0, "excluded_removed": 0,
    }

    if not QUOTES_DIR.exists():
        print(f"  WARNING: {QUOTES_DIR} does not exist")
        return stats

    excluded_slugs = {cite_key_to_slug(k) for k in load_excluded_works().keys()}

    md_files = sorted(QUOTES_DIR.glob("*.md"))
    for md_file in md_files:
        if md_file.name == "_index.md":
            continue

        content = md_file.read_text()
        stats["processed"] += 1

        # Split front matter from content using regex to correctly handle
        # delimiters that may also appear in the body content.
        fm_match = re.match(
            r"(\+\+\+\n.*?\n\+\+\+|\-\-\-\n.*?\n\-\-\-)(.*)",
            content, re.DOTALL,
        )
        if not fm_match:
            continue
        front_matter = fm_match.group(1)
        body = fm_match.group(2)

        # Takedown: drop any quote whose work is excluded.
        work_slug = _work_slug_from_front_matter(front_matter)
        if work_slug and work_slug in excluded_slugs:
            stats["excluded_removed"] += 1
            if dry_run:
                print(f"  [EXCLUDE] {md_file.name} (work={work_slug})")
            else:
                safe_remove(md_file)
            continue

        # Check if body has a blockquote
        if ">" not in body:
            stats["skipped_no_blockquote"] += 1
            continue

        # ox-hugo-exported quotes are diary quotes unless they were extracted
        # separately by extract-non-diary-quotes.py, which writes diary = false
        # explicitly. Missing diary metadata should therefore default to true.
        front_matter = ensure_diary_flag(front_matter, is_toml=content.startswith("+++"))

        cleaned_body = strip_citation_from_content(body.lstrip("\n"))
        new_content = front_matter + "\n" + cleaned_body

        if new_content == content:
            stats["already_clean"] += 1
            continue

        stats["modified"] += 1
        if not dry_run:
            atomic_write_text(md_file, new_content)

    return stats


# === Work page generation ===


def clean_work_field(value: str) -> str:
    """Strip BibTeX braces, unescape special chars, and convert org italic."""
    value = value.replace("{", "").replace("}", "")
    # Convert BibTeX escape sequences to plain characters
    value = value.replace("\\&", "&")
    value = value.replace("\\%", "%")
    value = value.replace("\\$", "$")
    value = value.replace("\\_", "_")
    value = value.replace("\\#", "#")
    # Convert org-mode /italic/ markup to HTML <em>.
    # Require at least 2 chars between slashes to avoid matching URL path segments.
    return re.sub(r'(?<!\w)/([^/\n]{2,})/(?!\w)', r'<em>\1</em>', value)


def work_metadata(entry: dict, *, excluded: bool = False,
                  excluded_reason: str = "") -> dict:
    """Return template-facing metadata for a BibTeX entry.

    When *excluded* is True, sets ``excluded = true`` in the output so
    Hugo templates can render the citation text without a hyperlink.
    ``external_url`` is suppressed for excluded entries (no off-site link either).
    """
    title = clean_work_field(entry["title"])
    shorttitle = clean_work_field(entry.get("shorttitle", ""))
    if not shorttitle and title and ":" in title:
        shorttitle = title.split(":")[0].strip()

    author_raw = entry["author"] or entry["editor"]
    editor_raw = entry.get("editor", "")
    raw_date = entry.get("date", "")
    pub_date = raw_date[:10] if re.match(r"\d{4}-\d{2}-\d{2}", raw_date) else ""

    external_url = "" if excluded else entry.get("url", "").replace("{", "").replace("}", "")

    bookauthor_raw = entry.get("bookauthor", "")
    meta = {
        "title": title,
        "author": bib_author_to_display(author_raw),
        "entry_type": entry.get("entry_type", "book"),
        "shorttitle": shorttitle,
        "year": entry.get("year", ""),
        "location": clean_work_field(entry.get("location", "")),
        "booktitle": clean_work_field(entry.get("booktitle", "")),
        "journaltitle": clean_work_field(entry.get("journaltitle", "")),
        "volume": clean_work_field(entry.get("volume", "")),
        "number": clean_work_field(entry.get("number", "")),
        "pages": clean_work_field(entry.get("pages", "")),
        "editor": bib_author_to_display(editor_raw) if editor_raw else "",
        "bookauthor": bib_author_to_display(bookauthor_raw) if bookauthor_raw else "",
        "external_url": external_url,
        "pub_date": pub_date,
    }
    if excluded:
        meta["excluded"] = True
        if excluded_reason:
            meta["excluded_reason"] = excluded_reason
    return meta


def generate_work_page(entry: dict) -> str:
    """Generate YAML front matter for a work page."""
    title = clean_work_field(entry["title"])
    # Compute short title: use BibTeX shorttitle if present, otherwise
    # truncate at the first colon (the standard subtitle separator)
    shorttitle = clean_work_field(entry.get("shorttitle", ""))
    if not shorttitle and title and ":" in title:
        shorttitle = title.split(":")[0].strip()
    author_raw = entry["author"] or entry["editor"]
    author = bib_author_to_display(author_raw)
    year = entry["year"]
    location = clean_work_field(entry.get("location", ""))
    entry_type = entry.get("entry_type", "book")
    booktitle = clean_work_field(entry.get("booktitle", ""))
    journaltitle = clean_work_field(entry.get("journaltitle", ""))
    volume = clean_work_field(entry.get("volume", ""))
    number = clean_work_field(entry.get("number", ""))
    pages = clean_work_field(entry.get("pages", ""))
    editor_raw = entry.get("editor", "")
    editor = bib_author_to_display(editor_raw) if editor_raw else ""
    bookauthor_raw = entry.get("bookauthor", "")
    bookauthor = bib_author_to_display(bookauthor_raw) if bookauthor_raw else ""
    url = entry.get("url", "").replace("{", "").replace("}", "")
    # Extract full date (YYYY-MM-DD) for rich formatting; year-only handled by 'year'
    raw_date = entry.get("date", "")
    pub_date = ""
    if re.match(r"\d{4}-\d{2}-\d{2}", raw_date):
        pub_date = raw_date[:10]

    lines = [
        "---",
        f'title: "{escape_yaml_string(title)}"',
        f'author: "{escape_yaml_string(author)}"',
        f'entry_type: "{entry_type}"',
    ]
    if shorttitle:
        lines.append(f'shorttitle: "{escape_yaml_string(shorttitle)}"')
    if year:
        lines.append(f'year: "{escape_yaml_string(year)}"')
    if location:
        lines.append(f'location: "{escape_yaml_string(location)}"')
    if booktitle:
        lines.append(f'booktitle: "{escape_yaml_string(booktitle)}"')
    if journaltitle:
        lines.append(f'journaltitle: "{escape_yaml_string(journaltitle)}"')
    if volume:
        lines.append(f'volume: "{escape_yaml_string(volume)}"')
    if number:
        lines.append(f'number: "{escape_yaml_string(number)}"')
    if pages:
        lines.append(f'pages: "{escape_yaml_string(pages)}"')
    if editor:
        lines.append(f'editor: "{escape_yaml_string(editor)}"')
    if bookauthor:
        lines.append(f'bookauthor: "{escape_yaml_string(bookauthor)}"')
    if url:
        lines.append(f'external_url: "{escape_yaml_string(url)}"')
    if pub_date:
        lines.append(f'pub_date: "{pub_date}"')
    lines.append("---")
    lines.append("")

    # Add abstract as page body content if available
    abstract = clean_work_field(entry.get("abstract", "")).strip()
    if abstract:
        lines.append(abstract)
        lines.append("")

    return "\n".join(lines)


def ensure_diary_flag(front_matter: str, *, is_toml: bool) -> str:
    """Ensure exported quote front matter has an explicit diary = true flag."""
    diary_re = r"^diary\s*=" if is_toml else r"^diary\s*:"
    if re.search(diary_re, front_matter, re.MULTILINE):
        return front_matter

    suffix = "\n+++" if is_toml else "\n---"
    diary_line = "diary = true" if is_toml else "diary: true"
    if not front_matter.endswith(suffix):
        return front_matter
    return front_matter[:-len(suffix)] + f"\n{diary_line}{suffix}"


def _work_entry_preference(entry: dict) -> tuple:
    """Return a tuple ranking competing bib entries for the same work slug."""
    populated_fields = sum(
        bool(entry.get(field))
        for field in (
            "author",
            "editor",
            "shorttitle",
            "location",
            "booktitle",
            "journaltitle",
            "volume",
            "number",
            "pages",
            "url",
            "date",
            "abstract",
        )
    )
    return (
        int(entry.get("entry_type") in COLLECTION_LIKE_TYPES and bool(entry.get("editor"))),
        int(bool(entry.get("author"))),
        int(bool(entry.get("editor"))),
        populated_fields,
        len(entry.get("title", "")),
        entry.get("_source_index", -1),
        entry.get("cite_key", ""),
    )


def select_canonical_work_entry(entries: list[dict], work_path: Path | None = None) -> dict:
    """Choose the canonical entry for a slug shared by multiple cite keys."""
    if work_path and work_path.exists():
        existing_text = work_path.read_text()
        for entry in entries:
            if generate_work_page(entry) == existing_text:
                return entry
    return max(entries, key=_work_entry_preference)


def build_work_entry_map(bib_by_key: dict) -> tuple[dict[str, dict], dict[str, list[str]]]:
    """Collapse cite-key aliases that normalize to the same work slug."""
    grouped = {}
    for entry in bib_by_key.values():
        slug = cite_key_to_slug(entry["cite_key"])
        grouped.setdefault(slug, []).append(entry)

    slug_to_entry = {}
    collisions = {}
    for slug, entries in sorted(grouped.items()):
        work_path = WORKS_DIR / f"{slug}.md"
        slug_to_entry[slug] = select_canonical_work_entry(entries, work_path)
        if len(entries) > 1:
            collisions[slug] = sorted(entry["cite_key"] for entry in entries)

    return slug_to_entry, collisions


def generate_work_pages(bib_by_key: dict, dry_run: bool = False, limit: int = 0) -> dict:
    """Generate or update work pages for every bib entry.

    Cite keys listed in ``data/excluded-works.json`` are treated as under
    takedown: their work pages are never written (and any existing page is
    removed), though their metadata is still emitted to ``data/works.json``
    with ``excluded = true`` so citation templates can render the reference
    text without a hyperlink.
    """
    stats = {"created": 0, "updated": 0, "unchanged": 0, "excluded_removed": 0}

    if not WORKS_DIR.exists():
        WORKS_DIR.mkdir(parents=True, exist_ok=True)

    excluded = load_excluded_works()
    excluded_slug_to_entry = {
        cite_key_to_slug(key): {"cite_key": key, "reason": info.get("reason", "")}
        for key, info in excluded.items()
    }

    slug_to_entry, collisions = build_work_entry_map(bib_by_key)
    excluded_slugs_present = {
        slug for slug in slug_to_entry
        if slug_to_entry[slug]["cite_key"] in excluded
    }
    all_slugs = set(slug_to_entry.keys())
    publishable_slugs = sorted(all_slugs - excluded_slugs_present)
    print(f"  {len(all_slugs)} unique slugs from bib files")
    if excluded_slugs_present:
        print(f"  {len(excluded_slugs_present)} slug(s) withheld by takedown blocklist")
    if collisions:
        print(f"  Resolved {len(collisions)} slug collision(s) via canonical entry selection")
        for slug, keys in list(collisions.items())[:10]:
            chosen = slug_to_entry[slug]["cite_key"]
            aliases = ", ".join(key for key in keys if key != chosen)
            print(f"    {slug}: {chosen}" + (f" (aliases: {aliases})" if aliases else ""))

    if not limit:
        metadata = {}
        for slug, entry in sorted(slug_to_entry.items()):
            is_excluded = slug in excluded_slugs_present
            reason = (
                excluded_slug_to_entry.get(slug, {}).get("reason", "")
                if is_excluded else ""
            )
            metadata[slug] = work_metadata(entry, excluded=is_excluded,
                                           excluded_reason=reason)
        if dry_run:
            print(f"  [DRY RUN] Would write {len(metadata)} work metadata entries")
        else:
            WORK_METADATA_PATH.parent.mkdir(parents=True, exist_ok=True)
            atomic_write_json(WORK_METADATA_PATH, metadata, ensure_ascii=False)
            print(f"  Wrote {len(metadata)} work metadata entries to data/works.json")

    processed = 0
    for slug in publishable_slugs:
        if limit and processed >= limit:
            break

        work_path = WORKS_DIR / f"{slug}.md"
        entry = slug_to_entry[slug]
        page_content = generate_work_page(entry)

        if work_path.exists():
            if work_path.read_text() == page_content:
                stats["unchanged"] += 1
                continue
            stats["updated"] += 1
            if dry_run:
                if processed < 5:
                    print(f"  [UPDATE] {slug}.md")
            else:
                atomic_write_text(work_path, page_content)
        else:
            stats["created"] += 1
            if dry_run:
                if processed < 5:
                    print(f"  [CREATE] {slug}.md")
            else:
                atomic_write_text(work_path, page_content)

        processed += 1

        if processed % 2000 == 0:
            print(f"  ... {processed} work pages processed")

    # Remove any existing work page for a newly-excluded slug.  Runs under
    # --limit too so takedowns always take effect on the next run.
    if WORKS_DIR.exists():
        for slug in sorted(excluded_slugs_present):
            work_path = WORKS_DIR / f"{slug}.md"
            if work_path.exists():
                stats["excluded_removed"] += 1
                if dry_run:
                    print(f"  [EXCLUDE] {slug}.md (takedown)")
                else:
                    safe_remove(work_path)

    # Remove stale work pages with no matching bib entry
    stats["removed"] = 0
    if WORKS_DIR.exists() and not limit:
        for work_file in sorted(WORKS_DIR.glob("*.md")):
            if work_file.name == "_index.md":
                continue
            slug = work_file.stem
            if slug not in all_slugs:
                stats["removed"] += 1
                if dry_run:
                    if stats["removed"] <= 5:
                        print(f"  [REMOVE] {slug}.md")
                else:
                    safe_remove(work_file)

    return stats


# === Main ===


def main():
    parser = argparse.ArgumentParser(
        description="Generate work pages and post-process quote markdown"
    )
    parser.add_argument("--dry-run", action="store_true", help="Preview without writing")
    parser.add_argument("--limit", type=int, default=0, help="Limit work pages to generate")
    parser.add_argument("--skip-postprocess", action="store_true", help="Skip citation stripping")
    parser.add_argument("--skip-works", action="store_true", help="Skip work page generation")
    args = parser.parse_args()

    # Step 1: Post-process quotes (strip citations)
    if not args.skip_postprocess:
        print("=" * 60)
        print("STEP 1: Post-processing quote markdown files")
        print("=" * 60)
        pp_stats = postprocess_quotes(dry_run=args.dry_run)
        print(f"  Files processed:  {pp_stats['processed']}")
        print(f"  Files modified:   {pp_stats['modified']}")
        print(f"  Already clean:    {pp_stats['already_clean']}")
        print(f"  No blockquote:    {pp_stats['skipped_no_blockquote']}")
        print(f"  Takedown removed: {pp_stats['excluded_removed']}")
        if args.dry_run:
            print("  *** DRY RUN — no files modified ***")
        print()

    # Step 2: Generate work pages
    if not args.skip_works:
        print("=" * 60)
        print("STEP 2: Generating work pages")
        print("=" * 60)

        # Parse all bib files
        print("\n  Parsing bib files...")
        bib_by_key = {}
        for source_index, bib_path in enumerate(BIB_FILES):
            if not bib_path.exists():
                print(f"  WARNING: {bib_path} not found, skipping")
                continue
            entries = _parse_bib_entries_for_works(bib_path)
            for entry in entries:
                entry["_source_index"] = source_index
                bib_by_key[entry["cite_key"]] = entry
            print(f"    {bib_path.name}: {len(entries)} entries")
        print(f"  Total unique keys: {len(bib_by_key)}")

        # Resolve crossref inheritance
        resolve_crossrefs(bib_by_key)
        print("  Crossrefs resolved")

        wp_stats = generate_work_pages(bib_by_key, dry_run=args.dry_run, limit=args.limit)
        print(f"\n  Work pages created:    {wp_stats['created']}")
        print(f"  Work pages updated:    {wp_stats['updated']}")
        print(f"  Unchanged:             {wp_stats['unchanged']}")
        print(f"  Stale pages removed:   {wp_stats['removed']}")
        print(f"  Takedown pages removed:{wp_stats['excluded_removed']}")
        if args.dry_run:
            print("  *** DRY RUN — no files created ***")

    print("\nDone.")


if __name__ == "__main__":
    main()
