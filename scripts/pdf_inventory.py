"""Current bibliography attachment eligibility, shared by processing and display."""

import json
from pathlib import Path

from lib import (
    BIB_FILES, REPO_ROOT, build_unique_slug_map, cite_key_to_slug,
    extract_pdf_path, load_excluded_works, parse_bib_entries,
)

DB_BIB = Path.home() / "repos/babel-refs/bib/db.bib"

# Entry types treated as "books" for copyright-filtering purposes.
# PDFs from these types are excluded when published in 2000 or later.
# See docs/pdf-hosting-policy.md.
BOOK_LIKE_TYPES = frozenset({
    "book", "collection", "reference",
    "mvbook", "mvcollection", "mvreference",
})
# Books published from 2000 onward are assumed to be under active copyright.
BOOK_YEAR_CUTOFF = 2000


def _is_excluded_book(entry: dict) -> bool:
    """Return True if *entry* is a book-like type published in 2000 or later."""
    if entry.get("entry_type", "") not in BOOK_LIKE_TYPES:
        return False
    year_str = entry.get("year", "").strip()
    if not year_str:
        return True  # unknown year — be conservative
    try:
        return int(year_str) >= BOOK_YEAR_CUTOFF
    except ValueError:
        return True  # unparseable year — be conservative


def resolve_crossref_pdfs(entries_by_key: dict) -> int:
    """Inherit ``file`` from crossref parents for entries that lack their own.

    Skips inheritance when the parent is a book-like entry published in
    2000 or later (see :func:`_is_excluded_book`).

    Returns the number of entries that inherited a PDF.
    """
    count = 0
    for entry in entries_by_key.values():
        crossref_key = entry.get("crossref", "").strip()
        if not crossref_key:
            continue
        if entry.get("file", "").strip():
            continue  # already has own PDF
        parent = entries_by_key.get(crossref_key)
        if not parent:
            continue
        if _is_excluded_book(parent):
            continue  # don't inherit from a copyright-restricted book
        parent_file = parent.get("file", "").strip()
        if parent_file:
            entry["file"] = parent_file
            count += 1
    return count


def collect_entries(*, bib_files=None, db_bib=None) -> list[dict]:
    """Parse all bib files and return entries with PDF paths.

    Resolves crossref inheritance: entries without a ``file`` field inherit
    from their crossref parent when available.

    Skips db.bib entries that have a non-empty ``translator`` field.
    """
    bib_files = BIB_FILES if bib_files is None else bib_files
    db_bib = DB_BIB if db_bib is None else db_bib
    # Step 1: Parse ALL entries from ALL bib files into a single dict
    entries_by_key = {}
    missing = [p for p in bib_files if not p.exists()]
    if missing:
        # A missing source silently narrows which PDFs are considered
        # hostable, so the run would look clean while dropping work.
        raise SystemExit(
            "  ERROR: bibliography source(s) missing, refusing to run:\n"
            + "\n".join(f"    {p}" for p in missing)
            + "\n  Fix the paths in scripts/lib.py:BIB_FILES, or restore the files."
        )
    for bib_path in bib_files:

        is_db = bib_path.resolve() == db_bib.resolve()

        parsed = parse_bib_entries(
            bib_path,
            strip_braces=False,
            extra_fields=["file", "translator", "crossref"],
        )

        for entry in parsed:
            # Skip translated works from db.bib
            if is_db and entry.get("translator", "").strip():
                continue
            entries_by_key[entry["cite_key"]] = entry

    # Step 2: Resolve crossrefs — inherit file from parent entries
    inherited = resolve_crossref_pdfs(entries_by_key)
    if inherited:
        print(f"  Inherited PDF from crossref parent for {inherited} entries")

    # Step 3: Filter for entries with a file field and build output list.
    # Exclude book-like entries published in 2000+ (copyright policy).
    # Also exclude cite keys in the takedown blocklist.
    excluded_takedown_keys = set(load_excluded_works().keys())
    entries = []
    excluded_books = 0
    excluded_takedown = 0
    for entry in entries_by_key.values():
        file_field = entry.get("file", "")
        if not file_field:
            continue

        if entry["cite_key"] in excluded_takedown_keys:
            excluded_takedown += 1
            continue

        if _is_excluded_book(entry):
            excluded_books += 1
            continue

        pdf_path = extract_pdf_path(file_field)
        if pdf_path is None:
            continue

        entries.append({
            "cite_key": entry["cite_key"],
            "slug": cite_key_to_slug(entry["cite_key"]),
            "pdf_path": pdf_path,
        })

    if excluded_books:
        print(f"  Excluded {excluded_books} book-like PDFs (published {BOOK_YEAR_CUTOFF}+)")
    if excluded_takedown:
        print(f"  Excluded {excluded_takedown} PDFs via takedown blocklist")

    return entries


def available_pdf_slugs(repo_root=REPO_ROOT) -> set[str]:
    """Return current eligible attachments with matching generated assets.

    Old generated files never establish bibliography membership. A manifest
    record must identify the current source, so changing an attachment also
    hides the previous PDF until processing has generated its replacement.
    The original may be offline: the current attachment path and generated
    assets establish identity without requiring access to the original.
    Missing required inventories abort rather than publishing an empty map.
    """
    repo_root = Path(repo_root)
    pdfs = repo_root / "static" / "pdfs"
    thumbs = repo_root / "static" / "pdf-thumbnails"
    manifest_path = pdfs / ".manifest.json"
    for directory in (pdfs, thumbs):
        if not directory.is_dir():
            raise SystemExit(f"ERROR: required PDF asset directory missing: {directory}")
    if not manifest_path.is_file():
        raise SystemExit(f"ERROR: required PDF manifest missing: {manifest_path}")
    manifest = json.loads(manifest_path.read_text())
    if not isinstance(manifest, dict):
        raise ValueError(f"PDF manifest must be an object: {manifest_path}")
    entries = collect_entries()
    slug_to_key = build_unique_slug_map(entry["cite_key"] for entry in entries)
    entries_by_key = {entry["cite_key"]: entry for entry in entries}
    available = set()
    for slug, key in slug_to_key.items():
        source = Path(entries_by_key[key]["pdf_path"]).expanduser().resolve()
        record = manifest.get(slug, {})
        if not isinstance(record, dict):
            raise ValueError(f"Invalid PDF manifest record: {slug}")
        recorded_source = record.get("src_path")
        if not recorded_source:
            continue
        if Path(recorded_source).expanduser().resolve() != source:
            continue
        if (pdfs / f"{slug}.pdf").is_file() and (thumbs / f"{slug}.png").is_file():
            available.add(slug)
    return available
