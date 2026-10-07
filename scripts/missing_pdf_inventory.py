"""Inventory existing old.bib books whose PDF is missing.

The nightly batch (download-missing-pdfs-batch.py) uses this module to choose
its queue and to read back attachments. It also exposes the shared paper-fetch
library handle that the batch uses to revalidate retained book reviews.
Candidate discovery and acquisition belong to paper-fetch itself.
"""

from __future__ import annotations

import importlib.util
import os
import sys
from pathlib import Path

from lib import extract_pdf_path, parse_bib_entries

__all__ = ["paper_fetch", "parse_bib_books", "books_missing_pdf", "extract_pdf_path"]

sys.dont_write_bytecode = True

_PAPER_FETCH_LIB = Path(
    os.environ.get("DOTFILES_DIR", str(Path.home() / "My Drive" / "dotfiles"))
) / "lib" / "python" / "paper_fetch.py"
_spec = importlib.util.spec_from_file_location("paper_fetch", _PAPER_FETCH_LIB)
if _spec is None or _spec.loader is None:
    raise SystemExit(f"cannot load shared paper library: {_PAPER_FETCH_LIB}")
paper_fetch = sys.modules.get("paper_fetch") or importlib.util.module_from_spec(_spec)
if "paper_fetch" not in sys.modules:
    sys.modules["paper_fetch"] = paper_fetch
    _spec.loader.exec_module(paper_fetch)


def parse_bib_books(bib_path: Path) -> list[dict]:
    """Read existing book entries without changing their metadata."""
    entries = parse_bib_entries(
        bib_path, strip_braces=False,
        extra_fields=["isbn", "date", "crossref", "file", "edition", "langid"],
        field_fallbacks={"author": "editor"},
    )
    return [{"key": entry["cite_key"], "title": entry["title"], "author": entry["author"],
             "isbn": entry["isbn"], "date": entry["date"] or entry["year"],
             "crossref": entry["crossref"], "file": entry["file"],
             "edition": entry["edition"], "language": entry["langid"]}
            for entry in entries if entry["entry_type"] == "book"]


def books_missing_pdf(books: list[dict], *, include_broken: bool = False) -> list[dict]:
    """Find missing PDFs without treating another attachment format as a PDF."""
    missing = []
    for book in books:
        pdf = extract_pdf_path(book["file"])
        if pdf is None or (include_broken and not pdf.is_file()):
            missing.append(book)
    return missing
