#!/usr/bin/env python3
"""Inspect missing book PDFs using the shared paper-fetch acquisition library.

This legacy website entry point no longer downloads blindly or writes BibTeX.
--dry-run discovers candidates for existing old.bib entries. --candidates plus
--reviews reports a byte-bound selection for the Zotra/Ebib attachment workflow.
Default runs and legacy --update-bib stop with needs-review before any service or
credential access. See docs/book-acquisition.md and the dotfiles shared policy.
"""

from __future__ import annotations

import argparse
import importlib.util
import json
import os
import re
import sys
from pathlib import Path

from lib import extract_pdf_path, parse_bib_entries

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

BIB_FILE = Path.home() / "My Drive" / "bibliography" / "old.bib"


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


def discover_existing_book(http, book: dict, host: str) -> dict:
    """Inspect existing metadata; an absent edition is never assumed to mean first."""
    isbn = re.sub(r"-", "", re.split(r"[\s;,]+", book.get("isbn", "").strip())[0])
    query = isbn or " ".join(filter(None, (book["title"], book["author"])))
    if not query:
        return {"key": book["key"], "status": "needs-review", "candidates": [],
                "message": "Entry lacks a search identifier, title and author"}
    attempts, candidates = [], {}
    routes = [("annas-book-search", lambda: paper_fetch.search_annas_books(http, query, host))]
    if isbn:
        routes.insert(0, ("libgen-isbn", lambda: paper_fetch.libgen_book_results(http, isbn, strict=True)))
    for route, lookup in routes:
        try:
            records = lookup()
            attempts.append({"route": route, "status": "ok", "count": len(records)})
            for record in records:
                if record["md5"] in candidates:
                    candidates[record["md5"]]["observations"].append(record)
                else:
                    candidates[record["md5"]] = {**record, "observations": [record]}
        except paper_fetch.PaperFetchError as exc:
            attempts.append({"route": route, "status": "unknown", "error": str(exc)})
        except Exception as exc:
            attempts.append({"route": route, "status": "unknown", "error": type(exc).__name__})
    return {"key": book["key"], "metadata": book, "query": query, "status": "needs-review",
            "candidates": list(candidates.values()), "attempts": attempts,
            "search_complete": all(attempt["status"] == "ok" for attempt in attempts)}


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--dry-run", action="store_true", help="discover candidates without downloading or writing")
    parser.add_argument("--candidates", help="paper-fetch candidate inventory for one exact edition")
    parser.add_argument("--reviews", help="paper-fetch review manifest for that inventory")
    parser.add_argument("--only", default="", help="inspect only this existing citekey")
    parser.add_argument("--limit", type=int, default=0)
    parser.add_argument("--include-broken", action="store_true")
    parser.add_argument("--base-url", default="")
    parser.add_argument("--update-bib", action="store_true", help="retired: attachments must go through Ebib")
    # Accept old scheduler flags so existing jobs receive a useful refusal without
    # reading progress, modifying it, or echoing an old credential argument.
    parser.add_argument("--resume", action="store_true", help=argparse.SUPPRESS)
    parser.add_argument("--retry-errors", action="store_true", help=argparse.SUPPRESS)
    parser.add_argument("--retry-not-found", action="store_true", help=argparse.SUPPRESS)
    parser.add_argument("--delay", type=float, default=0, help=argparse.SUPPRESS)
    parser.add_argument("--key", default="", help=argparse.SUPPRESS)
    parser.add_argument("--verbose", action="store_true", help=argparse.SUPPRESS)
    args = parser.parse_args(argv)

    if args.update_bib or (not args.dry_run and not args.reviews):
        print(json.dumps({"status": "needs-review", "message":
            "Blind downloads and direct BibTeX updates are retired. Inspect with --dry-run; "
            "use paper-fetch book-candidates/book-stage/book-inspect, then supply --candidates and --reviews. "
            "Attach the selected reviewed file through Zotra/Ebib."}))
        return 5
    try:
        if args.reviews or args.candidates:
            if not (args.reviews and args.candidates) or args.dry_run:
                raise paper_fetch.PaperFetchError("Use --candidates and --reviews together, separately from --dry-run")
            manifest = paper_fetch.read_book_candidates(Path(args.candidates))
            reviews = json.loads(Path(args.reviews).read_text())
            result = paper_fetch.select_book_candidate(manifest, reviews)
            print(json.dumps(result, indent=2))
            return 0 if result["status"] == "ok" else 5
        books = books_missing_pdf(parse_bib_books(BIB_FILE), include_broken=args.include_broken)
        if args.only:
            books = [book for book in books if book["key"] == args.only]
        if args.limit < 0:
            raise paper_fetch.PaperFetchError("--limit must be nonnegative")
        if args.limit:
            books = books[:args.limit]
        http = paper_fetch.Http()
        host = paper_fetch.resolve_annas_hosts(http, args.base_url)[0]
        results = [discover_existing_book(http, book, host) for book in books]
        print(json.dumps({"status": "needs-review" if results else "ok", "books": results,
                          "message": "Discovery only; no edition, attachment or pagination has been approved."}, indent=2))
        return 5 if results else 0
    except (paper_fetch.PaperFetchError, OSError, ValueError) as exc:
        message = str(exc) if isinstance(exc, paper_fetch.PaperFetchError) else type(exc).__name__
        print(json.dumps({"status": "error", "message": message}), file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
