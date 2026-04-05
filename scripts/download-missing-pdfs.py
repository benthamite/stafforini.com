#!/usr/bin/env python3
"""Download missing book PDFs from Anna's Archive.

Parses old.bib to find @book entries that lack a PDF file, searches
Anna's Archive for each, and downloads the best matching PDF via
the fast download API.

Selection criteria (per the user's preference):
  1. The PDF must reflect the physical book's pages and font — i.e.
     a publisher-quality digital edition or a scan, NOT an ebook
     (epub/mobi) that was converted to PDF.
  2. Subject to (1), the file should be as small as possible.

Usage:
    python3 scripts/download-missing-pdfs.py [OPTIONS]

Options:
    --dry-run           Search and rank results but don't download
    --limit N           Process at most N books (for testing)
    --resume            Skip books already in the progress file
    --key KEY           Anna's Archive secret key (default: from
                        ~/.claude.json MCP config)
    --base-url URL      Anna's Archive base URL
                        (default: https://annas-archive.gl/)
    --delay SECS        Delay between searches (default: 3)
    --include-broken    Also re-download books whose file field
                        points to a non-existent path
    --only KEY          Process only the book with this citekey
    --update-bib        Add file fields to old.bib for downloaded PDFs
    --verbose           Print extra debugging info
"""

from __future__ import annotations

import argparse
import json
import os
import re
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path

from lib import parse_bib_entries

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------

BIB_FILE = Path.home() / "My Drive" / "bibliography" / "old.bib"
LIBRARY_DIR = Path.home() / "My Drive" / "library-pdf"
PROGRESS_FILE = Path(__file__).resolve().parent / "download-missing-pdfs-progress.json"

# ---------------------------------------------------------------------------
# Bib parsing
# ---------------------------------------------------------------------------


def parse_bib_books(bib_path: Path) -> list[dict]:
    """Return a list of @book entries from a .bib file.

    Each entry is a dict with keys: key, title, author, isbn, date,
    crossref, file.
    """
    books = []
    entries = parse_bib_entries(
        bib_path,
        strip_braces=False,
        extra_fields=["isbn", "date", "crossref", "file"],
        field_fallbacks={"author": "editor"},
    )

    for entry in entries:
        if entry["entry_type"] != "book":
            continue
        books.append(
            {
                "key": entry["cite_key"],
                "title": entry["title"],
                "author": entry["author"] or entry["editor"],
                "isbn": entry.get("isbn", ""),
                "date": entry.get("date", "") or entry["year"],
                "crossref": entry.get("crossref", ""),
                "file": entry.get("file", ""),
            }
        )
    return books


def books_missing_pdf(
    books: list[dict], *, include_broken: bool = False
) -> list[dict]:
    """Filter to books that need a PDF download."""
    missing = []
    for b in books:
        file_val = b["file"]
        if not file_val:
            missing.append(b)
            continue
        if include_broken:
            # Check if all referenced files actually exist
            paths = file_val.split(";")
            all_missing = all(
                not Path(p.strip().replace("~", str(Path.home()))).exists()
                for p in paths
                if p.strip()
            )
            if all_missing:
                b["_broken_ref"] = True
                missing.append(b)
    return missing


# ---------------------------------------------------------------------------
# Anna's Archive search
# ---------------------------------------------------------------------------


def build_search_query(book: dict) -> str:
    """Build the search query for a book using its ISBN.

    Returns the first ISBN (hyphens stripped), or empty string if none.
    """
    isbn = book["isbn"]
    if not isbn:
        return ""
    first_isbn = re.split(r"[\s;,]+", isbn)[0]
    return re.sub(r"-", "", first_isbn)


def search_annas_archive(
    query: str, base_url: str, *, verbose: bool = False
) -> list[dict]:
    """Search Anna's Archive and return parsed results.

    Each result is a dict with keys: md5, title, authors, format, size,
    size_bytes, year, language, source, filename.
    """
    encoded = urllib.parse.quote_plus(query)
    url = f"{base_url}search?q={encoded}&content=book_any"

    if verbose:
        print(f"    Search URL: {url}", file=sys.stderr)

    try:
        html = _fetch_url(url)
    except Exception as e:
        print(f"    Search failed: {e}", file=sys.stderr)
        return []

    return _parse_search_results(html)


def _fetch_url(url: str, timeout: int = 30) -> str:
    """Fetch a URL using urllib with browser-like headers."""
    req = urllib.request.Request(
        url,
        headers={
            "User-Agent": (
                "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
                "AppleWebKit/537.36 (KHTML, like Gecko) "
                "Chrome/120.0.0.0 Safari/537.36"
            ),
            "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
            "Accept-Language": "en-US,en;q=0.5",
        },
    )
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        return resp.read().decode("utf-8", errors="replace")


def _parse_search_results(html: str) -> list[dict]:
    """Parse search result HTML into a list of result dicts."""
    results = []

    # Find unique md5 hashes and their positions
    md5_pattern = re.compile(r'href="/md5/([0-9a-f]+)"')
    seen = set()
    md5_positions = []
    for m in md5_pattern.finditer(html):
        h = m.group(1)
        if h not in seen:
            seen.add(h)
            md5_positions.append((h, m.start()))

    for i, (md5, pos) in enumerate(md5_positions):
        next_pos = (
            md5_positions[i + 1][1] if i + 1 < len(md5_positions) else pos + 5000
        )
        block = html[pos:next_pos]

        # Strip HTML tags to extract text lines
        text = re.sub(r"<[^>]+>", "\n", block)
        lines = [ln.strip() for ln in text.split("\n") if ln.strip()]

        # The metadata line has the format:
        #   English [en] · PDF · 9.5MB · 2022 · 📘 Book (non-fiction) · 🚀/lgli/zlib
        # Fields separated by · (middle dot, U+00B7)
        metadata_line = ""
        for ln in lines:
            if "·" in ln and re.search(r"\d+(?:\.\d+)?\s*[MGK]B", ln):
                metadata_line = ln
                break

        if not metadata_line:
            continue

        tokens = [t.strip() for t in metadata_line.split("·")]

        fmt = ""
        size_str = ""
        year = ""
        language = ""
        source = ""
        filename = ""

        for tok in tokens:
            tok_clean = tok.strip()
            # Format: uppercase 3-4 letter extension
            if re.fullmatch(r"[A-Z]{2,6}", tok_clean):
                fmt = tok_clean.lower()
            # Size: number + unit
            elif re.match(r"\d+(?:\.\d+)?\s*[MGK]B$", tok_clean):
                size_str = tok_clean
            # Year: 4-digit number
            elif re.fullmatch(r"[12]\d{3}", tok_clean):
                year = tok_clean
            # Language: contains [xx] pattern
            elif re.search(r"\[..\]", tok_clean):
                language = tok_clean
            # Source: starts with rocket emoji
            elif "🚀" in tok_clean or tok_clean.startswith("/"):
                source = tok_clean.replace("🚀", "").strip()

        # Extract filename from the first text line (before title)
        for ln in lines:
            if ln.startswith("href="):
                continue
            if "/" in ln and (ln.endswith(".pdf") or ln.endswith(".epub")
                            or ln.endswith(".djvu") or ln.endswith(".mobi")
                            or ln.endswith(".azw3") or ln.endswith(".fb2")
                            or ln.endswith(".cbr") or ln.endswith(".txt")):
                filename = ln
                break
            # Also match filenames without directory path
            if re.search(r"\.\w{2,5}$", ln) and len(ln) < 500:
                filename = ln
                break

        # Extract title (usually the second meaningful text line)
        title = ""
        authors = ""
        meaningful = [
            ln for ln in lines
            if not ln.startswith("href=")
            and "·" not in ln
            and not ln.startswith("window.")
            and not ln.startswith("(function")
            and not ln.startswith("var ")
            and not ln.startswith("Read more")
            and not ln.startswith("Save")
            and not ln.startswith("el.")
            and not ln.startswith("if ")
            and not ln.startswith("}")
            and not ln.startswith("{")
            and "addReadMoreButton" not in ln
            and "aarecord_id" not in ln
            and "leftPos" not in ln
            and "innerStyles" not in ln
            and "scriptParent" not in ln
            and "getBoundingClientRect" not in ln
            and "addEventListener" not in ln
            and "classList" not in ln
            and "clientWidth" not in ln
        ]
        # Skip filename line
        if meaningful and filename and meaningful[0] == filename:
            meaningful = meaningful[1:]
        if meaningful:
            title = meaningful[0]
        if len(meaningful) > 1:
            authors = meaningful[1]

        results.append(
            {
                "md5": md5,
                "title": title,
                "authors": authors,
                "format": fmt,
                "size": size_str,
                "size_bytes": _parse_size(size_str),
                "year": year,
                "language": language,
                "source": source,
                "filename": filename,
            }
        )

    return results


def _parse_size(size_str: str) -> int:
    """Convert '9.5MB' or '120 KB' to bytes."""
    m = re.match(r"([\d.]+)\s*([MGK])B", size_str)
    if not m:
        return 0
    num = float(m.group(1))
    unit = m.group(2)
    multipliers = {"K": 1024, "M": 1024**2, "G": 1024**3}
    return int(num * multipliers.get(unit, 1))


# ---------------------------------------------------------------------------
# Title / author relevance matching
# ---------------------------------------------------------------------------

# Common words to ignore when comparing titles
STOP_WORDS = frozenset(
    "a an and at by de del di el en for from il in la le les"
    " of on or per the to un une vol y".split()
)


def _normalize_text(s: str) -> str:
    """Lowercase, strip accents, remove punctuation."""
    import unicodedata

    s = unicodedata.normalize("NFKD", s.lower())
    s = "".join(c for c in s if not unicodedata.combining(c))
    s = re.sub(r"[^\w\s]", " ", s)
    s = re.sub(r"\s+", " ", s).strip()
    return s


def _significant_words(text: str) -> set[str]:
    """Return the set of non-stop words from normalized text."""
    return {w for w in _normalize_text(text).split() if w not in STOP_WORDS and len(w) > 1}


def title_similarity(target_title: str, result_title: str) -> float:
    """Compute word-overlap similarity between target and result titles.

    Returns a value between 0 and 1 (Jaccard on significant words).
    """
    a = _significant_words(target_title)
    b = _significant_words(result_title)
    if not a or not b:
        return 0.0
    return len(a & b) / len(a | b)


def author_matches(target_author: str, result_authors: str) -> bool:
    """Check if the result's authors field contains the target's first author surname."""
    if not target_author:
        return True  # Can't verify — give benefit of the doubt
    first_author = re.split(r"\s+and\s+", target_author)[0].strip()
    surname = first_author.split(",")[0].strip() if "," in first_author else first_author.split()[-1]
    surname = surname.replace("{", "").replace("}", "")
    norm_surname = _normalize_text(surname)
    norm_result = _normalize_text(result_authors)
    return norm_surname in norm_result


# Minimum title similarity to consider a result relevant
MIN_TITLE_SIMILARITY = 0.25


# ---------------------------------------------------------------------------
# Result scoring
# ---------------------------------------------------------------------------

# Size thresholds (bytes)
MIN_PHYSICAL_PDF_SIZE = 2 * 1024 * 1024  # 2 MB — below this, likely ebook-derived
MAX_REASONABLE_SIZE = 500 * 1024 * 1024   # 500 MB — too large, skip


def is_ebook_derived(result: dict) -> bool:
    """Heuristic: return True if the PDF is likely an ebook conversion."""
    fn = (result.get("filename") or "").lower()
    # Filename contains ebook conversion artifacts
    if any(kw in fn for kw in ["epub", "mobi", "azw", "ebook", "e-book",
                                "calibre", "fb2", "kindle", "nook"]):
        return True
    return False


def score_result(result: dict, target_book: dict | None = None) -> tuple:
    """Return a sort key for ranking results. Lower is better.

    Priority:
      1. Must be PDF
      2. Must match the target book (title/author relevance)
      3. Must not be an ebook-derived PDF
      4. Prefer smaller file (subject to being a plausible physical-page PDF)
    """
    fmt = result.get("format", "")
    size = result.get("size_bytes", 0)

    # Disqualifiers — return very high score
    if fmt != "pdf":
        return (100, size)
    if size > MAX_REASONABLE_SIZE:
        return (90, size)
    if is_ebook_derived(result):
        return (50, size)

    # Relevance check — require title AND/OR author evidence
    if target_book:
        target_title = target_book.get("title", "").strip()
        sim = title_similarity(target_title, result.get("title", ""))
        auth_ok = author_matches(target_book.get("author", ""), result.get("authors", ""))
        has_author = bool(target_book.get("author", "").strip())
        has_title = bool(target_title)

        if not has_title and has_author and auth_ok:
            pass  # fallback when bibliography metadata lacks a parseable title
        # High title similarity — accept on title alone
        elif sim >= 0.5 and (has_author and auth_ok or sim >= 0.7):
            pass  # good match
        # Moderate title similarity + author confirmation — accept
        elif sim >= 0.25 and auth_ok and has_author:
            pass  # acceptable
        # Everything else — reject
        else:
            return (80, size)

    # Prefer physical-page PDFs: penalize very small PDFs
    if size < MIN_PHYSICAL_PDF_SIZE and size > 0:
        return (30, size)

    # Good candidate — rank by size (smaller is better)
    return (0, size)


def select_best_result(
    results: list[dict], target_book: dict | None = None
) -> dict | None:
    """Pick the best PDF from search results."""
    pdfs = [r for r in results if r.get("format") == "pdf"]
    if not pdfs:
        return None

    ranked = sorted(pdfs, key=lambda r: score_result(r, target_book))
    best = ranked[0]

    # Don't return results with high penalty scores
    if score_result(best, target_book)[0] >= 50:
        return None

    return best


# ---------------------------------------------------------------------------
# Download
# ---------------------------------------------------------------------------


class RateLimitError(Exception):
    """Raised when the API returns HTTP 429."""


class QuotaExhaustedError(Exception):
    """Raised when repeated 429s indicate daily quota is exhausted."""


# Track consecutive 429 failures across books to detect quota exhaustion
_consecutive_429s = 0
_QUOTA_EXHAUSTION_THRESHOLD = 3  # 3 consecutive books failing = daily limit hit


def download_via_fast_api(
    md5: str, secret_key: str, base_url: str, dest_path: Path,
    *, verbose: bool = False, max_retries: int = 3
) -> bool:
    """Download a file using Anna's Archive fast download API.

    Retries with exponential backoff on HTTP 429.  Returns True on
    success, False on non-retryable failure.  Raises QuotaExhaustedError
    when repeated failures suggest the daily limit has been hit.
    """
    global _consecutive_429s

    for attempt in range(max_retries + 1):
        try:
            result = _download_once(md5, secret_key, base_url, dest_path,
                                    verbose=verbose)
            _consecutive_429s = 0  # reset on success
            return result
        except RateLimitError:
            if attempt == max_retries:
                _consecutive_429s += 1
                if _consecutive_429s >= _QUOTA_EXHAUSTION_THRESHOLD:
                    raise QuotaExhaustedError(
                        f"Daily download quota appears exhausted "
                        f"({_consecutive_429s} consecutive 429 failures)")
                print("    Rate-limited: retries exhausted for this book",
                      file=sys.stderr)
                return False
            wait = 30 * (2 ** attempt)  # 30s, 60s, 120s
            print(f"    Rate-limited (429). Waiting {wait}s before retry "
                  f"{attempt + 2}/{max_retries + 1}...", file=sys.stderr)
            time.sleep(wait)
    return False


def _download_once(
    md5: str, secret_key: str, base_url: str, dest_path: Path,
    *, verbose: bool = False
) -> bool:
    """Single download attempt.  Raises RateLimitError on 429."""
    api_url = (
        f"{base_url}dyn/api/fast_download.json"
        f"?md5={urllib.parse.quote(md5)}"
        f"&key={urllib.parse.quote(secret_key)}"
        f"&path_index=0&domain_index=0"
    )

    if verbose:
        print(f"    API URL: {api_url[:80]}...", file=sys.stderr)

    try:
        req = urllib.request.Request(
            api_url,
            headers={"Accept": "application/json"},
        )
        with urllib.request.urlopen(req, timeout=30) as resp:
            data = json.loads(resp.read())
    except urllib.error.HTTPError as e:
        if e.code == 429:
            raise RateLimitError()
        print(f"    Fast download API failed: {e}", file=sys.stderr)
        return False
    except Exception as e:
        print(f"    Fast download API failed: {e}", file=sys.stderr)
        return False

    download_url = data.get("download_url")
    error = data.get("error")

    if error:
        print(f"    API error: {error}", file=sys.stderr)
        return False

    if not download_url:
        print("    No download URL returned", file=sys.stderr)
        return False

    if verbose:
        print(f"    Download URL: {download_url[:80]}...", file=sys.stderr)

    # Download the actual file
    try:
        req = urllib.request.Request(
            download_url,
            headers={
                "User-Agent": (
                    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
                    "AppleWebKit/537.36 (KHTML, like Gecko) "
                    "Chrome/120.0.0.0 Safari/537.36"
                ),
            },
        )
        with urllib.request.urlopen(req, timeout=300) as resp:
            content = resp.read()

            # Check if we got HTML instead of a PDF (DDoS Guard challenge)
            if content[:20].lstrip().startswith((b"<!DOCTYPE", b"<html", b"<HTML")):
                print("    Got HTML instead of PDF (DDoS challenge?)", file=sys.stderr)
                return False

            dest_path.parent.mkdir(parents=True, exist_ok=True)
            dest_path.write_bytes(content)

    except Exception as e:
        print(f"    Download failed: {e}", file=sys.stderr)
        # Clean up partial file
        if dest_path.exists():
            dest_path.unlink()
        return False

    return True


# ---------------------------------------------------------------------------
# Progress tracking
# ---------------------------------------------------------------------------


def load_progress(path: Path) -> dict:
    """Load progress state from JSON file."""
    if path.exists():
        return json.loads(path.read_text())
    return {"downloaded": {}, "not_found": [], "errors": [], "skipped": []}


def save_progress(path: Path, progress: dict) -> None:
    """Save progress state to JSON file."""
    path.write_text(json.dumps(progress, indent=2, ensure_ascii=False))


# ---------------------------------------------------------------------------
# Bib update
# ---------------------------------------------------------------------------


def update_bib_entry(bib_path: Path, key: str) -> bool:
    """Add a file field to a single @book entry.  Returns True if updated."""
    content = bib_path.read_text(encoding="utf-8")
    expected_path = f"~/My Drive/library-pdf/{key}.pdf"

    pattern = re.compile(
        rf"(@book\{{{re.escape(key)},.*?)(^\}})",
        re.DOTALL | re.MULTILINE,
    )
    m = pattern.search(content)
    if not m:
        return False

    entry_text = m.group(1)
    if re.search(r"^\s*file\s*=", entry_text, re.MULTILINE):
        return False

    # Ensure the last field before the closing brace has a trailing comma
    entry_text_end = content[: m.start(2)]
    entry_text_end = re.sub(r"([^\s,])([ \t]*\n)$", r"\1,\2", entry_text_end)
    file_line = f"\tfile = {{{expected_path}}}\n"
    content = entry_text_end + file_line + content[m.start(2) :]
    bib_path.write_text(content, encoding="utf-8")
    return True


def update_bib_file(bib_path: Path, progress: dict) -> int:
    """Add file fields to bib entries for downloaded PDFs.

    Returns the number of entries updated.
    """
    updated = 0
    for key in progress.get("downloaded", {}):
        if update_bib_entry(bib_path, key):
            updated += 1
    return updated


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------


def get_secret_key() -> str:
    """Read the Anna's Archive secret key from ~/.claude.json."""
    claude_json = Path.home() / ".claude.json"
    if not claude_json.exists():
        return ""
    try:
        data = json.loads(claude_json.read_text())
        return (
            data.get("mcpServers", {})
            .get("annas-archive", {})
            .get("env", {})
            .get("ANNAS_SECRET_KEY", "")
        )
    except (json.JSONDecodeError, KeyError):
        return ""


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Download missing book PDFs from Anna's Archive"
    )
    parser.add_argument("--dry-run", action="store_true",
                        help="Search and rank but don't download")
    parser.add_argument("--limit", type=int, default=0,
                        help="Process at most N books")
    parser.add_argument("--resume", action="store_true",
                        help="Skip books already in the progress file")
    parser.add_argument("--key", default="",
                        help="Anna's Archive secret key")
    parser.add_argument("--base-url", default="https://annas-archive.gl/",
                        help="Anna's Archive base URL")
    parser.add_argument("--delay", type=float, default=5.0,
                        help="Seconds between searches (default: 5)")
    parser.add_argument("--include-broken", action="store_true",
                        help="Also process books with broken file refs")
    parser.add_argument("--only", default="",
                        help="Process only this citekey")
    parser.add_argument("--update-bib", action="store_true",
                        help="Add file fields to old.bib for downloaded PDFs")
    parser.add_argument("--retry-not-found", action="store_true",
                        help="Retry books previously marked as not found")
    parser.add_argument("--retry-errors", action="store_true",
                        help="Retry books that failed due to download errors")
    parser.add_argument("--verbose", action="store_true",
                        help="Extra debug output")
    args = parser.parse_args()

    # Handle --update-bib separately
    if args.update_bib:
        progress = load_progress(PROGRESS_FILE)
        if not progress.get("downloaded"):
            print("No downloaded books in progress file.")
            sys.exit(0)
        n = update_bib_file(BIB_FILE, progress)
        print(f"Updated {n} entries in {BIB_FILE.name}")
        sys.exit(0)

    # Ensure base URL ends with /
    if not args.base_url.endswith("/"):
        args.base_url += "/"

    # Resolve secret key
    secret_key = args.key or os.environ.get("ANNAS_SECRET_KEY", "") or get_secret_key()
    if not secret_key and not args.dry_run:
        print("Error: no secret key. Set --key, ANNAS_SECRET_KEY env var, "
              "or configure in ~/.claude.json", file=sys.stderr)
        sys.exit(1)

    # Handle --retry-not-found / --retry-errors: clear lists so they get retried
    if args.retry_not_found or args.retry_errors:
        progress = load_progress(PROGRESS_FILE)
        if args.retry_not_found:
            cleared = len(progress.get("not_found", []))
            progress["not_found"] = []
            print(f"Cleared {cleared} not-found entries.")
        if args.retry_errors:
            cleared = len(progress.get("errors", []))
            progress["errors"] = []
            print(f"Cleared {cleared} error entries.")
        save_progress(PROGRESS_FILE, progress)
        if not args.resume:
            args.resume = True

    # Parse bib file
    print(f"Parsing {BIB_FILE}...")
    books = parse_bib_books(BIB_FILE)
    print(f"  Total @book entries: {len(books)}")

    missing = books_missing_pdf(books, include_broken=args.include_broken)
    print(f"  Missing PDFs: {len(missing)}")

    if args.only:
        missing = [b for b in missing if b["key"] == args.only]
        if not missing:
            print(f"  Citekey '{args.only}' not found among missing books")
            sys.exit(1)

    # Load progress
    progress = load_progress(PROGRESS_FILE) if args.resume else {
        "downloaded": {}, "not_found": [], "errors": [], "skipped": []
    }

    # Process books
    to_process = missing
    if args.limit:
        to_process = to_process[: args.limit]

    downloaded_count = 0
    not_found_count = 0
    error_count = 0
    skipped_count = 0

    print(f"\nProcessing {len(to_process)} books...\n")

    for i, book in enumerate(to_process, 1):
        key = book["key"]
        broken = book.get("_broken_ref", False)

        # Skip if already processed
        if args.resume:
            if key in progress["downloaded"]:
                skipped_count += 1
                continue
            if key in progress["not_found"]:
                skipped_count += 1
                continue
            if key in progress["errors"]:
                skipped_count += 1
                continue
            if key in progress["skipped"]:
                skipped_count += 1
                continue

        # Check if file already exists on disk
        dest = LIBRARY_DIR / f"{key}.pdf"
        if dest.exists() and not broken:
            skipped_count += 1
            if args.verbose:
                print(f"[{i}/{len(to_process)}] {key}: already exists, skipping")
            continue

        title_preview = (book["title"] or "(no title)")[:60]
        tag = "BROKEN" if broken else "MISSING"
        print(f"[{i}/{len(to_process)}] {key} ({tag})")
        print(f"  Title: {title_preview}")

        # Build and execute search
        query = build_search_query(book)
        if not query:
            print("  No ISBN. Skipping.")
            progress["skipped"].append(key)
            skipped_count += 1
            continue

        print(f"  Query: {query}")

        results = search_annas_archive(query, args.base_url, verbose=args.verbose)
        pdf_count = sum(1 for r in results if r.get("format") == "pdf")
        print(f"  Results: {len(results)} total, {pdf_count} PDFs")

        best = select_best_result(results, target_book=book)

        if not best:
            print("  No suitable PDF found.")
            if key not in progress["not_found"]:
                progress["not_found"].append(key)
            not_found_count += 1
            save_progress(PROGRESS_FILE, progress)
            if i < len(to_process):
                time.sleep(args.delay)
            continue

        sim = title_similarity(book.get("title", ""), best.get("title", ""))
        ebook_flag = " [EBOOK-DERIVED]" if is_ebook_derived(best) else ""
        print(
            f"  Best: {best['md5'][:12]}... "
            f"({best['size']}, {best.get('year', '?')}, sim={sim:.2f}){ebook_flag}"
        )
        if args.verbose:
            print(f"    Title: {best.get('title', '?')}")
            print(f"    Source: {best.get('source', '?')}")
            print(f"    Filename: {best.get('filename', '?')[:80]}")

        if args.dry_run:
            print("  [DRY RUN] Would download.")
            downloaded_count += 1
            if i < len(to_process):
                time.sleep(args.delay)
            continue

        # Download
        print(f"  Downloading to {dest.name}...")
        try:
            ok = download_via_fast_api(
                best["md5"], secret_key, args.base_url, dest,
                verbose=args.verbose
            )
        except QuotaExhaustedError as e:
            print(f"\n  {e}")
            print("  Stopping. Re-run with --retry-errors --resume tomorrow.")
            if key not in progress["errors"]:
                progress["errors"].append(key)
            save_progress(PROGRESS_FILE, progress)
            break

        if ok:
            file_size = dest.stat().st_size
            print(f"  OK ({file_size / (1024*1024):.1f} MB)")
            progress["downloaded"][key] = {
                "md5": best["md5"],
                "size": file_size,
                "query": query,
            }
            downloaded_count += 1
            # Update bib entry inline
            if update_bib_entry(BIB_FILE, key):
                print(f"  Updated bib entry")
        else:
            print("  FAILED")
            if key not in progress["errors"]:
                progress["errors"].append(key)
            error_count += 1

        save_progress(PROGRESS_FILE, progress)

        if i < len(to_process):
            time.sleep(args.delay)

    # Summary
    print(f"\n{'='*60}")
    print(f"Done. Downloaded: {downloaded_count}, Not found: {not_found_count}, "
          f"Errors: {error_count}, Skipped: {skipped_count}")
    print(f"Progress saved to: {PROGRESS_FILE}")

    if progress["downloaded"]:
        print(f"\nTo add file fields to old.bib, run:")
        print(f"  python3 {Path(__file__).name} --update-bib")
        print(f"\nDownloaded PDFs are in: {LIBRARY_DIR}/")

    # Print not-found citekeys for manual follow-up
    if progress["not_found"]:
        print(f"\nBooks not found ({len(progress['not_found'])}):")
        for k in progress["not_found"][-20:]:
            print(f"  {k}")
        if len(progress["not_found"]) > 20:
            print(f"  ... and {len(progress['not_found']) - 20} more")


if __name__ == "__main__":
    main()
