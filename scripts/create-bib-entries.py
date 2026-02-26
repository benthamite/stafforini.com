#!/usr/bin/env python3
"""Create BibLaTeX entries for unmatched WordPress quotes.

Five-tier lookup strategy:
1. Amazon ISBN via zotra /search (extract ISBN-10 from Amazon URLs)
2. OpenLibrary ISBN via zotra /search (search by author+title)
3. CrossRef DOI via zotra /search (search by author+title for journal articles)
4. Web URL via zotra /web (search DuckDuckGo for newspaper/magazine articles)
5. Fallback from WP metadata (minimal entry from attribution text)

Usage:
    python create-bib-entries.py                        # Process all works
    python create-bib-entries.py --limit 10             # Process first 10 works
    python create-bib-entries.py --dry-run --limit 10   # Test first 10
    python create-bib-entries.py --reprocess-fallbacks  # Re-run fallback entries through new tiers
"""

import argparse
import html as html_module
import json
import re
import sys
import time
import unicodedata
from datetime import datetime, timezone
from pathlib import Path
from urllib.parse import quote_plus, unquote

try:
    import requests
except ImportError:
    print("ERROR: 'requests' package required. Install with: pip install requests")
    sys.exit(1)


# === Constants ===

SCRIPTS_DIR = Path(__file__).parent
MATCHED_JSON = SCRIPTS_DIR / "wp-quotes-matched.json"
PROGRESS_FILE = SCRIPTS_DIR / "create-bib-progress.json"
REPORT_FILE = SCRIPTS_DIR / "create-bib-report.txt"
MIGRATION_BIB = Path.home() / "My Drive/bibliography/migration.bib"

ZOTRA_BASE = "http://localhost:1969"
ZOTRA_TIMEOUT = 30

OPENLIBRARY_SEARCH_URL = "https://openlibrary.org/search.json"
OPENLIBRARY_RATE_LIMIT = 1.0  # seconds between requests

CROSSREF_API_URL = "https://api.crossref.org/works"
CROSSREF_RATE_LIMIT = 0.5  # seconds between requests

DUCKDUCKGO_HTML_URL = "https://html.duckduckgo.com/html/"
DUCKDUCKGO_RATE_LIMIT = 2.5  # seconds between requests (be polite)

from lib import CORE_BIB_FILES as BIB_FILES
from lib import STOP_WORDS as _BASE_STOP_WORDS
from lib import normalize, parse_bib_entries

# Stop words for cite key title extraction (EN/ES/FR/DE/IT)
STOP_WORDS = _BASE_STOP_WORDS | {
    "how", "what", "why", "who", "which", "when", "where", "all", "some",
    "any", "no", "into", "about", "over", "after", "before", "between",
    "through", "during", "upon", "toward", "towards", "new",
    # Spanish
    "el", "la", "los", "las", "de", "del", "en", "un", "una", "y", "o",
    "al", "con", "por", "para", "que", "se", "su", "sus", "como", "mas",
    "sino", "sobre", "entre", "hacia",
    # French
    "le", "les", "des", "du", "et", "au", "aux", "ce", "cette", "ces",
    "ne", "pas", "qui", "ou", "sur", "dans", "pour", "avec",
    # German
    "der", "die", "das", "und", "von", "ein", "eine", "einer", "eines",
    "zu", "den", "dem", "auf", "ist", "im", "mit", "fur", "uber",
    "nach", "sich",
    # Italian
    "il", "lo", "gli", "i", "di", "da", "con", "su", "per", "tra",
    "fra", "che", "non", "si", "ne", "ci", "vi",
}

# Title truncation pattern (from bibtex-autokey settings)
TITLE_TRUNCATE_RE = re.compile(r"[.!?;]|--")

# Known compound surnames: normalized full name -> surname for cite key
COMPOUND_SURNAMES = {
    "adolfo bioy casares": "Bioy Casares",
    "bioy casares": "Bioy Casares",
    "alain de botton": "de Botton",
    "simone de beauvoir": "de Beauvoir",
    "alexis de tocqueville": "de Tocqueville",
    "antoine de saint-exupery": "de Saint-Exupery",
    "charles de montesquieu": "de Montesquieu",
    "charles de gaulle": "de Gaulle",
    "miguel de unamuno": "de Unamuno",
    "miguel de cervantes": "de Cervantes",
    "martin luther king": "King",
    "martin luther king jr": "King",
    "arthur conan doyle": "Conan Doyle",
    "ursula k le guin": "LeGuin",
    "ursula le guin": "LeGuin",
}

# Classical authors for special handling (lowercase, no accents)
CLASSICAL_AUTHORS_LIST = [
    "aristotle", "plato", "socrates", "seneca", "cicero", "epictetus",
    "epicurus", "marcus aurelius", "plutarch", "tacitus", "horace",
    "virgil", "ovid", "lucretius", "thucydides", "herodotus",
    "xenophon", "diogenes laertius", "aesop", "homer", "hesiod",
    "sextus empiricus", "augustine", "thomas aquinas", "boethius",
    "confucius", "lao tzu", "buddha",
]

# Preferred field ordering for bib output
FIELD_ORDER = [
    "crossref",
    "author", "editor", "translator",
    "title", "shorttitle", "subtitle",
    "booktitle", "journaltitle", "maintitle",
    "date", "origdate",
    "edition", "volume", "number", "part",
    "pages", "pagetotal",
    "publisher", "location",
    "isbn", "issn", "doi",
    "url", "urldate",
    "howpublished", "type", "entrysubtype",
    "series", "language", "langid",
    "note",
    "abstract", "keywords",
    "file", "timestamp",
]


# === Utility functions ===


def strip_accents(text):
    """Remove accents but preserve case."""
    text = unicodedata.normalize("NFD", text)
    text = "".join(c for c in text if unicodedata.category(c) != "Mn")
    return text


def extract_surname_for_key(author_str):
    """Extract surname for cite key generation from a 'First Last' name.

    Returns the raw surname (not normalized), suitable for key building.
    Handles compound surnames and particles.
    """
    if not author_str:
        return ""

    # Strip "et al.", "(ed.)", "(eds.)"
    author_str = re.sub(r"\s+et\s+al\.?.*$", "", author_str).strip()
    author_str = re.sub(r"\s*\(eds?\.?\)\s*", "", author_str).strip()

    # Check compound surname table
    lookup = normalize(author_str)
    if lookup in COMPOUND_SURNAMES:
        return COMPOUND_SURNAMES[lookup]

    # "Last, First" format (from bib entries)
    if "," in author_str:
        return author_str.split(",")[0].strip().replace("{", "").replace("}", "")

    # "First Last" format: last word is surname
    parts = author_str.strip().split()
    if len(parts) >= 2:
        return parts[-1]
    return author_str.strip()


def to_bib_author(name):
    """Convert 'First Last' to 'Last, First' for BibLaTeX."""
    if not name:
        return ""

    # Already in "Last, First" format
    if "," in name:
        return name.strip()

    # Handle "et al."
    et_al = ""
    if re.search(r"\bet\s+al", name, re.IGNORECASE):
        name = re.sub(r"\s+et\s+al\.?", "", name, flags=re.IGNORECASE).strip()
        et_al = " and others"

    # Handle "(ed.)" / "(eds.)"
    name = re.sub(r"\s*\(eds?\.?\)\s*", "", name).strip()

    parts = name.strip().split()
    if len(parts) <= 1:
        return name.strip() + et_al

    # Check compound surname table
    lookup = normalize(name)
    if lookup in COMPOUND_SURNAMES:
        surname = COMPOUND_SURNAMES[lookup]
        # Find first name(s) by removing surname words from the end
        surname_words = surname.split()
        remaining = parts[: len(parts) - len(surname_words)]
        if remaining:
            return f"{surname}, {' '.join(remaining)}" + et_al
        return surname + et_al

    # Default: last word is surname
    return f"{parts[-1]}, {' '.join(parts[:-1])}" + et_al


def to_bib_editors(editor_str):
    """Convert editor string (potentially multiple editors) to BibTeX format.

    Splits on ' and ' and converts each name to 'Last, First' format.
    """
    if not editor_str:
        return ""
    names = re.split(r"\s+and\s+", editor_str)
    bib_names = [to_bib_author(n.strip()) for n in names if n.strip()]
    return " and ".join(bib_names)


def extract_first_editor_surname(editor_str):
    """Extract the first editor's surname for collection key generation."""
    if not editor_str:
        return ""
    editors = re.split(r"\s+and\s+", editor_str)
    return extract_surname_for_key(editors[0].strip())


def get_timestamp():
    """Current timestamp in bib convention format."""
    now = datetime.now(timezone.utc)
    return now.strftime("%Y-%m-%d %H:%M:%S (GMT)")


# === Bib file parsing ===


def _parse_bib_entries_for_keys(bib_path):
    """Parse bib entries with brace-stripped author/title for key generation."""
    return parse_bib_entries(bib_path, strip_braces=["author", "title"])


def load_existing_keys(bib_files):
    """Load all existing cite keys for collision detection.

    Returns dict: cite_key -> {author, title, year}.
    """
    existing = {}
    for bib_path in bib_files:
        if not bib_path.exists():
            continue
        for entry in _parse_bib_entries_for_keys(bib_path):
            existing[entry["cite_key"]] = {
                "author": entry["author"],
                "title": entry["title"],
                "year": entry["year"],
            }
    return existing


# === Cite key generation ===


def generate_cite_key(author_surname, year, title, used_keys):
    """Generate a cite key following bibtex-autokey conventions.

    Format: {Surname}{Year}{Word1}{Word2}{Word3}

    Args:
        author_surname: raw surname string (may have accents, spaces)
        year: 4-digit year string
        title: work title
        used_keys: set of already-used keys (mutated on success)

    Returns:
        Generated cite key string.
    """
    # Surname: strip accents, remove dots/hyphens, concatenate multi-word
    surname = strip_accents(author_surname).replace(".", "").replace("'", "")
    surname_parts = surname.split()
    # Capitalize each part, preserving lowercase particles like "de", "von"
    surname_key = ""
    for part in surname_parts:
        if not part:
            continue
        surname_key += part[0].upper() + part[1:]

    # Year
    year_key = year if year else ""

    # Title: truncate at sentence-ending punctuation, extract content words
    title_clean = title or ""
    trunc = TITLE_TRUNCATE_RE.search(title_clean)
    if trunc:
        title_clean = title_clean[: trunc.start()]

    # Extract alphabetic words, strip accents
    words = re.findall(r"[a-zA-Z\u00C0-\u024F]+", strip_accents(title_clean))
    content_words = [w for w in words if w.lower() not in STOP_WORDS]
    title_words = content_words[:3]
    title_key = "".join(w[0].upper() + w[1:] for w in title_words)

    key = f"{surname_key}{year_key}{title_key}"
    if not key:
        key = "Unknown"

    # Collision detection
    if key not in used_keys:
        used_keys.add(key)
        return key

    # Append suffix for collisions
    for suffix in "abcdefghijklmnopqrstuvwxyz":
        candidate = f"{key}{suffix}"
        if candidate not in used_keys:
            used_keys.add(candidate)
            return candidate

    # Extremely unlikely fallback: try double-letter suffixes
    for length in range(2, 5):
        for suffix in "abcdefghijklmnopqrstuvwxyz":
            candidate = f"{key}{suffix * length}"
            if candidate not in used_keys:
                used_keys.add(candidate)
                return candidate
    # Last resort: use a unique numeric suffix
    counter = 2
    while True:
        candidate = f"{key}_{counter}"
        if candidate not in used_keys:
            used_keys.add(candidate)
            return candidate
        counter += 1


# === Work type detection ===


def detect_work_type(quote):
    """Detect work type from quote metadata.

    Returns one of: 'classical', 'speech', 'letter', 'article', 'incollection', 'book'.
    """
    author = quote.get("author", "")
    attribution = quote.get("attribution_text", "")
    article_title = quote.get("article_title", "")
    work_title = quote.get("work_title", "")

    # Classical/ancient authors
    if normalize(author) in CLASSICAL_AUTHORS_LIST:
        return "classical"

    # Articles/chapters in collections (check before speech/letter to avoid
    # false positives on titles like "A Lecture on Ethics" or "Letter to X")
    if article_title and work_title:
        # Check if work_title looks like a periodical rather than a book
        periodical_re = (
            r"\b(?:Newspaper|Review|Journal|Quarterly|Magazine|Gazette|Tribune"
            r"|Times|Herald|Post|Observer|Guardian|Chronicle|Monthly|Weekly"
            r"|Proceedings|Bulletin|Annals)\b"
        )
        if re.search(periodical_re, work_title, re.IGNORECASE):
            return "article"
        return "incollection"

    # Journal articles (article_title + volume but no work_title)
    if article_title and not work_title:
        if re.search(r"\bvol\.\s*\d+", attribution):
            return "article"
        return "misc"  # standalone piece (poem, essay, etc.)

    # Speeches (not published in a book/collection — those are caught above)
    speech_re = r"\b(?:speech|lecture|address|sermon|keynote|testimony|commencement)\b"
    if re.search(speech_re, attribution, re.IGNORECASE):
        # Skip if keyword is just part of the article title
        if not (article_title and re.search(speech_re, article_title, re.IGNORECASE)):
            if not work_title or ", in " not in attribution:
                return "speech"

    # Letters (not published in collections)
    if re.search(r"\bletter\s+to\b", attribution, re.IGNORECASE):
        if not (article_title and re.search(r"\bletter\b", article_title, re.IGNORECASE)):
            if not work_title or ", in " not in attribution:
                return "letter"

    return "book"


def extract_isbn_from_html(attribution_html):
    """Extract ISBN-10 from Amazon URL in attribution HTML.

    Skips B-prefix ASINs (Amazon product IDs, not ISBNs).
    Returns ISBN string or None.
    """
    if not attribution_html:
        return None
    unescaped = html_module.unescape(attribution_html)
    asin_match = re.search(
        r"(?:product|dp|ASIN)[/=]([A-Z0-9]{10})", unescaped, re.IGNORECASE
    )
    if asin_match:
        asin = asin_match.group(1).upper()
        if not asin.startswith("B"):
            return asin
    return None


def extract_quoted_in_author(quote):
    """Extract the book author from a 'quoted in' attribution.

    E.g. 'Niels Bohr, quoted in Niels Blaedel, Harmony and Unity...'
    -> 'Niels Blaedel'
    """
    attr = quote.get("attribution_text", "")
    title = quote.get("work_title", "")

    qi_match = re.search(r"quoted\s+in\s+", attr, re.IGNORECASE)
    if not qi_match:
        return None

    after_qi = attr[qi_match.end() :]

    # Try to find text between "quoted in" and the work title
    if title:
        title_idx = after_qi.find(title)
        if title_idx > 0:
            author_part = after_qi[:title_idx].strip().rstrip(",").strip()
            # Remove editor markers
            author_part = re.sub(r"\s*\(eds?\.?\)\s*", "", author_part)
            author_part = re.sub(r",?\s+in\s*$", "", author_part, flags=re.IGNORECASE)
            if author_part:
                return author_part

    # Fallback: take text up to the next comma
    parts = after_qi.split(",")
    if parts:
        return parts[0].strip()

    return None


def extract_work_info(quote):
    """Extract work info for bib entry creation.

    Handles 'quoted in' pattern, work type detection, ISBN extraction,
    and metadata parsing from attribution text.
    """
    attribution_text = quote.get("attribution_text", "")
    attribution_html = quote.get("attribution_html", "")

    # Check for "quoted in" pattern
    is_quoted_in = bool(re.search(r"quoted\s+in\b", attribution_text, re.IGNORECASE))

    if is_quoted_in:
        bib_author = extract_quoted_in_author(quote) or quote.get("author", "")
    else:
        bib_author = quote.get("author", "")

    title = quote.get("work_title", "") or quote.get("article_title", "")
    year = quote.get("year", "")

    # Extract ISBN from Amazon URL
    isbn = extract_isbn_from_html(attribution_html)

    # Parse additional metadata from attribution text
    location = None
    edition = None
    editor = None
    journaltitle = None

    # Location: city name before year
    loc_match = re.search(
        r",\s+([A-Z][a-z]+(?:\s+[A-Z][a-z]+)*)\s*,\s*\d{4}", attribution_text
    )
    if loc_match:
        candidate = loc_match.group(1)
        work_title = quote.get("work_title", "")
        # Filter out false positives: work title, common non-location words
        if (
            candidate.lower() not in {"vol", "ed", "trans", "rev"}
            and candidate != work_title
            and candidate not in attribution_text.split("'")  # not part of article title
        ):
            location = candidate

    # Edition
    ed_match = re.search(
        r"(\d+)(?:st|nd|rd|th)\s+ed\.?", attribution_text, re.IGNORECASE
    )
    if ed_match:
        edition = ed_match.group(1)
    elif re.search(r"revised\s+ed", attribution_text, re.IGNORECASE):
        edition = "revised"

    # Editor (for incollections: "in Author (ed.), ...")
    editor_match = re.search(
        r"\bin\s+(.+?)\s*\(eds?\.?\)", attribution_text, re.IGNORECASE
    )
    if editor_match:
        editor = editor_match.group(1).strip()

    # Journal title (for articles without work_title)
    article_title = quote.get("article_title", "")
    if article_title and not quote.get("work_title"):
        idx = attribution_text.find(article_title)
        if idx >= 0:
            after = attribution_text[idx + len(article_title) :]
            after = after.lstrip("',\" ").strip()
            jt_match = re.match(r"(.+?)(?:,\s*vol\.|,\s*\d{4}|$)", after)
            if jt_match:
                jt = jt_match.group(1).strip().rstrip(",")
                if jt and len(jt) > 2:
                    journaltitle = jt

    # Volume, number, pages — extracted directly from attribution_text
    volume = None
    vol_match = re.search(r"(?:supp\.\s+)?vol\.\s*(\d+)", attribution_text)
    if vol_match:
        volume = vol_match.group(1)

    number = None
    num_match = re.search(r"no\.\s*(\d+)", attribution_text)
    if num_match:
        number = num_match.group(1)

    pages = None
    pages_match = re.search(
        r"pp?\.\s*([\d\w]+(?:\s*[-–—]\s*[\d\w]+)?)", attribution_text
    )
    if pages_match:
        # Normalize dashes to en-dash
        pages = re.sub(r"\s*[-–—]\s*", "--", pages_match.group(1))

    work_type = detect_work_type(quote)

    return {
        "bib_author": bib_author,
        "title": title,
        "year": year,
        "isbn": isbn,
        "is_quoted_in": is_quoted_in,
        "work_type": work_type,
        "location": location,
        "edition": edition,
        "editor": editor,
        "journaltitle": journaltitle,
        "volume": volume,
        "number": number,
        "pages": pages,
        "article_title": article_title,
        "work_title": quote.get("work_title", ""),
        "attribution_text": attribution_text,
    }


# === Zotra API ===


def check_zotra():
    """Check if zotra server is running."""
    try:
        requests.get(f"{ZOTRA_BASE}/", timeout=5)
        return True
    except requests.ConnectionError:
        return False


def zotra_search(identifier):
    """Search zotra by identifier (ISBN, DOI, etc.).

    Returns Zotero JSON array or None.
    """
    try:
        resp = requests.post(
            f"{ZOTRA_BASE}/search",
            data=identifier,
            headers={"Content-Type": "text/plain"},
            timeout=ZOTRA_TIMEOUT,
        )
        if resp.status_code == 200:
            return resp.json()
        return None
    except (requests.RequestException, json.JSONDecodeError):
        return None


def zotra_export_biblatex(zotero_json):
    """Export Zotero JSON to BibLaTeX format.

    Returns BibLaTeX string or None.
    """
    try:
        resp = requests.post(
            f"{ZOTRA_BASE}/export?format=biblatex",
            json=zotero_json,
            headers={"Content-Type": "application/json"},
            timeout=ZOTRA_TIMEOUT,
        )
        if resp.status_code == 200:
            # Force UTF-8 decoding (requests defaults to MacRoman when
            # the server doesn't specify a charset)
            return resp.content.decode("utf-8")
        return None
    except requests.RequestException:
        return None


# === OpenLibrary API ===

_last_ol_request = 0.0


def openlibrary_search(author_surname, title, limit=3):
    """Search OpenLibrary for a work. Returns list of ISBNs.

    Respects rate limiting (1 req/sec).
    """
    global _last_ol_request

    elapsed = time.time() - _last_ol_request
    if elapsed < OPENLIBRARY_RATE_LIMIT:
        time.sleep(OPENLIBRARY_RATE_LIMIT - elapsed)

    try:
        params = {
            "author": author_surname,
            "title": title,
            "limit": limit,
            "fields": "isbn,title,author_name",
        }
        resp = requests.get(OPENLIBRARY_SEARCH_URL, params=params, timeout=15)
        _last_ol_request = time.time()

        if resp.status_code != 200:
            return []

        data = resp.json()
        docs = data.get("docs", [])

        isbns = []
        for doc in docs:
            doc_isbns = doc.get("isbn", [])
            # Prefer ISBN-13
            isbn13s = [i for i in doc_isbns if len(i) == 13]
            isbn10s = [i for i in doc_isbns if len(i) == 10]
            isbns.extend(isbn13s[:3])
            isbns.extend(isbn10s[:3])

        return isbns[:10]
    except (requests.RequestException, json.JSONDecodeError):
        _last_ol_request = time.time()
        return []


# === CrossRef API ===

_last_cr_request = 0.0


def crossref_search_doi(author_surname, title, journal=None):
    """Search CrossRef for a DOI by author and title.

    Mirrors bib.el's bib-search-crossref logic:
    query.bibliographic=<title>&query.author=<author>

    Returns DOI string or None.
    """
    global _last_cr_request

    elapsed = time.time() - _last_cr_request
    if elapsed < CROSSREF_RATE_LIMIT:
        time.sleep(CROSSREF_RATE_LIMIT - elapsed)

    try:
        params = {
            "query.bibliographic": title,
            "rows": 5,
            "select": "DOI,title,author,container-title,score",
        }
        if author_surname:
            params["query.author"] = author_surname
        if journal:
            params["query.container-title"] = journal

        headers = {
            "User-Agent": "stafforini-migration/1.0 (https://stafforini.com; quote migration script)",
        }

        resp = requests.get(
            CROSSREF_API_URL, params=params, headers=headers, timeout=15
        )
        _last_cr_request = time.time()

        if resp.status_code != 200:
            return None

        data = resp.json()
        items = data.get("message", {}).get("items", [])

        title_norm = normalize(title)
        surname_norm = normalize(author_surname) if author_surname else ""

        for item in items:
            item_title = (item.get("title") or [""])[0]
            item_title_norm = normalize(item_title)

            # Verify title match (at least partial)
            if not item_title_norm:
                continue

            # Check title similarity: all significant words from our title
            # should appear in the CrossRef title (or vice versa)
            our_words = {w for w in title_norm.split() if w not in STOP_WORDS and len(w) > 2}
            their_words = {w for w in item_title_norm.split() if w not in STOP_WORDS and len(w) > 2}
            if not our_words:
                continue
            overlap = our_words & their_words
            if len(overlap) < min(len(our_words), 2):
                continue

            # Verify author match if we have a surname
            if surname_norm:
                authors = item.get("author", [])
                author_match = any(
                    surname_norm in normalize(a.get("family", "") + " " + a.get("given", ""))
                    for a in authors
                )
                if not author_match:
                    continue

            doi = item.get("DOI")
            if doi:
                return doi

        return None
    except (requests.RequestException, json.JSONDecodeError, KeyError):
        _last_cr_request = time.time()
        return None


# === Web URL search (DuckDuckGo) ===

_last_ddg_request = 0.0

# Domains to skip when searching for article URLs
SKIP_DOMAINS = {
    "amazon.com", "goodreads.com", "wikipedia.org", "librarything.com",
    "worldcat.org", "openlibrary.org", "google.com", "bing.com",
    "duckduckgo.com", "facebook.com", "twitter.com", "reddit.com",
    "youtube.com", "archive.org", "scholar.google.com", "jstor.org",
    "philpapers.org",
}


def search_article_url(title, publication, author, year):
    """Search DuckDuckGo HTML for an article URL.

    Uses the same duckduckgo.com/html/ endpoint as bib.el's
    bib-lbx--ddg-items, with double-decoding of result URLs.

    Returns URL string or None.
    """
    global _last_ddg_request

    if not title:
        return None

    elapsed = time.time() - _last_ddg_request
    if elapsed < DUCKDUCKGO_RATE_LIMIT:
        time.sleep(DUCKDUCKGO_RATE_LIMIT - elapsed)

    try:
        # Build query: quoted title + publication name + author
        parts = [f'"{title}"']
        if publication:
            parts.append(publication)
        if author:
            # Use just the surname to avoid over-constraining
            surname = extract_surname_for_key(author)
            if surname:
                parts.append(surname)

        query = " ".join(parts)
        url = f"{DUCKDUCKGO_HTML_URL}?q={quote_plus(query)}"

        resp = requests.get(
            url,
            headers={
                "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
                "AppleWebKit/537.36 (KHTML, like Gecko) "
                "Chrome/120.0.0.0 Safari/537.36",
            },
            timeout=15,
        )
        _last_ddg_request = time.time()

        if resp.status_code != 200:
            return None

        html = resp.text

        # DuckDuckGo wraps result URLs in uddg= parameter (URL-encoded)
        # Double-decode like bib.el does
        decoded = unquote(html)
        decoded = unquote(decoded)

        # Extract uddg URLs
        url_matches = re.findall(r"uddg=([^&\"' ]+)", decoded)

        for encoded_url in url_matches:
            candidate = unquote(encoded_url)

            # Skip irrelevant domains
            if any(skip in candidate.lower() for skip in SKIP_DOMAINS):
                continue

            # Must be a proper HTTP URL
            if candidate.startswith("http"):
                return candidate

        return None
    except requests.RequestException:
        _last_ddg_request = time.time()
        return None


# === Zotra /web endpoint ===


def zotra_web(url):
    """Extract metadata from a URL via zotra /web endpoint.

    Returns Zotero JSON array or None.
    """
    try:
        resp = requests.post(
            f"{ZOTRA_BASE}/web",
            data=url,
            headers={"Content-Type": "text/plain"},
            timeout=ZOTRA_TIMEOUT,
        )
        if resp.status_code == 200:
            return resp.json()
        return None
    except (requests.RequestException, json.JSONDecodeError):
        return None


# === BibLaTeX construction ===


def parse_biblatex_entry(text):
    """Parse a single BibLaTeX entry.

    Returns (entry_type, cite_key, fields_dict) or None.
    """
    match = re.search(r"@(\w+)\s*\{([^,\s]+)\s*,", text)
    if not match:
        return None
    entry_type = match.group(1).lower()
    cite_key = match.group(2)

    fields = {}
    body = text[match.end() :]
    for fm in re.finditer(
        r"(\w+)\s*=\s*(?:\{((?:[^{}]|\{[^{}]*\})*)\}|\"([^\"]*)\"|(\d+))",
        body,
    ):
        field_name = fm.group(1).lower()
        field_value = fm.group(2) or fm.group(3) or fm.group(4) or ""
        fields[field_name] = field_value.strip()

    return entry_type, cite_key, fields


def format_bib_entry(entry_type, cite_key, fields):
    """Format a BibLaTeX entry with tab indentation and ordered fields."""
    lines = [f"@{entry_type}{{{cite_key},"]

    added = set()
    for key in FIELD_ORDER:
        if key in fields and fields[key]:
            lines.append(f"\t{key} = {{{fields[key]}}},")
            added.add(key)

    for key, value in fields.items():
        if key not in added and value:
            lines.append(f"\t{key} = {{{value}}},")

    lines.append("}")
    return "\n".join(lines)


def build_entry_from_zotra(biblatex_text, work_info, cite_key):
    """Build a formatted entry from zotra output with our cite key.

    Parses zotra's BibLaTeX, replaces the key, adds special fields.
    """
    parsed = parse_biblatex_entry(biblatex_text)
    if not parsed:
        return None

    entry_type, _, fields = parsed

    # Override entry type for special work types
    if work_info["work_type"] == "classical":
        fields.setdefault("entrysubtype", "classical")
    elif work_info["work_type"] == "speech":
        entry_type = "misc"
    elif work_info["work_type"] == "letter":
        entry_type = "misc"

    # Ensure author is present (zotra /web may not extract it)
    if "author" not in fields and work_info.get("bib_author"):
        fields["author"] = to_bib_author(work_info["bib_author"])

    # Ensure title is present
    if "title" not in fields and work_info.get("title"):
        fields["title"] = work_info["title"]

    # Ensure date is present
    if "date" not in fields and "year" not in fields and work_info.get("year"):
        fields["date"] = work_info["year"]

    # Prefer 'date' over 'year'
    if "year" in fields and "date" not in fields:
        fields["date"] = fields.pop("year")

    # Remove 'abstract' to keep entries lean
    fields.pop("abstract", None)

    fields["timestamp"] = get_timestamp()

    return format_bib_entry(entry_type, cite_key, fields)


def build_fallback_entry(work_info, cite_key):
    """Build a minimal BibLaTeX entry from WP metadata."""
    wtype = work_info["work_type"]

    type_map = {
        "book": "book",
        "classical": "book",
        "article": "article",
        "incollection": "incollection",
        "speech": "misc",
        "letter": "misc",
        "misc": "misc",
    }
    entry_type = type_map.get(wtype, "book")

    fields = {}

    # Author
    bib_author = to_bib_author(work_info["bib_author"])
    if bib_author:
        fields["author"] = bib_author

    # Editor
    if work_info.get("editor"):
        fields["editor"] = to_bib_author(work_info["editor"])

    # Title fields depend on entry type
    article_title = work_info.get("article_title", "")
    work_title = work_info.get("work_title", "")

    if entry_type == "incollection":
        fields["title"] = article_title or work_title
        if work_title and article_title and work_title != article_title:
            fields["booktitle"] = work_title
    elif entry_type == "article":
        fields["title"] = article_title or work_title
        if work_info.get("journaltitle"):
            fields["journaltitle"] = work_info["journaltitle"]
        elif work_title and work_title != article_title:
            # work_title is likely the periodical name
            fields["journaltitle"] = work_title
    else:
        fields["title"] = work_info["title"]

    # Date
    if work_info["year"]:
        fields["date"] = work_info["year"]

    # Location
    if work_info.get("location"):
        fields["location"] = work_info["location"]

    # Edition
    if work_info.get("edition"):
        fields["edition"] = work_info["edition"]

    # Volume, number, pages
    if work_info.get("volume"):
        fields["volume"] = work_info["volume"]
    if work_info.get("number"):
        fields["number"] = work_info["number"]
    if work_info.get("pages"):
        fields["pages"] = work_info["pages"]

    # Special fields
    if wtype == "classical":
        fields["entrysubtype"] = "classical"

    if wtype == "speech":
        attr = work_info.get("attribution_text", "")
        # Try to extract date
        date_match = re.search(
            r"(\w+\s+\d{1,2},?\s*\d{4})", attr
        )
        if date_match:
            fields["howpublished"] = f"Speech, {date_match.group(1)}"
        else:
            fields["howpublished"] = "Speech"

    if wtype == "letter":
        attr = work_info.get("attribution_text", "")
        letter_match = re.search(
            r"[Ll]etter\s+to\s+(.+?)(?:,\s*(\w+\s+\d{1,2},?\s*\d{4}))?(?:,|\s*$)",
            attr,
        )
        if letter_match:
            recipient = letter_match.group(1).strip()
            date = letter_match.group(2)
            howpub = f"Letter to {recipient}"
            if date:
                howpub += f", {date}"
            fields["howpublished"] = howpub

    fields["note"] = "Entry auto-generated from WordPress attribution"
    fields["timestamp"] = get_timestamp()

    return format_bib_entry(entry_type, cite_key, fields)


def build_collection_entry(work_info, collection_key):
    """Build a @collection entry for the parent work of an incollection."""
    fields = {}

    editor = work_info.get("editor", "")
    if editor:
        fields["editor"] = to_bib_editors(editor)

    fields["title"] = work_info.get("work_title", "")

    if work_info.get("year"):
        fields["date"] = work_info["year"]

    if work_info.get("location"):
        fields["location"] = work_info["location"]

    if work_info.get("edition"):
        fields["edition"] = work_info["edition"]

    fields["note"] = "Entry auto-generated from WordPress attribution"
    fields["timestamp"] = get_timestamp()

    return format_bib_entry("collection", collection_key, fields)


def build_incollection_entry(work_info, cite_key, collection_key):
    """Build an @incollection entry with crossref to parent collection."""
    fields = {"crossref": collection_key}

    bib_author = to_bib_author(work_info.get("bib_author", ""))
    if bib_author:
        fields["author"] = bib_author

    fields["title"] = work_info.get("article_title", "")

    if work_info.get("pages"):
        fields["pages"] = work_info["pages"]

    fields["timestamp"] = get_timestamp()

    return format_bib_entry("incollection", cite_key, fields)


# === Processing ===


def group_by_work(quotes):
    """Group unmatched quotes by unique work.

    Returns dict of work_key -> work_info dict.
    """
    unmatched_statuses = {"no_match", "no_match_confirmed", "no_candidates", "weak"}
    works = {}

    for idx, quote in enumerate(quotes):
        status = quote.get("bib_match", {}).get("status", "")
        if status not in unmatched_statuses:
            continue

        info = extract_work_info(quote)

        # Deduplication key
        key = f"{normalize(info['bib_author'])}|{normalize(info['title'])}|{info['year']}"

        if key not in works:
            works[key] = {
                "bib_author": info["bib_author"],
                "title": info["title"],
                "year": info["year"],
                "quote_indices": [],
                "isbns": set(),
                "work_type": info["work_type"],
                "location": info.get("location"),
                "edition": info.get("edition"),
                "editor": info.get("editor"),
                "journaltitle": info.get("journaltitle"),
                "volume": info.get("volume"),
                "number": info.get("number"),
                "pages": info.get("pages"),
                "article_title": info.get("article_title"),
                "work_title": info.get("work_title"),
                "attribution_text": info["attribution_text"],
                "is_quoted_in": info["is_quoted_in"],
            }

        works[key]["quote_indices"].append(idx)
        if info.get("isbn"):
            works[key]["isbns"].add(info["isbn"])

    return works


def process_work(work_key, work, used_keys, zotra_available, dry_run=False,
                  collections=None):
    """Process a single work through the five-tier lookup.

    Returns dict with cite_key, tier, bib_entry, status, notes.

    For incollection works (essay/chapter in an edited collection), creates
    two entries: a @collection for the parent work and an @incollection with
    crossref.  The collections dict tracks already-created collection entries
    to avoid duplicates across works.
    """
    if collections is None:
        collections = {}

    bib_author = work["bib_author"]
    title = work["title"]
    year = work["year"]
    isbns = list(work["isbns"])
    wtype = work.get("work_type", "book")

    surname = extract_surname_for_key(bib_author)

    # For articles, the search title should be the article_title (the actual paper)
    article_title = work.get("article_title", "")
    work_title = work.get("work_title", "")
    search_title = article_title or title

    # Detect incollection: needs both an article title and a distinct work title
    is_incollection = wtype == "incollection" and article_title and work_title

    result = {
        "cite_key": None,
        "tier": None,
        "bib_entry": None,
        "status": "error",
        "notes": "",
    }

    # --- Incollection pair builder -------------------------------------------

    def _make_incollection_pair(zotra_biblatex, tier, notes):
        """Create @collection + @incollection pair.

        If *zotra_biblatex* is provided, the collection metadata is taken from
        zotra output; otherwise a minimal fallback collection is built from the
        WP attribution.  Returns True on success (and mutates *result*).
        """
        editor = work.get("editor", "")
        editor_surname = ""

        # Prefer editor from WP metadata
        if editor:
            editor_surname = extract_first_editor_surname(editor)

        # Fall back to editor from zotra data
        if not editor_surname and zotra_biblatex:
            parsed = parse_biblatex_entry(zotra_biblatex)
            if parsed:
                _, _, zfields = parsed
                if "editor" in zfields:
                    editor_surname = extract_first_editor_surname(
                        zfields["editor"]
                    )

        # Last resort: use essay author surname
        if not editor_surname:
            editor_surname = surname

        # ---- Collection entry ------------------------------------------------
        coll_norm = (
            f"{normalize(editor_surname)}|{normalize(work_title)}|{year}"
        )

        coll_entry_str = None
        if coll_norm in collections:
            coll_key = collections[coll_norm]
        else:
            coll_key = generate_cite_key(
                editor_surname, year, work_title, used_keys
            )
            collections[coll_norm] = coll_key

            if zotra_biblatex:
                parsed = parse_biblatex_entry(zotra_biblatex)
                if parsed:
                    _, _, zfields = parsed
                    # Remove chapter-level fields
                    zfields.pop("author", None)
                    zfields.pop("pages", None)
                    # booktitle -> title for the collection
                    if "booktitle" in zfields:
                        zfields["title"] = zfields.pop("booktitle")
                    # Ensure editor
                    if "editor" not in zfields and editor:
                        zfields["editor"] = to_bib_editors(editor)
                    # Prefer date over year
                    if "year" in zfields and "date" not in zfields:
                        zfields["date"] = zfields.pop("year")
                    zfields.pop("abstract", None)
                    zfields["timestamp"] = get_timestamp()
                    coll_entry_str = format_bib_entry(
                        "collection", coll_key, zfields
                    )

            if not coll_entry_str:
                coll_entry_str = build_collection_entry(work, coll_key)

        # ---- Incollection entry ----------------------------------------------
        incoll_key = generate_cite_key(
            surname, year, article_title, used_keys
        )
        incoll_entry_str = build_incollection_entry(work, incoll_key, coll_key)

        combined = ""
        if coll_entry_str:
            combined = coll_entry_str + "\n\n"
        combined += incoll_entry_str

        result.update(
            cite_key=incoll_key,
            tier=tier,
            bib_entry=combined,
            status="success",
            notes=notes,
        )
        return True

    # --- Zotra helpers --------------------------------------------------------

    def _try_zotra_search(identifier, tier, note_prefix):
        """Helper: try zotra /search + /export for an identifier."""
        zotero_json = zotra_search(identifier)
        if zotero_json:
            biblatex = zotra_export_biblatex(zotero_json)
            if biblatex and biblatex.strip():
                if is_incollection:
                    return _make_incollection_pair(
                        biblatex, tier, f"{note_prefix} via zotra /search"
                    )
                ck = generate_cite_key(surname, year, title, used_keys)
                entry = build_entry_from_zotra(biblatex, work, ck)
                if entry:
                    result.update(
                        cite_key=ck,
                        tier=tier,
                        bib_entry=entry,
                        status="success",
                        notes=f"{note_prefix} via zotra /search",
                    )
                    return True
        return False

    def _try_zotra_web(url, tier, note_prefix):
        """Helper: try zotra /web + /export for a URL."""
        zotero_json = zotra_web(url)
        if zotero_json:
            biblatex = zotra_export_biblatex(zotero_json)
            if biblatex and biblatex.strip():
                if is_incollection:
                    return _make_incollection_pair(
                        biblatex, tier, f"{note_prefix} via zotra /web"
                    )
                ck = generate_cite_key(surname, year, title, used_keys)
                entry = build_entry_from_zotra(biblatex, work, ck)
                if entry:
                    result.update(
                        cite_key=ck,
                        tier=tier,
                        bib_entry=entry,
                        status="success",
                        notes=f"{note_prefix} via zotra /web",
                    )
                    return True
        return False

    # === Tier 1: Amazon ISBN via zotra /search ===
    if isbns and zotra_available and not dry_run:
        for isbn in isbns:
            if _try_zotra_search(isbn, 1, f"ISBN {isbn}"):
                return result
        result["notes"] += f"Tier 1 failed (ISBNs: {', '.join(isbns)}). "
    elif isbns and dry_run:
        result["notes"] += f"Tier 1: would try ISBNs {', '.join(isbns)}. "

    # === Tier 2: OpenLibrary ISBN via zotra /search ===
    if zotra_available and not dry_run:
        ol_isbns = openlibrary_search(surname, title)
        if ol_isbns:
            for isbn in ol_isbns[:5]:
                if _try_zotra_search(isbn, 2, f"OpenLibrary ISBN {isbn}"):
                    return result
            result["notes"] += f"Tier 2 failed ({len(ol_isbns)} ISBNs tried). "
        else:
            result["notes"] += "Tier 2: no OpenLibrary results. "
    elif dry_run:
        result["notes"] += f"Tier 2: would search OpenLibrary for '{surname}' + '{title}'. "

    # === Tier 3: CrossRef DOI via zotra /search ===
    # Try for articles, incollections, and anything with an article_title
    if zotra_available and not dry_run:
        journal = work.get("journaltitle") or work.get("work_title", "")
        # Use article_title for CrossRef search if available (it's the paper title)
        doi = crossref_search_doi(surname, search_title, journal=journal)
        if doi:
            if _try_zotra_search(doi, 3, f"DOI {doi}"):
                return result
            result["notes"] += f"Tier 3: DOI {doi} found but zotra failed. "
        else:
            result["notes"] += "Tier 3: no CrossRef DOI. "
    elif dry_run:
        result["notes"] += f"Tier 3: would search CrossRef for '{surname}' + '{search_title}'. "

    # === Tier 4: Web URL via DuckDuckGo + zotra /web ===
    # Try for newspaper/magazine articles and anything with an article_title
    if zotra_available and not dry_run and search_title:
        publication = work.get("work_title") or work.get("journaltitle", "")
        found_url = search_article_url(
            search_title, publication, surname, year
        )
        if found_url:
            if _try_zotra_web(found_url, 4, f"URL {found_url[:60]}"):
                return result
            result["notes"] += f"Tier 4: URL found but zotra failed ({found_url[:60]}). "
        else:
            result["notes"] += "Tier 4: no URL found. "
    elif dry_run:
        result["notes"] += f"Tier 4: would search DuckDuckGo for '{search_title}'. "

    # === Tier 5: Fallback from WP metadata ===
    if is_incollection:
        _make_incollection_pair(
            None, 5, (result["notes"] or "") + "Tier 5: fallback from WP metadata"
        )
    else:
        cite_key = generate_cite_key(surname, year, title, used_keys)
        entry = build_fallback_entry(work, cite_key)
        result.update(
            cite_key=cite_key,
            tier=5,
            bib_entry=entry,
            status="success",
            notes=(result["notes"] or "") + "Tier 5: fallback from WP metadata",
        )
    return result


# === Progress management ===


def load_progress():
    """Load progress from file."""
    if PROGRESS_FILE.exists():
        return json.loads(PROGRESS_FILE.read_text())
    return {"processed": {}}


def save_progress(progress):
    """Save progress to file."""
    PROGRESS_FILE.write_text(json.dumps(progress, indent=2, ensure_ascii=False))


# === Main ===


def main():
    parser = argparse.ArgumentParser(
        description="Create BibLaTeX entries for unmatched WP quotes"
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would be done without API calls or file writes",
    )
    parser.add_argument(
        "--limit", type=int, default=0, help="Process only N works (0 = all)"
    )
    parser.add_argument(
        "--reprocess-fallbacks",
        action="store_true",
        help="Re-process entries that used the WP metadata fallback (old tier 3, new tier 5)",
    )
    parser.add_argument(
        "--reprocess-incollections",
        action="store_true",
        help="Re-process incollection entries to create collection+incollection pairs with crossref",
    )
    args = parser.parse_args()

    print("=" * 60)
    print("Phase 3: Create missing BibLaTeX entries")
    print("=" * 60)

    # Check zotra
    zotra_available = False
    if not args.dry_run:
        print("\nChecking zotra server...")
        if check_zotra():
            print(f"  zotra server running at {ZOTRA_BASE}")
            zotra_available = True
        else:
            print(f"  WARNING: zotra server not reachable at {ZOTRA_BASE}")
            print("  Start it with: cd ~/source/zotra-server && npm start")
            print("  Tiers 1-4 will be skipped; all entries use Tier 5 fallback.")
            if sys.stdin.isatty():
                resp = input("  Continue anyway? [y/N] ")
                if resp.lower() != "y":
                    sys.exit(0)
            else:
                print("  Non-interactive mode: exiting.", file=sys.stderr)
                sys.exit(1)

    # Load quotes
    print("\nLoading matched quotes...")
    quotes = json.loads(MATCHED_JSON.read_text())
    print(f"  {len(quotes)} quotes loaded")

    # Load existing bib keys
    print("\nLoading existing bib entries for collision detection...")
    existing_entries = load_existing_keys(BIB_FILES)
    if MIGRATION_BIB.exists():
        for entry in _parse_bib_entries_for_keys(MIGRATION_BIB):
            existing_entries[entry["cite_key"]] = {
                "author": entry["author"],
                "title": entry["title"],
                "year": entry["year"],
            }
    print(f"  {len(existing_entries)} existing cite keys loaded")

    # Group unmatched quotes by work
    print("\nGrouping unmatched quotes by work...")
    works = group_by_work(quotes)
    total_unmatched = sum(len(w["quote_indices"]) for w in works.values())
    print(f"  {total_unmatched} unmatched quotes")
    print(f"  {len(works)} unique works")

    with_isbn = sum(1 for w in works.values() if w["isbns"])
    print(f"  {with_isbn} works with Amazon ISBNs (Tier 1 candidates)")
    print(f"  {len(works) - with_isbn} works without ISBNs (Tiers 2-5)")

    # Load progress for resume
    progress = load_progress() if not args.dry_run else {"processed": {}}

    # --reprocess-fallbacks: remove old fallback entries (tier 3 or 5) from progress
    # and also remove their cite keys from the collision set so they can be
    # re-generated without spurious suffixes.
    if args.reprocess_fallbacks and not args.dry_run:
        fallback_keys = [
            k for k, v in progress["processed"].items()
            if v.get("tier") in (1, 2, 3, 5)  # include all tiers to fix collision bug
        ]
        for k in fallback_keys:
            # Free the cite key so it can be re-generated
            old_key = progress["processed"][k].get("cite_key")
            if old_key and old_key in existing_entries:
                del existing_entries[old_key]
            # Also free any collection keys associated with this entry
            bib_text = progress["processed"][k].get("bib_entry", "")
            for m in re.finditer(r"@\w+\{([^,]+),", bib_text):
                extra_key = m.group(1).strip()
                if extra_key != old_key and extra_key in existing_entries:
                    del existing_entries[extra_key]
            del progress["processed"][k]
        print(f"\n  Reprocessing: removed {len(fallback_keys)} fallback entries from progress")
        print(f"  {len(existing_entries)} existing cite keys after cleanup")
        save_progress(progress)

    # --reprocess-incollections: restructure old incollection/collection entries
    # in-place to create collection+incollection pairs with crossref, preserving
    # any rich metadata already present (from prior zotra lookups).
    if args.reprocess_incollections and not args.dry_run:
        # First pass: build used_keys for collision detection
        _used = set(existing_entries.keys())
        for wd in progress["processed"].values():
            if wd.get("cite_key"):
                _used.add(wd["cite_key"])

        restructured = 0
        for k, v in list(progress["processed"].items()):
            bib_entry = v.get("bib_entry", "")
            if not bib_entry or "crossref" in bib_entry:
                continue

            # Identify entries that need restructuring
            needs_fix = False
            if "@incollection{" in bib_entry:
                needs_fix = True
            elif (
                "@collection{" in bib_entry
                and k in works
                and works[k].get("work_type") == "incollection"
            ):
                needs_fix = True
            if not needs_fix:
                continue

            work = works.get(k)
            if not work:
                continue

            article_title = work.get("article_title", "")
            work_title = work.get("work_title", "")
            if not article_title or not work_title:
                continue

            # Parse the existing entry to preserve its rich metadata
            parsed = parse_biblatex_entry(bib_entry)
            if not parsed:
                continue
            _, old_key, fields = parsed

            # Determine editor surname
            editor = work.get("editor", "")
            editor_surname = ""
            if "editor" in fields:
                editor_surname = extract_first_editor_surname(fields["editor"])
            elif editor:
                editor_surname = extract_first_editor_surname(editor)
            if not editor_surname:
                editor_surname = extract_surname_for_key(work["bib_author"])

            year = work.get("year", "")

            # Free old key before generating new ones
            _used.discard(old_key)

            # ---- Collection entry (preserve existing rich fields) ----
            coll_key = generate_cite_key(
                editor_surname, year, work_title, _used
            )
            coll_fields = dict(fields)
            coll_fields.pop("author", None)
            coll_fields.pop("pages", None)
            if "booktitle" in coll_fields:
                coll_fields["title"] = coll_fields.pop("booktitle")
            elif not coll_fields.get("title"):
                coll_fields["title"] = work_title
            if "editor" not in coll_fields and editor:
                coll_fields["editor"] = to_bib_editors(editor)
            if "date" not in coll_fields and year:
                coll_fields["date"] = year
            coll_fields["timestamp"] = get_timestamp()
            coll_entry = format_bib_entry("collection", coll_key, coll_fields)

            # ---- Incollection entry ----
            surname = extract_surname_for_key(work["bib_author"])
            incoll_key = generate_cite_key(
                surname, year, article_title, _used
            )
            incoll_entry = build_incollection_entry(work, incoll_key, coll_key)

            combined = coll_entry + "\n\n" + incoll_entry

            progress["processed"][k] = {
                "cite_key": incoll_key,
                "tier": v.get("tier"),
                "status": "success",
                "bib_entry": combined,
                "notes": (v.get("notes", "") or "")
                + " | restructured to collection+incollection pair",
            }

            # Update quote records
            for qidx in work["quote_indices"]:
                quotes[qidx]["new_cite_key"] = incoll_key

            restructured += 1

        if restructured:
            save_progress(progress)
        print(f"\n  Restructured {restructured} entries into collection+incollection pairs")

    already_done = len(progress["processed"])
    if already_done > 0:
        print(f"\n  Resuming: {already_done} works already processed")

    # Build used-keys set from existing + already-processed
    used_keys = set(existing_entries.keys())
    for wd in progress["processed"].values():
        if wd.get("cite_key"):
            used_keys.add(wd["cite_key"])

    # Rebuild collections dict from existing progress entries so that new
    # incollections sharing a parent collection reuse the existing key.
    collections = {}
    for wd in progress["processed"].values():
        bib_entry = wd.get("bib_entry", "")
        if not bib_entry or "@collection{" not in bib_entry:
            continue
        coll_match = re.search(r"@collection\{([^,]+),", bib_entry)
        if not coll_match:
            continue
        coll_key = coll_match.group(1)
        used_keys.add(coll_key)
        # Extract the collection's own text (up to next @entry or end)
        coll_start = coll_match.start()
        next_at = bib_entry.find("\n@", coll_start + 1)
        coll_text = (
            bib_entry[coll_start:next_at] if next_at > 0
            else bib_entry[coll_start:]
        )
        parsed = parse_biblatex_entry(coll_text)
        if parsed:
            _, _, cfields = parsed
            ed = cfields.get("editor", "")
            ttl = cfields.get("title", "")
            dt = cfields.get("date", "")
            ed_surname = extract_first_editor_surname(ed) if ed else ""
            if ed_surname and ttl:
                norm = f"{normalize(ed_surname)}|{normalize(ttl)}|{dt}"
                collections[norm] = coll_key

    # Select works to process
    work_items = list(works.items())
    if args.limit > 0:
        work_items = work_items[: args.limit]

    to_process = [(k, w) for k, w in work_items if k not in progress["processed"]]
    skipped = len(work_items) - len(to_process)

    print(f"\nProcessing {len(to_process)} works ({skipped} already done)...")
    if args.dry_run:
        print("  (DRY RUN mode)\n")

    stats = {"tier1": 0, "tier2": 0, "tier3": 0, "tier4": 0, "tier5": 0, "errors": 0}
    report_lines = []
    new_entries = []

    for i, (work_key, work) in enumerate(to_process):
        prefix = f"  [{i + 1}/{len(to_process)}]"
        author_display = work["bib_author"][:30]
        title_display = (work["title"] or "(no title)")[:40]
        print(f"{prefix} {author_display} -- {title_display}...", end="", flush=True)

        result = process_work(work_key, work, used_keys, zotra_available, args.dry_run, collections)

        if result["status"] == "success":
            tier = result["tier"]
            stats[f"tier{tier}"] += 1
            print(f" T{tier}: {result['cite_key']}")

            if result["bib_entry"]:
                new_entries.append(result["bib_entry"])

            # Update quote records
            for qidx in work["quote_indices"]:
                quotes[qidx]["new_cite_key"] = result["cite_key"]

            if args.dry_run and result["bib_entry"]:
                print(f"\n{result['bib_entry']}\n")

            # Items that used fallback should appear in report for review
            if tier == 5:
                report_lines.append(
                    f"[TIER 5 - REVIEW] {work['bib_author']} -- "
                    f"{work['title'] or '(no title)'} ({work['year'] or 'no year'})\n"
                    f"  Key: {result['cite_key']}\n"
                    f"  Attribution: {work['attribution_text'][:150]}\n"
                    f"  Notes: {result['notes']}\n"
                )
        else:
            stats["errors"] += 1
            print(f" ERROR: {result['notes']}")
            report_lines.append(
                f"[ERROR] {work['bib_author']} -- "
                f"{work['title'] or '(no title)'} ({work['year'] or 'no year'})\n"
                f"  {result['notes']}\n"
            )

        # Save progress after each work (not in dry-run)
        if not args.dry_run:
            progress["processed"][work_key] = {
                "cite_key": result["cite_key"],
                "tier": result["tier"],
                "status": result["status"],
                "bib_entry": result["bib_entry"],
                "notes": result["notes"],
            }
            save_progress(progress)

    # === Summary ===
    print(f"\n{'=' * 60}")
    print("RESULTS")
    print(f"{'=' * 60}")
    print(f"  Tier 1 (Amazon ISBN):     {stats['tier1']:>5}")
    print(f"  Tier 2 (OpenLibrary):     {stats['tier2']:>5}")
    print(f"  Tier 3 (CrossRef DOI):    {stats['tier3']:>5}")
    print(f"  Tier 4 (Web URL):         {stats['tier4']:>5}")
    print(f"  Tier 5 (WP fallback):     {stats['tier5']:>5}")
    print(f"  Skipped (already done):   {skipped:>5}")
    print(f"  Errors:                   {stats['errors']:>5}")
    print(f"  {'~' * 40}")
    total = stats["tier1"] + stats["tier2"] + stats["tier3"] + stats["tier4"] + stats["tier5"]
    print(f"  Total processed:          {total:>5}")

    if args.dry_run:
        print("\n  (DRY RUN -- no files written)")
        return

    # === Write output files ===

    # Collect ALL entries (from progress, including previous runs)
    all_entries = []
    for wd in progress["processed"].values():
        if wd.get("status") == "success" and wd.get("bib_entry"):
            all_entries.append(wd["bib_entry"])

    if all_entries:
        header = (
            "% migration.bib -- Auto-generated BibLaTeX entries for WP quote migration\n"
            f"% Generated: {datetime.now().strftime('%Y-%m-%d %H:%M')}\n"
            f"% Entries: {len(all_entries)}\n"
        )
        content = header + "\n" + "\n\n".join(all_entries) + "\n"
        MIGRATION_BIB.write_text(content)
        print(f"\n  Bib file written: {MIGRATION_BIB}")
        print(f"  ({len(all_entries)} total entries)")

    # Update wp-quotes-matched.json
    MATCHED_JSON.write_text(json.dumps(quotes, indent=2, ensure_ascii=False))
    print(f"  Updated: {MATCHED_JSON}")

    # Write report
    if report_lines:
        report_content = (
            f"BibLaTeX creation report -- {datetime.now().strftime('%Y-%m-%d %H:%M')}\n"
            f"{'=' * 60}\n\n"
            f"Tier 1 (Amazon ISBN):  {stats['tier1']}\n"
            f"Tier 2 (OpenLibrary):  {stats['tier2']}\n"
            f"Tier 3 (CrossRef DOI): {stats['tier3']}\n"
            f"Tier 4 (Web URL):      {stats['tier4']}\n"
            f"Tier 5 (WP fallback):  {stats['tier5']}\n"
            f"Errors: {stats['errors']}\n\n"
            f"Items needing manual review (Tier 5 fallbacks):\n"
            f"{'=' * 60}\n\n"
        )
        report_content += "\n".join(report_lines)
        REPORT_FILE.write_text(report_content)
        print(f"  Report: {REPORT_FILE} ({len(report_lines)} items)")

    print("\nDone!")


if __name__ == "__main__":
    main()
