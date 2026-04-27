#!/usr/bin/env python3
"""Rebuild migration.bib entries from the original WordPress export.

The auto-generated entries in ``~/My Drive/bibliography/migration.bib`` were
populated by an earlier OCLC-based lookup that often returned the wrong
record. The original WordPress quotes export (preserved at
``scripts/wp-quotes-matched.json``) carries the authoritative attribution
data for each post.

This script walks every cite key that lives only in migration.bib, pairs it
with the WP posts that map to it, and emits a proposed replacement BibTeX
entry only when the existing migration entry is *provably wrong* relative
to the WP data (mismatching author surname or no title overlap). Entries
that are merely sparse but plausibly correct are preserved, since they
often carry later enrichment (URL, ISBN, file path) that we don't want to
discard.

Output is staged to ``~/My Drive/bibliography/migration-rebuilt.bib`` and a
side-by-side report is written to ``scripts/migration-rebuild-report.txt``
so the diff can be reviewed before anything is moved into ``new.bib``.
"""

import json
import re
import sys
import unicodedata
from collections import Counter, defaultdict
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
from lib import BIB_FILES, cite_key_to_slug  # noqa: E402

REPO_ROOT = Path(__file__).resolve().parent.parent
WP_MATCHED = REPO_ROOT / "scripts" / "wp-quotes-matched.json"
MIGRATION_BIB = Path.home() / "My Drive/bibliography/migration.bib"
STAGED_BIB = Path.home() / "My Drive/bibliography/migration-rebuilt.bib"
REPORT = REPO_ROOT / "scripts" / "migration-rebuild-report.txt"

BIB_KEY_RE = re.compile(r"^@(\w+)\s*\{\s*([^,\s]+)\s*,", re.MULTILINE)
ENTRY_SPLIT_RE = re.compile(r"(?=^@\w+\s*\{)", re.MULTILINE)


def load_bib_entries(path: Path) -> dict[str, str]:
    if not path.exists():
        return {}
    text = path.read_text(encoding="utf-8", errors="replace")
    out = {}
    for chunk in ENTRY_SPLIT_RE.split(text):
        m = BIB_KEY_RE.match(chunk)
        if m:
            out[m.group(2)] = chunk.rstrip() + "\n"
    return out


def keys_per_bib() -> dict[str, set[str]]:
    """Return {cite_key: {bib_label, ...}}."""
    out: dict[str, set[str]] = {}
    for path in BIB_FILES:
        if not path.exists():
            continue
        label = path.stem
        text = path.read_text(encoding="utf-8", errors="replace")
        for m in BIB_KEY_RE.finditer(text):
            out.setdefault(m.group(2), set()).add(label)
    return out


_EDITOR_SUFFIX_RE = re.compile(
    r"\s*\((?:ed\.?|eds\.?|editor|editors|trans\.?|translator|hrsg\.?)\)\s*",
    re.IGNORECASE,
)
_EDITOR_PREFIX_RE = re.compile(
    r"^\s*\((?:ed\.?|eds\.?|editor|editors|trans\.?|translator|hrsg\.?)\)[,\s]*",
    re.IGNORECASE,
)


def split_authors(raw: str) -> list[str]:
    """Parse ``First Last & Other Person`` → ``['Last, First', 'Person, Other']``."""
    if not raw:
        return []
    # Strip trailing/leading "(ed.)" annotations before splitting
    raw = _EDITOR_SUFFIX_RE.sub(" ", raw)
    raw = _EDITOR_PREFIX_RE.sub("", raw)
    parts = re.split(r"\s+(?:&|and)\s+", raw)
    flat = []
    for p in parts:
        flat.extend(s.strip() for s in p.split(",") if s.strip())
    cleaned = []
    for name in flat:
        name = _EDITOR_SUFFIX_RE.sub("", name).strip()
        if name:
            cleaned.append(flip_name(name))
    return cleaned


# Compound surnames (Spanish/Portuguese/Italian double-barreled, particled
# names). Last token alone is not the surname for these.
COMPOUND_SURNAMES = {
    "Bioy Casares", "García Márquez", "Vargas Llosa", "Lloyd Wright",
    "Conan Doyle", "Lévi-Strauss", "Stephens-Davidowitz",
    "Cervantes Saavedra", "Halperín Donghi", "Pérez Reverte",
    "Ortega y Gasset", "García Lorca", "Pérez Galdós",
}


def flip_name(name: str) -> str:
    """``First Middle Last`` → ``Last, First Middle``.

    Particles (``de``, ``van``, ``von``, ``del``...) are kept with the given
    name in BibTeX convention: ``Miguel de Cervantes`` → ``Cervantes, Miguel
    de``. We do *not* try to attach particles to the surname; we only honour
    the explicit compound-surname allowlist above for cases where particle
    placement is genuinely ambiguous.
    """
    name = name.strip().rstrip(",")
    if not name or "," in name:
        return name
    parts = name.split()
    if len(parts) == 1:
        return parts[0]
    for compound in COMPOUND_SURNAMES:
        toks = compound.split()
        n = len(toks)
        if len(parts) >= n + 1 and parts[-n:] == toks:
            given = " ".join(parts[:-n])
            return f"{compound}, {given}"
    return f"{parts[-1]}, {' '.join(parts[:-1])}"


def cite_key_surname(cite_key: str) -> str:
    """Extract the surname-shaped prefix of a cite key.

    ``Alexander2014ControlGroupOut`` → ``Alexander``
    ``BioyCasares2002AdPorcos``      → ``BioyCasares``
    ``Stephens-Davidowitz2017...``   → ``Stephens-Davidowitz``
    ``CiceroAmicitia``               → ``Cicero``  (no year — first CamelCase chunk)
    """
    # Year-bounded keys: surname is everything up to the year
    m = re.match(r"^(.+?)(?=\d{4})", cite_key)
    if m:
        return m.group(1)
    # No year — surname is the first CamelCase chunk (capital + lowercase)
    m = re.match(r"^([A-Z][a-z]+)", cite_key)
    return m.group(1) if m else ""


def cite_key_title_words(cite_key: str) -> list[str]:
    """Extract the CamelCase title fragment, lowercased.

    With year: title is everything after ``\\d{4}``.
    Without year: title is everything after the first CamelCase chunk.
    """
    m = re.search(r"\d{4}([A-Z][\w-]*)$", cite_key)
    if m:
        tail = m.group(1)
    else:
        m = re.match(r"^[A-Z][a-z]+([A-Z][\w-]*)$", cite_key)
        if not m:
            return []
        tail = m.group(1)
    words = re.findall(r"[A-Z][a-z]+|[A-Z]+(?=[A-Z]|$)", tail)
    return [w.lower() for w in words]


def get_field(entry: str, field: str) -> str:
    """Pull a single BibTeX field value from a raw entry string.

    NFC-normalizes the result so that decomposed combining marks (common in
    auto-generated bib output) don't break downstream `\\w+` matching.
    """
    m = re.search(rf"^\s*{field}\s*=\s*\{{(.*?)\}},?\s*$", entry,
                  re.MULTILINE | re.DOTALL)
    if not m:
        return ""
    raw = re.sub(r"\s+", " ", m.group(1)).strip()
    return unicodedata.normalize("NFC", raw)


# Title noise we strip before fuzzy matching
_NORMALIZE_RE = re.compile(r"[^a-z0-9]+")


def normalize_word(s: str) -> str:
    """Lowercase + fold accents to ASCII + strip non-alphanumeric.

    Critical that accents fold rather than drop: cite keys typically strip
    accents (``Bernardez``) but bib entries preserve them (``Bernárdez``).
    """
    decomposed = unicodedata.normalize("NFKD", s)
    ascii_only = decomposed.encode("ascii", "ignore").decode("ascii")
    return _NORMALIZE_RE.sub("", ascii_only.lower())


def migration_entry_is_plausible(entry: str, cite_key: str,
                                 wp_posts: list[dict]) -> tuple[bool, str]:
    """Decide whether the existing migration entry matches the cite key.

    Returns (is_plausible, reason). The reason string starts with one of:
      - "plausible"
      - "author mismatch:" — entry's author is for a different work
      - "title mismatch:"  — author overlaps but title fields don't match
      - "all-caps title"   — raw OCLC migration record never enriched
      - "placeholder location" — "Place of publication not identified"
    Callers can branch on the prefix to decide whether to preserve any
    enrichment fields (URL, ISBN, DOI, editor) from the original.
    """
    surname_key = cite_key_surname(cite_key)
    if not surname_key:
        return True, "plausible (no surname in key)"

    author = get_field(entry, "author") + " " + get_field(entry, "editor")
    title = (get_field(entry, "title") + " "
             + get_field(entry, "shorttitle") + " "
             + get_field(entry, "booktitle") + " "
             + get_field(entry, "journaltitle"))

    surname_norm = normalize_word(surname_key)
    author_norm = normalize_word(author)
    surname_present = surname_norm and surname_norm in author_norm
    if not surname_present:
        wp_authors = {normalize_word(p.get("author", "")) for p in wp_posts}
        if wp_authors and not any(a and a in author_norm for a in wp_authors):
            return False, f"author mismatch: key={surname_key!r} entry author={author!r}"

    key_words = set(cite_key_title_words(cite_key))
    if key_words:
        title_words = {normalize_word(w) for w in re.findall(r"\w+", title)}
        title_words.discard("")
        overlap = {w for w in key_words if w in title_words}
        if not overlap:
            wp_title_words = set()
            for p in wp_posts:
                for src in (p.get("work_title", ""), p.get("article_title", "")):
                    wp_title_words.update(normalize_word(w)
                                          for w in re.findall(r"\w+", src))
            wp_overlap = {w for w in key_words if w in wp_title_words}
            if wp_overlap:
                return False, (f"title mismatch: key words={sorted(key_words)} "
                               f"absent from entry title={title!r}")

    raw_title = get_field(entry, "title")
    if raw_title and raw_title.isupper():
        return False, "all-caps title (unenriched migration)"

    location = get_field(entry, "location")
    if "not identified" in location.lower():
        return False, "placeholder location"

    return True, "plausible"


def extract_editors_from_attribution(attribution: str) -> str:
    """Pull editor names from ``"X (ed.)"`` / ``"X and Y (eds.)"`` patterns.

    The WP attribution often labels the book editor explicitly:
      ``Speaker, in Ray Carney (ed.), Title, ...``
      ``Speaker, quoted in Paul Edwards (ed.), Author, Title, ...``
    """
    if not attribution:
        return ""
    m = re.search(
        r"(?:in|by)\s+([A-ZÁÉÍÓÚÑÜ][^,()]*?(?:\s+(?:&|and)\s+[^,()]+)*?)\s*"
        r"\((?:ed\.?|eds\.?|editor|editors)\)",
        attribution, re.IGNORECASE)
    if not m:
        return ""
    return " and ".join(split_authors(m.group(1).strip()))


def extract_authors_from_attribution(attribution: str, fallback: str) -> str:
    """Return the BibTeX-formatted author list.

    Patterns handled:
      ``Speaker, quoted in Author, Title, ...``         → author = Author
      ``Speaker, quoted in Editor (ed.), Author, ...``  → author = Author
      ``Speaker, in Editor (ed.), Title, ...``          → author = Speaker
      ``Author1 & Author2, Title, ...``                 → author = both
      ``Author, 'Article', ...``                        → author = Author
    """
    if not attribution:
        return " and ".join(split_authors(fallback))
    # "X, quoted in Y (ed.), Z, Title, ..." → author = Z (the work's author)
    qm = re.search(
        r"\bquoted in\s+[^,()]+\s*\((?:ed\.?|eds\.?|editor|editors)\)\s*,\s*"
        r"([^,]+(?:\s+(?:&|and)\s+[^,]+)*)",
        attribution, re.IGNORECASE)
    if qm:
        return " and ".join(split_authors(qm.group(1).strip()))
    # "X, quoted in Y, Title, ..." → author = Y
    qm = re.search(r"\bquoted in\s+([^,]+(?:\s+(?:&|and)\s+[^,]+)*)",
                   attribution, re.IGNORECASE)
    if qm:
        return " and ".join(split_authors(qm.group(1).strip()))
    # Default: head of attribution up to the title (quoted segment)
    m = re.match(r"^(.+?)\s*[,]\s*(?:'|\")", attribution)
    candidate = m.group(1) if m else None
    if not candidate:
        candidate = attribution.split(",", 1)[0]
    if "&" in candidate or re.search(r"\sand\s", candidate):
        return " and ".join(split_authors(candidate))
    if fallback:
        return " and ".join(split_authors(fallback))
    return " and ".join(split_authors(candidate))


# Locations that frequently appear in the attribution_text. We only accept
# location segments matching a curated allowlist; otherwise we skip rather
# than risk emitting non-place strings (article title fragments, etc.).
KNOWN_LOCATIONS = {
    "Amsterdam", "Athens", "Atlanta", "Auckland", "Austin", "Baltimore",
    "Barcelona", "Basel", "Basingstoke", "Berkeley", "Berlin", "Birmingham",
    "Bloomington", "Bogotá", "Bologna", "Boston", "Boulder", "Bristol",
    "Brookfield", "Brussels", "Buenos Aires", "Cambridge", "Canberra",
    "Cape Town", "Chapel Hill", "Charlotte", "Charlottesville", "Chicago",
    "Cincinnati", "Cleveland", "Cologne", "Copenhagen", "Cordoba",
    "Córdoba", "Dallas", "Delhi", "Denver", "Dordrecht", "Dublin",
    "Durham", "Edinburgh", "Eugene", "Florence", "Frankfurt", "Geneva",
    "Glasgow", "Göttingen", "Hamburg", "Helsinki", "Hoboken", "Hong Kong",
    "Houston", "Indianapolis", "Ithaca", "Jerusalem", "Kingston", "Lanham",
    "Las Vegas", "Leiden", "Liverpool", "London", "Los Angeles", "Madison",
    "Madrid", "Manchester", "Mexico City", "Miami", "Milan", "Minneapolis",
    "Montevideo", "Montreal", "Moscow", "Munich", "Nashville", "New Delhi",
    "New Haven", "New Orleans", "New York", "Norman", "Oakland", "Ottawa",
    "Oxford", "Paris", "Philadelphia", "Phoenix", "Pittsburgh", "Portland",
    "Prague", "Princeton", "Providence", "Quito", "Reading", "Reno",
    "Reykjavik", "Richmond", "Rio de Janeiro", "Rome", "Rotterdam",
    "Salzburg", "San Diego", "San Francisco", "San Juan", "Santa Barbara",
    "Santa Fe", "Santa Monica", "Santiago", "São Paulo", "Seattle",
    "Seoul", "Singapore", "South Bend", "South Holland", "Stanford",
    "Stockholm", "Strasbourg", "Stuttgart", "Sydney", "Syracuse",
    "Taipei", "Tallahassee", "Tel Aviv", "Tokyo", "Toronto", "Tübingen",
    "Tucson", "Turin", "Urbana", "Valencia", "Vancouver", "Vienna",
    "Warsaw", "Washington", "Wellington", "Westport", "Wiesbaden",
    "Worcester", "Zurich",
}


def extract_location(attribution: str, year: str) -> str:
    """Pull the publisher location from a 'Author, Title, Place, Year, p. X' chain."""
    if not attribution or not year:
        return ""
    # Look at the segment immediately before the year
    pat = re.compile(rf",\s*([^,]+?)\s*,\s*{re.escape(year)}\b")
    m = pat.search(attribution)
    if not m:
        return ""
    candidate = m.group(1).strip().strip("'\"")
    # Strip trailing US state ("Princeton, NJ" → preserve as-is; "Lanham, MD"
    # often appears split across two segments)
    if candidate in KNOWN_LOCATIONS:
        return candidate
    # Try removing a state code: "South Holland, Illinois"
    head = candidate.split(",")[0].strip()
    if head in KNOWN_LOCATIONS:
        return candidate  # keep the qualifier
    return ""


def extract_volume_issue(attribution: str) -> tuple[str, str, str]:
    """Return (volume, number, issue_date) parsed from journal-style attribution."""
    vol = num = idate = ""
    m = re.search(r"\bvol\.\s*(\d+)", attribution)
    if m:
        vol = m.group(1)
    m = re.search(r"\bno\.\s*([\w/]+)", attribution)
    if m:
        num = m.group(1)
    m = re.search(r"\(([^)]+)\)", attribution)
    if m:
        idate = m.group(1).strip()
    return vol, num, idate


def parse_locator_pages(locator: str) -> str:
    """Convert ``p. 18``/``pp. 67-68`` to a BibTeX-friendly pages range.

    Returned for reference only; pages belong on the citation, not the work
    entry, so we don't write this into the rebuilt bib. Kept here in case we
    later want to flag suspicious locators.
    """
    if not locator:
        return ""
    m = re.match(r"^p+\.\s*(.+)$", locator.strip())
    return m.group(1) if m else locator.strip()


_NEWSPAPER_WORDS = (
    "Times", "Post", "Tribune", "Globe", "Herald", "Gazette", "Guardian",
    "Independent", "Observer", "Telegraph", "Standard", "Statesman",
    "Mail", "Chronicle", "Express", "Sun", "Mirror", "Daily", "Weekly",
    "Newspaper", "News",
    "Nación", "Nacion", "Clarín", "Clarin", "País", "Pais", "Mundo",
    "Crónica", "Cronica", "Página", "Pagina",
)
_JOURNAL_WORDS = (
    "Review", "Journal", "Quarterly", "Magazine", "Inquiry", "Mind",
    "Analysis", "Studies", "Bulletin", "Notes", "Annals", "Transactions",
    "Proceedings",
)
_ONLINE_DOMAINS_RE = re.compile(r"\b\w+\.(?:org|com|net|info|blog|io)\b",
                                re.IGNORECASE)
_KNOWN_BLOGS = {
    "Slate Star Codex", "LessWrong", "Less Wrong", "Astral Codex Ten",
    "Overcoming Bias", "Marginal Revolution", "Shtetl-Optimized",
    "Paul Graham", "Eliezer Yudkowsky", "ZNet", "Edge", "Edge.org",
    "edge.org", "Wikiquote", "Wikipedia",
}
_BOOK_MARKERS = (
    "Dictionary", "Encyclopedia", "Encyclopaedia", "Companion",
    "Handbook", "Guide", "Reader", "Anthology", "Collected Works",
    "Selected Works", "Essays", "Lectures",
)


def container_kind(work_title: str, attribution: str) -> str:
    """Classify the container: 'journal', 'newspaper', 'online', or 'collection'.

    Drives entry-type selection. Collections become @incollection (with
    booktitle), journals/newspapers become @article (with journaltitle),
    online sources become @online.
    """
    if not work_title:
        return "online"
    # Explicit periodical markers in attribution: vol/no
    if re.search(r"\bvol\.\s*\d+|\bno\.\s*\w+", attribution):
        return "journal"
    # Reference works (Penguin Dictionary etc.) are collections
    if any(m in work_title for m in _BOOK_MARKERS):
        return "collection"
    if work_title in _KNOWN_BLOGS:
        return "online"
    if _ONLINE_DOMAINS_RE.search(work_title):
        return "online"
    title_words = work_title.split()
    if any(w in title_words for w in _NEWSPAPER_WORDS):
        return "newspaper"
    if any(w in title_words for w in _JOURNAL_WORDS):
        return "journal"
    # Short, no location in attribution → likely periodical/online not a book
    if len(title_words) <= 3 and not _attribution_has_location(attribution):
        return "online"
    return "collection"


def _attribution_has_location(attribution: str) -> bool:
    """Detect whether attribution_text carries a publisher-place segment.

    Pattern is ``..., Place, YYYY, p. X``. We look for any ``,
    <KnownLocation>, <year>`` triple.
    """
    return bool(re.search(
        r",\s*([A-Z][A-Za-zÀ-ÿ\s.]+?)\s*,\s*\d{4}\b", attribution))


def escape_bib(value: str) -> str:
    """Minimal escape; the value will sit between curly braces."""
    return value.replace("\\", "\\\\").strip()


def render_entry(entry_type: str, key: str, fields: dict[str, str]) -> str:
    lines = [f"@{entry_type}{{{key},"]
    # Stable, readable field order
    order = ("author", "editor", "title", "booktitle", "journaltitle",
             "shorttitle", "subtitle", "date", "year", "volume", "number",
             "issue", "edition", "publisher", "location", "url", "urldate",
             "doi", "isbn", "note", "crossref")
    seen = set()
    for k in order:
        v = fields.get(k)
        if v:
            lines.append(f"\t{k} = {{{escape_bib(v)}}},")
            seen.add(k)
    for k, v in fields.items():
        if k in seen or not v:
            continue
        lines.append(f"\t{k} = {{{escape_bib(v)}}},")
    if lines[-1].endswith(","):
        lines[-1] = lines[-1][:-1]
    lines.append("}")
    return "\n".join(lines) + "\n"


def recover_full_quoted_title(attribution: str, truncated: str) -> str:
    """Recover an article title that WP's parser truncated at an apostrophe.

    The upstream WP migration parsed ``article_title`` by splitting on the
    first ``'``, so titles like ``"One Man's View"`` ended up as
    ``"One man"``. The full title still lives in attribution_text inside a
    matched pair of quotes. We pull the longest quoted span that *starts
    with* the truncated text.
    """
    if not attribution or not truncated:
        return ""
    # Match anything between a leading single quote and a closing quote that
    # is followed by a comma, period, or end-of-string (so internal
    # apostrophes are skipped).
    cand = ""
    for m in re.finditer(r"'(.+?)'(?=[,.]|\s+in\s+|$)", attribution):
        text = m.group(1).strip()
        if text.lower().startswith(truncated.lower()) and len(text) > len(cand):
            cand = text
    return cand


def select_primary_post(cite_key: str, posts: list[dict]) -> dict:
    """For a cite key collapsed across multiple posts, pick the one whose
    article_title best matches the cite key's title fragment. This recovers
    the originally-intended single-article cite key from the migration's
    over-collapse."""
    if len(posts) == 1:
        return posts[0]
    key_words = set(cite_key_title_words(cite_key))
    if not key_words:
        return posts[0]
    best = posts[0]
    best_score = -1
    for p in posts:
        candidate = (p.get("article_title", "") + " "
                     + p.get("work_title", ""))
        words = {normalize_word(w) for w in re.findall(r"\w+", candidate)}
        score = len(key_words & words)
        if score > best_score:
            best_score = score
            best = p
    return best


def build_entry(cite_key: str, posts: list[dict],
                original: str = "") -> tuple[str | None, str]:
    """Return (rendered BibTeX entry, status note).

    ``original`` is the raw text of the existing migration entry, if any.
    Editor / translator / URL / DOI / ISBN values are preserved from it
    when present, since the WP attribution rarely carries that information.
    """
    if not posts:
        return None, "no WP posts"

    # Case-insensitive deduplication so trivial differences like
    # "Borges Verbal" vs "Borges verbal" don't trigger spurious conflicts.
    def dedup_ci(values):
        seen = {}
        for v in values:
            k = v.lower().strip()
            if k and k not in seen:
                seen[k] = v
        return list(seen.values())

    authors = dedup_ci([p.get("author", "") for p in posts])
    work_titles = dedup_ci([p.get("work_title", "") for p in posts])
    years = dedup_ci([p.get("year", "") for p in posts])
    article_titles = dedup_ci([p.get("article_title", "") for p in posts])

    multi_post = len(posts) > 1
    article_conflict = multi_post and len(article_titles) > 1
    work_conflict = len(work_titles) > 1
    author_conflict = len(authors) > 1
    year_conflict = len(years) > 1

    if work_conflict or author_conflict or year_conflict:
        return None, (f"conflict: authors={authors!r} works={work_titles!r} "
                      f"years={years!r}")

    if article_conflict:
        # Migration script collapsed multiple distinct articles into one
        # cite key. Recover the article that matches the cite key's title
        # fragment best; the others will need separate cite keys later.
        primary = select_primary_post(cite_key, posts)
        article_titles = [primary.get("article_title", "")]
    else:
        primary = posts[0]
    attribution = primary.get("attribution_text", "") or ""
    work_title = work_titles[0] if work_titles else ""
    # Strip parser-artifact work titles inherited from the WP migration
    # (e.g. "et al", produced when the original WP attribution lacked any
    # real work title and the parser grabbed a trailing fragment).
    if work_title.strip(".").strip().lower() in ("et al", "ibid", "id"):
        work_title = ""
    article_title = article_titles[0] if article_titles else ""
    year = years[0] if years else ""
    author_field = extract_authors_from_attribution(
        attribution, primary.get("author", "")
    )

    fields: dict[str, str] = {}
    if author_field:
        fields["author"] = author_field
    editor_field = extract_editors_from_attribution(attribution)
    # Avoid editor == author duplication
    if editor_field and normalize_word(editor_field) != normalize_word(author_field):
        fields["editor"] = editor_field
    if year:
        fields["date"] = year

    # Recover full article title if WP truncated it at an apostrophe
    if article_title:
        recovered = recover_full_quoted_title(attribution, article_title)
        if recovered:
            article_title = recovered
    elif not article_title and attribution:
        # WP sometimes drops article_title entirely. Look for a single
        # quoted segment immediately followed by ", in <Editor> (ed.)".
        # Tolerate a missing closing quote after a terminal '?' or '!'
        # (some WP attributions have ?, instead of ?', a typo carried over
        # from the original WordPress posts).
        m = re.search(
            r"'([^']{4,80}?(?:[?!]|[^?!]))'?\s*,?\s*in\s+[^,]+?\s*"
            r"\((?:ed\.?|eds\.?|editor|editors)\)",
            attribution)
        if m:
            cand = m.group(1).strip()
            # Reject obvious garbage: leading punctuation, embedded comma
            if not cand.startswith((",", ".", ";")) and "," not in cand[:60]:
                article_title = cand

    # Decide entry type
    if article_title:
        kind = container_kind(work_title, attribution)
        if kind == "journal" or kind == "newspaper":
            entry_type = "article"
            fields["title"] = article_title
            if work_title:
                fields["journaltitle"] = work_title
            vol, num, idate = extract_volume_issue(attribution)
            if vol:
                fields["volume"] = vol
            if num:
                fields["number"] = num
            if idate and idate != year:
                fields["issue"] = idate
        elif kind == "online":
            entry_type = "online"
            fields["title"] = article_title
            if work_title:
                fields["journaltitle"] = work_title
        else:  # collection
            entry_type = "incollection"
            fields["title"] = article_title
            fields["booktitle"] = work_title
            loc = extract_location(attribution, year)
            if loc:
                fields["location"] = loc
    elif work_title:
        entry_type = "book"
        fields["title"] = work_title
        loc = extract_location(attribution, year)
        if loc:
            fields["location"] = loc
    else:
        return None, "no title information"

    # Preserve safe enrichment fields from the existing migration entry
    # (editor, translator, DOI, ISBN, URL/urldate, PDF file path). These
    # rarely come from WP attribution and would otherwise be lost.
    if original:
        # When rebuilding to @incollection from an original whose `author`
        # is structurally an editor (cite key surname differs but the
        # original describes the same book), promote the original author
        # to `editor` on the rebuilt entry.
        if entry_type == "incollection" and not get_field(original, "editor"):
            orig_author = get_field(original, "author")
            new_author_norm = normalize_word(fields.get("author", ""))
            orig_author_norm = normalize_word(orig_author)
            if orig_author and orig_author_norm not in new_author_norm:
                fields["editor"] = orig_author
        for preserve in ("editor", "translator", "doi", "isbn", "url",
                         "urldate", "file"):
            existing_val = get_field(original, preserve)
            if existing_val and preserve not in fields:
                if preserve == "url" and _is_junk_url(existing_val):
                    continue
                fields[preserve] = existing_val

    # Strip any author names that leaked into the editor field (the work's
    # author is never their own editor in practice).
    if fields.get("editor") and fields.get("author"):
        fields["editor"] = _strip_names_overlapping_author(
            fields["editor"], fields["author"])
        if not fields["editor"]:
            del fields["editor"]

    return render_entry(entry_type, cite_key, fields), "ok"


def _strip_names_overlapping_author(editor: str, author: str) -> str:
    """Return ``editor`` with any name segments that match ``author`` removed."""
    author_norms = {normalize_word(a) for a in author.split(" and ")}
    kept = []
    for name in editor.split(" and "):
        if normalize_word(name) in author_norms:
            continue
        kept.append(name)
    return " and ".join(kept)


def _original_describes_same_work(original: str, wp_posts: list[dict]) -> bool:
    """True when the original entry's title sufficiently overlaps the WP
    work_title (the *book*-level title, not the article title).

    Used to detect interview / edited-collection cases where the original
    has the book's *editor* in the ``author`` field — those people belong
    on the rebuilt entry as ``editor``, not as discarded data.
    """
    orig_title = (get_field(original, "title") + " "
                  + get_field(original, "shorttitle"))
    if not orig_title.strip():
        return False
    orig_words = {normalize_word(w) for w in re.findall(r"\w+", orig_title)
                  if len(w) >= 4}
    orig_words.discard("")
    if not orig_words:
        return False
    # Only compare against work_title (the container), since article_title
    # is the chapter and would never overlap a separately-titled book.
    wp_words: set[str] = set()
    for p in wp_posts:
        for w in re.findall(r"\w+", p.get("work_title", "") or ""):
            if len(w) >= 4:
                wp_words.add(normalize_word(w))
    if not wp_words:
        return False
    overlap = orig_words & wp_words
    # Same-work threshold: at least 2 shared content words, OR all
    # work_title content words appear in the original.
    return len(overlap) >= 2 or overlap == wp_words


def _is_junk_url(url: str) -> bool:
    """Drop URLs that clearly point at search-result / disambiguation pages."""
    junk_substrings = (
        "abebooks.com/book-search",
        "casadellibro.com",
        "wikiquote.org",
        "stafforini.com/quotes/?",
        "stafforini.com/quotes/?tag=",
        "lanacion.com.ar/autor/",
        "/search?",
        "altchaNotVerified",
    )
    return any(s in url for s in junk_substrings)


def main() -> int:
    if not WP_MATCHED.exists():
        print(f"FATAL: missing {WP_MATCHED}", file=sys.stderr)
        return 1

    print("Loading bib files...")
    membership = keys_per_bib()
    migration_only = {k for k, v in membership.items() if v == {"migration"}}
    print(f"  Migration-only cite keys: {len(migration_only)}")

    print("Loading WordPress export...")
    matched = json.loads(WP_MATCHED.read_text(encoding="utf-8"))
    posts_by_key: dict[str, list[dict]] = defaultdict(list)
    for q in matched:
        k = q.get("new_cite_key")
        if k:
            posts_by_key[k].append(q)
    print(f"  WP posts with cite keys: {len(matched)}")

    print("Loading existing migration.bib entries (for fallback)...")
    migration_entries = load_bib_entries(MIGRATION_BIB)

    rebuilt: dict[str, str] = {}
    statuses: Counter[str] = Counter()
    flagged: list[tuple[str, str]] = []
    plausible_kept: list[tuple[str, str]] = []

    for key in sorted(migration_only):
        posts = posts_by_key.get(key, [])
        if not posts:
            statuses["no WP coverage"] += 1
            continue
        existing = migration_entries.get(key, "")
        preserve_from = ""
        if existing:
            ok, why = migration_entry_is_plausible(existing, key, posts)
            if ok:
                statuses["kept (plausible existing)"] += 1
                plausible_kept.append((key, why))
                continue
            # Decide whether to preserve enrichment fields:
            #  - Title mismatch / placeholder location: original is the same
            #    work, just structured wrong → preserve everything.
            #  - Author mismatch: usually means the original is a totally
            #    different work, BUT if the original's title matches the WP
            #    work_title, the original is actually the same work with the
            #    book's editor stored in `author`. Preserve in that case so
            #    we can promote that author to `editor`.
            if why.startswith("title mismatch") or why.startswith("placeholder"):
                preserve_from = existing
            elif why.startswith("author mismatch"):
                if _original_describes_same_work(existing, posts):
                    preserve_from = existing
        entry, note = build_entry(key, posts, original=preserve_from)
        if entry:
            rebuilt[key] = entry
            statuses["rebuilt"] += 1
        else:
            statuses[f"flagged: {note.split(':')[0]}"] += 1
            flagged.append((key, note))

    print()
    print("=== Summary ===")
    for status, count in statuses.most_common():
        print(f"  {count:5d}  {status}")

    print()
    print(f"Writing {len(rebuilt)} proposed entries → {STAGED_BIB}")
    STAGED_BIB.parent.mkdir(parents=True, exist_ok=True)
    with STAGED_BIB.open("w", encoding="utf-8") as f:
        f.write("% Auto-generated by scripts/rebuild-migration-bib.py\n")
        f.write("% Source: WordPress export (scripts/wp-quotes-matched.json)\n")
        f.write("% Review before merging into bibliography/new.bib\n\n")
        for key in sorted(rebuilt):
            f.write(rebuilt[key])
            f.write("\n")

    print(f"Writing side-by-side report → {REPORT}")
    with REPORT.open("w", encoding="utf-8") as f:
        f.write("# migration-bib rebuild report\n\n")
        f.write(f"Rebuilt: {len(rebuilt)} / {len(migration_only)}\n")
        f.write(f"Flagged: {len(flagged)}\n")
        f.write(f"No WP coverage: {statuses.get('no WP coverage', 0)}\n\n")

        f.write("## Side-by-side: original migration entry vs rebuilt entry\n\n")
        for key in sorted(rebuilt):
            f.write(f"--- {key} ---\n")
            f.write("ORIGINAL:\n")
            f.write(migration_entries.get(key, "(missing)\n"))
            f.write("REBUILT:\n")
            f.write(rebuilt[key])
            f.write("\n")

        f.write("\n## Flagged for human review\n\n")
        for key, note in sorted(flagged):
            f.write(f"--- {key} ---\n")
            f.write(f"{note}\n")
            f.write("ORIGINAL:\n")
            f.write(migration_entries.get(key, "(missing)\n"))
            f.write("\n")

        f.write("\n## Kept (existing migration entry deemed plausible)\n\n")
        for key, why in sorted(plausible_kept):
            f.write(f"  {key}  ({why})\n")

    print()
    print("Done. Next: review the report, sanity-check rebuilt entries, then")
    print("merge bibliography/migration-rebuilt.bib into bibliography/new.bib")
    print("and remove the corresponding entries from bibliography/migration.bib.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
