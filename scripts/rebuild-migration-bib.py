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


def split_authors(raw: str) -> list[str]:
    """Parse ``First Last & Other Person`` → ``['Last, First', 'Person, Other']``."""
    if not raw:
        return []
    # Split on ' & ', ' and ', or commas-followed-by-name (multiple authors)
    parts = re.split(r"\s+(?:&|and)\s+", raw)
    flat = []
    for p in parts:
        # Some attributions use commas to separate multiple authors:
        # "Mark Williams, John Teasdale, Zindel Seagal & Jon Kabat-Zinn"
        flat.extend(s.strip() for s in p.split(",") if s.strip())
    return [flip_name(n) for n in flat if n]


# Compound surnames (Spanish/Portuguese/Italian double-barreled, particled
# names). Last token alone is not the surname for these.
COMPOUND_SURNAMES = {
    "Bioy Casares", "García Márquez", "Vargas Llosa", "Lloyd Wright",
    "Conan Doyle", "Lévi-Strauss", "Stephens-Davidowitz",
}
NAME_PARTICLES = {"de", "del", "della", "di", "du", "la", "las", "le", "los",
                  "van", "von", "der", "den", "ten", "ter", "af", "av", "zu",
                  "zur", "y"}


def flip_name(name: str) -> str:
    """``First Middle Last`` → ``Last, First Middle``."""
    name = name.strip()
    if not name or "," in name:
        return name
    parts = name.split()
    if len(parts) == 1:
        return parts[0]
    # Compound surname allowlist
    for compound in COMPOUND_SURNAMES:
        toks = compound.split()
        n = len(toks)
        if len(parts) >= n + 1 and parts[-n:] == toks:
            given = " ".join(parts[:-n])
            return f"{compound}, {given}"
    # Particles like "von", "de", "van" stay attached to the surname when
    # they immediately precede the last token.
    if len(parts) >= 3 and parts[-2].lower() in NAME_PARTICLES:
        return f"{parts[-2]} {parts[-1]}, {' '.join(parts[:-2])}"
    return f"{parts[-1]}, {' '.join(parts[:-1])}"


def cite_key_surname(cite_key: str) -> str:
    """Extract the surname-shaped prefix of a cite key.

    ``Alexander2014ControlGroupOut`` → ``Alexander``
    ``BioyCasares2002AdPorcos``      → ``BioyCasares``
    ``Stephens-Davidowitz2017...``   → ``Stephens-Davidowitz``
    """
    m = re.match(r"^([A-Za-z][\w-]*?)(?=\d|$)", cite_key)
    return m.group(1) if m else ""


def cite_key_title_words(cite_key: str) -> list[str]:
    """Extract the CamelCase title fragment after the year, lowercased."""
    m = re.search(r"\d{4}([A-Z][\w-]*)$", cite_key)
    if not m:
        return []
    tail = m.group(1)
    # Split CamelCase: AdPorcos -> Ad, Porcos
    words = re.findall(r"[A-Z][a-z]+|[A-Z]+(?=[A-Z]|$)", tail)
    return [w.lower() for w in words]


def get_field(entry: str, field: str) -> str:
    """Pull a single BibTeX field value from a raw entry string."""
    m = re.search(rf"^\s*{field}\s*=\s*\{{(.*?)\}},?\s*$", entry,
                  re.MULTILINE | re.DOTALL)
    if not m:
        return ""
    return re.sub(r"\s+", " ", m.group(1)).strip()


# Title noise we strip before fuzzy matching
_NORMALIZE_RE = re.compile(r"[^a-z0-9]+")


def normalize_word(s: str) -> str:
    return _NORMALIZE_RE.sub("", s.lower())


def migration_entry_is_plausible(entry: str, cite_key: str,
                                 wp_posts: list[dict]) -> tuple[bool, str]:
    """Decide whether the existing migration entry matches the cite key.

    Returns (is_plausible, reason). Plausible entries are preserved as-is,
    keeping any later enrichment (URLs, ISBNs, file paths). Implausible ones
    are replaced with the WP-derived rebuild.
    """
    surname_key = cite_key_surname(cite_key)
    if not surname_key:
        return True, "no surname in key"

    author = get_field(entry, "author") + " " + get_field(entry, "editor")
    title = (get_field(entry, "title") + " "
             + get_field(entry, "shorttitle") + " "
             + get_field(entry, "booktitle") + " "
             + get_field(entry, "journaltitle"))

    # Check 1: cite key surname must appear in author/editor (handles
    # "Bioy Casares, Adolfo" matching key prefix "BioyCasares").
    surname_norm = normalize_word(surname_key)
    author_norm = normalize_word(author)
    surname_present = surname_norm and surname_norm in author_norm
    if not surname_present:
        # Cross-check WP author too — if WP also disagrees with the
        # migration author, that confirms migration is wrong.
        wp_authors = {normalize_word(p.get("author", "")) for p in wp_posts}
        if wp_authors and not any(a and a in author_norm for a in wp_authors):
            return False, f"author mismatch: key={surname_key!r} entry author={author!r}"

    # Check 2: title overlap. Cite key's title words should appear in the
    # entry title or in the WP work_title.
    key_words = set(cite_key_title_words(cite_key))
    if key_words:
        title_words = {normalize_word(w) for w in re.findall(r"\w+", title)}
        title_words.discard("")
        overlap = {w for w in key_words if w in title_words}
        if not overlap:
            # If the WP work_title shares words with the cite key but the
            # migration title doesn't, migration is suspicious.
            wp_title_words = set()
            for p in wp_posts:
                for src in (p.get("work_title", ""), p.get("article_title", "")):
                    wp_title_words.update(normalize_word(w)
                                          for w in re.findall(r"\w+", src))
            wp_overlap = {w for w in key_words if w in wp_title_words}
            if wp_overlap:
                return False, (f"title mismatch: key words={sorted(key_words)} "
                               f"absent from entry title={title!r}")

    # Check 3: ALL CAPS title indicates a raw migration record that wasn't
    # later enriched.
    raw_title = get_field(entry, "title")
    if raw_title and raw_title.isupper():
        return False, "all-caps title (unenriched migration)"

    # Check 4: "Place of publication not identified" is a placeholder.
    location = get_field(entry, "location")
    if "not identified" in location.lower():
        return False, "placeholder location"

    return True, "plausible"


def extract_authors_from_attribution(attribution: str, fallback: str) -> str:
    """Return the BibTeX-formatted author list.

    For single-author posts the WP ``author`` field is reliable. For multi-
    author works it usually carries only the primary author; pull the full
    list from the head of ``attribution_text`` instead. For "quoted in"
    attributions, the work's author appears after the phrase, not before.
    """
    if not attribution:
        return " and ".join(split_authors(fallback))
    # "X, quoted in Y, Title, ..." → use Y as author, not X (the speaker).
    qm = re.search(r"\bquoted in\s+([^,]+(?:\s+&\s+[^,]+)*)", attribution,
                   re.IGNORECASE)
    if qm:
        candidate = qm.group(1).strip()
        return " and ".join(split_authors(candidate))
    m = re.match(r"^(.+?)\s*[,]\s*(?:'|\")", attribution)
    candidate = m.group(1) if m else None
    if not candidate:
        head = attribution.split(",", 1)[0]
        candidate = head
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


def is_journalish(work_title: str, attribution: str) -> bool:
    """Detect periodical / journal containers, not single-volume collections."""
    if re.search(r"\bvol\.\s*\d+", attribution):
        return True
    if re.search(r"\bno\.\s*\w+", attribution):
        return True
    if not work_title:
        return False
    # Multi-volume reference works (dictionaries, encyclopedias, companions)
    # are *not* journals even though their titles contain "Philosophy" etc.
    book_markers = ("Dictionary", "Encyclopedia", "Encyclopaedia",
                    "Companion", "Handbook", "Guide", "Reader",
                    "Anthology", "Collected Works", "Selected Works")
    if any(m in work_title for m in book_markers):
        return False
    journal_words = ("Review", "Journal", "Quarterly", "Magazine",
                     "Times", "Inquiry", "Mind", "Analysis", "Studies")
    return any(w in work_title.split() for w in journal_words)


def is_blog_or_online(work_title: str, attribution: str) -> bool:
    if not work_title:
        return True  # bare 'Title', date pattern → online
    blog_names = ("Slate Star Codex", "LessWrong", "Less Wrong",
                  "Astral Codex Ten", "Overcoming Bias", "Marginal Revolution",
                  "Paul Graham", "Eliezer Yudkowsky")
    return work_title in blog_names


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


def build_entry(cite_key: str, posts: list[dict]) -> tuple[str | None, str]:
    """Return (rendered BibTeX entry, status note)."""
    if not posts:
        return None, "no WP posts"

    authors = list({p.get("author", "") for p in posts if p.get("author")})
    work_titles = list({p.get("work_title", "") for p in posts if p.get("work_title")})
    years = list({p.get("year", "") for p in posts if p.get("year")})
    article_titles = list({p.get("article_title", "") for p in posts if p.get("article_title")})

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
    if year:
        fields["date"] = year

    # Decide entry type
    if article_title and is_journalish(work_title, attribution):
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
    elif article_title and is_blog_or_online(work_title, attribution):
        entry_type = "online"
        fields["title"] = article_title
        if work_title:
            fields["journaltitle"] = work_title
    elif article_title and work_title:
        entry_type = "incollection"
        fields["title"] = article_title
        fields["booktitle"] = work_title
        loc = extract_location(attribution, year)
        if loc:
            fields["location"] = loc
    elif article_title and not work_title:
        entry_type = "online"
        fields["title"] = article_title
    elif work_title and not article_title:
        entry_type = "book"
        fields["title"] = work_title
        loc = extract_location(attribution, year)
        if loc:
            fields["location"] = loc
    else:
        return None, "no title information"

    return render_entry(entry_type, cite_key, fields), "ok"


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
        if existing:
            ok, why = migration_entry_is_plausible(existing, key, posts)
            if ok:
                statuses["kept (plausible existing)"] += 1
                plausible_kept.append((key, why))
                continue
        entry, note = build_entry(key, posts)
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
