#!/usr/bin/env python3
"""Match parsed WP quotes to biblatex entries using citar-style orderless matching.

Like citar-insert-citation, builds a searchable string per bib entry
(author + year + title + cite_key) and checks if search terms from
the WP quote all appear in it.
"""

import json
import re
import unicodedata
from pathlib import Path

SCRIPTS_DIR = Path(__file__).parent
QUOTES_JSON = SCRIPTS_DIR / "wp-quotes-parsed.json"
OUTPUT_JSON = SCRIPTS_DIR / "wp-quotes-matched.json"
REPORT_FILE = SCRIPTS_DIR / "match-report.txt"

BIB_FILES = [
    Path.home() / "Library/CloudStorage/Dropbox/bibliography/new.bib",
    Path.home() / "Library/CloudStorage/Dropbox/bibliography/old.bib",
    Path.home() / "Library/CloudStorage/Dropbox/repos/babel-refs/bib/fluid.bib",
    Path.home() / "Library/CloudStorage/Dropbox/repos/babel-refs/bib/stable.bib",
    Path.home() / "Library/CloudStorage/Dropbox/repos/babel-refs/bib/db.bib",
]


def normalize(text: str) -> str:
    """Lowercase, strip accents, remove punctuation, collapse whitespace."""
    text = unicodedata.normalize("NFD", text)
    text = "".join(c for c in text if unicodedata.category(c) != "Mn")
    text = text.lower()
    text = re.sub(r"[^\w\s-]", " ", text)
    text = re.sub(r"\s+", " ", text).strip()
    return text


def extract_surname(author_str: str) -> str:
    """Extract the primary author's surname."""
    if not author_str:
        return ""
    author_str = re.split(r"\band\b", author_str, flags=re.IGNORECASE)[0].strip()
    author_str = author_str.replace("{", "").replace("}", "")
    if "," in author_str:
        return author_str.split(",")[0].strip()
    parts = author_str.strip().split()
    return parts[-1] if len(parts) >= 2 else author_str.strip()


STOP_WORDS = {
    "a", "an", "the", "of", "in", "on", "and", "or", "for", "to", "with",
    "from", "by", "at", "its", "is", "are", "was", "were", "be", "been",
    "being", "have", "has", "had", "do", "does", "did", "not", "but",
    "that", "this", "these", "those", "as", "if", "than", "so",
    "el", "la", "los", "las", "de", "del", "en", "un", "una", "y", "o",
    "le", "les", "des", "du", "et", "der", "die", "das", "und", "von",
}


def parse_bib_entries(bib_path: Path) -> list[dict]:
    """Parse a .bib file into a list of entry dicts."""
    entries = []
    text = bib_path.read_text(errors="replace")
    entry_starts = list(re.finditer(r"@(\w+)\s*\{([^,\s]+)\s*,", text))

    for idx, match in enumerate(entry_starts):
        entry_type = match.group(1).lower()
        cite_key = match.group(2).strip()
        if entry_type in ("comment", "preamble", "string"):
            continue

        start_pos = match.end()
        end_pos = entry_starts[idx + 1].start() if idx + 1 < len(entry_starts) else len(text)
        body = text[start_pos:end_pos]

        fields = {}
        for field_match in re.finditer(
            r"(\w+)\s*=\s*(?:\{((?:[^{}]|\{[^{}]*\})*)\}|\"([^\"]*)\"|(\d+))",
            body,
        ):
            field_name = field_match.group(1).lower()
            field_value = (
                field_match.group(2) or field_match.group(3) or field_match.group(4) or ""
            )
            field_value = field_value.replace("{", "").replace("}", "")
            fields[field_name] = field_value.strip()

        year = fields.get("year", "")
        if not year and "date" in fields:
            ym = re.search(r"(\d{4})", fields["date"])
            if ym:
                year = ym.group(1)

        entries.append({
            "cite_key": cite_key,
            "entry_type": entry_type,
            "author": fields.get("author", ""),
            "editor": fields.get("editor", ""),
            "title": fields.get("title", ""),
            "shorttitle": fields.get("shorttitle", ""),
            "booktitle": fields.get("booktitle", ""),
            "origtitle": fields.get("origtitle", ""),
            "journaltitle": fields.get("journaltitle", fields.get("journal", "")),
            "year": year,
            "crossref": fields.get("crossref", ""),
            "bib_file": str(bib_path),
        })

    return entries


def build_searchable_string(entry: dict) -> str:
    """Build a single searchable string for a bib entry, like citar does.

    Concatenates: author, editor, year, title, shorttitle, booktitle,
    origtitle, cite_key (with CamelCase split), entry_type.
    """
    parts = []

    for field in ("author", "editor", "title", "shorttitle", "booktitle", "origtitle", "journaltitle"):
        if entry[field]:
            parts.append(entry[field])

    if entry["year"]:
        parts.append(entry["year"])

    # Split CamelCase cite key into words
    key_expanded = re.sub(r"([A-Z])", r" \1", entry["cite_key"])
    parts.append(key_expanded)

    parts.append(entry["entry_type"])

    return normalize(" ".join(parts))


def build_index(entries: list[dict]) -> dict:
    """Build lookup indices for fast matching."""
    by_key = {}
    by_surname = {}
    searchable = {}  # cite_key -> normalized searchable string

    for entry in entries:
        by_key[entry["cite_key"]] = entry
        searchable[entry["cite_key"]] = build_searchable_string(entry)

        author = entry["author"] or entry["editor"]
        surname = normalize(extract_surname(author))
        if surname:
            by_surname.setdefault(surname, []).append(entry)
            # Also index each word of multi-word surnames separately
            # so "Bioy Casares" is findable via "casares" alone
            surname_words = surname.split()
            if len(surname_words) > 1:
                for word in surname_words:
                    if word not in STOP_WORDS and len(word) > 1:
                        by_surname.setdefault(word, []).append(entry)

    return {
        "by_key": by_key,
        "by_surname": by_surname,
        "searchable": searchable,
    }


def extract_search_terms(quote: dict) -> list[str]:
    """Extract search terms from a WP quote, like what you'd type in citar.

    Returns a list of normalized terms. We pick distinctive words from:
    - Author surname
    - Year
    - Article/chapter title (if present, the text in quotes)
    - Work title (the italic text)
    """
    terms = []

    # Author surname
    surname = normalize(extract_surname(quote["author"]))
    if surname:
        terms.append(surname)

    # Year
    if quote["year"]:
        terms.append(quote["year"])

    # Get distinctive content words from titles
    article_title = quote.get("article_title", "")
    work_title = quote.get("work_title", "")

    # Collect content words from both titles for broader matching
    all_content_words = set()
    for title_source in (article_title, work_title):
        if title_source:
            words = normalize(title_source).split()
            for w in words:
                if w not in STOP_WORDS and len(w) > 2:
                    all_content_words.add(w)

    # Take up to 3 most distinctive words (longest words tend to be most distinctive)
    sorted_words = sorted(all_content_words, key=len, reverse=True)
    terms.extend(sorted_words[:3])

    return terms


def orderless_match(terms: list[str], searchable: str) -> bool:
    """Check if ALL terms appear somewhere in the searchable string."""
    return all(term in searchable for term in terms)


def score_match(terms: list[str], searchable: str) -> float:
    """Score how many terms match (0 to 1).

    Includes year tolerance: if a 4-digit year term doesn't match exactly,
    check ±2 years as a partial credit (0.5 for nearby year).
    """
    if not terms:
        return 0.0
    total = 0.0
    for t in terms:
        if t in searchable:
            total += 1.0
        elif re.fullmatch(r"\d{4}", t):
            # Year tolerance: check ±2 years
            year = int(t)
            if any(str(y) in searchable for y in range(year - 2, year + 3) if y != year):
                total += 0.5
    return total / len(terms)


def match_quote(quote: dict, index: dict) -> dict:
    """Match a quote using citar-style orderless matching."""
    terms = extract_search_terms(quote)
    if not terms:
        return {"status": "no_author", "matches": []}

    surname = normalize(extract_surname(quote["author"]))

    # Get candidate entries: all entries by this author surname
    candidate_entries = index["by_surname"].get(surname, [])

    if not candidate_entries:
        return {"status": "no_candidates", "matches": []}

    # Score each candidate using orderless matching
    scored = []
    for entry in candidate_entries:
        s = index["searchable"][entry["cite_key"]]
        score = score_match(terms, s)
        if score > 0:
            scored.append((score, entry))

    # Also check crossref parent entries
    for entry in candidate_entries:
        if entry["crossref"]:
            parent = index["by_key"].get(entry["crossref"])
            if parent:
                s = index["searchable"][parent["cite_key"]]
                score = score_match(terms, s)
                if score > 0:
                    scored.append((score, parent))

    if not scored:
        return {"status": "no_match", "matches": []}

    # Deduplicate by cite_key, keeping best score
    best_by_key = {}
    for score, entry in scored:
        k = entry["cite_key"]
        if k not in best_by_key or score > best_by_key[k][0]:
            best_by_key[k] = (score, entry)
    scored = sorted(best_by_key.values(), key=lambda x: -x[0])

    best_score = scored[0][0]

    def fmt(candidates, min_score=0.0):
        return [
            {"cite_key": e["cite_key"], "score": round(s, 3),
             "bib_title": e["title"], "bib_author": e["author"]}
            for s, e in candidates if s >= min_score
        ][:5]

    # A full match means all terms found
    if best_score >= 1.0:
        # Check for ties
        ties = [x for x in scored if x[0] >= 1.0]
        if len(ties) > 1:
            return {"status": "ambiguous", "matches": fmt(ties)}
        return {"status": "matched", "matches": fmt(scored[:1])}
    elif best_score >= 0.75:
        ties = [x for x in scored if x[0] >= best_score * 0.95]
        if len(ties) > 1:
            return {"status": "ambiguous", "matches": fmt(ties)}
        return {"status": "matched", "matches": fmt(scored[:1])}
    elif best_score >= 0.5:
        return {"status": "weak", "matches": fmt(scored[:3], 0.4)}
    else:
        return {"status": "no_match", "matches": fmt(scored[:3])}


def main():
    print("Loading parsed quotes...")
    quotes = json.loads(QUOTES_JSON.read_text())
    print(f"  {len(quotes)} quotes loaded")

    print("\nParsing bib files...")
    all_entries = []
    for bib_path in BIB_FILES:
        if not bib_path.exists():
            print(f"  WARNING: {bib_path} not found, skipping")
            continue
        entries = parse_bib_entries(bib_path)
        print(f"  {bib_path.name}: {len(entries)} entries")
        all_entries.extend(entries)
    print(f"  Total: {len(all_entries)} bib entries")

    print("\nBuilding index...")
    index = build_index(all_entries)
    print(f"  {len(index['by_key'])} unique cite keys")
    print(f"  {len(index['by_surname'])} unique author surnames")

    print("\nMatching quotes to bib entries...")
    stats = {"matched": 0, "ambiguous": 0, "weak": 0, "no_match": 0, "no_candidates": 0, "no_author": 0}
    report_lines = []

    for quote in quotes:
        result = match_quote(quote, index)
        quote["bib_match"] = result
        stats[result["status"]] += 1

        if result["status"] in ("no_match", "no_candidates", "weak"):
            terms = extract_search_terms(quote)
            report_lines.append(
                f"[{result['status'].upper()}] {quote['author']} — "
                f"{quote.get('article_title') or quote['work_title'] or '(no title)'} ({quote['year'] or 'no year'})\n"
                f"  Search terms: {terms}\n"
                f"  Attribution: {quote['attribution_text'][:150]}\n"
                + (f"  Best candidate: {result['matches'][0]['cite_key']} "
                   f"(score: {result['matches'][0]['score']}, "
                   f"title: {result['matches'][0]['bib_title'][:80]})\n"
                   if result["matches"] else "  No candidates found\n")
            )
        elif result["status"] == "ambiguous":
            report_lines.append(
                f"[AMBIGUOUS] {quote['author']} — "
                f"{quote.get('article_title') or quote['work_title'] or '(no title)'} ({quote['year'] or 'no year'})\n"
                + "".join(
                    f"  Candidate: {m['cite_key']} (score: {m['score']}, title: {m['bib_title'][:80]})\n"
                    for m in result["matches"]
                )
            )

    print(f"\n{'='*60}")
    print("MATCHING RESULTS")
    print(f"{'='*60}")
    print(f"  Matched:        {stats['matched']:>5}  (confident single match)")
    print(f"  Ambiguous:      {stats['ambiguous']:>5}  (multiple close matches)")
    print(f"  Weak:           {stats['weak']:>5}  (low-confidence match)")
    print(f"  No match:       {stats['no_match']:>5}  (surname found, title mismatch)")
    print(f"  No candidates:  {stats['no_candidates']:>5}  (surname not in bib)")
    print(f"  No author:      {stats['no_author']:>5}  (no author data)")
    print(f"  {'─'*40}")
    print(f"  Total:          {len(quotes):>5}")

    need_attention = stats['ambiguous'] + stats['weak'] + stats['no_match'] + stats['no_candidates'] + stats['no_author']
    print(f"\n  Need attention: {need_attention} quotes")
    print(f"  Ready to use:   {stats['matched']} quotes ({stats['matched']*100//len(quotes)}%)")

    OUTPUT_JSON.write_text(json.dumps(quotes, indent=2, ensure_ascii=False))
    print(f"\nAugmented JSON written to {OUTPUT_JSON}")

    REPORT_FILE.write_text("\n".join(report_lines))
    print(f"Report of {len(report_lines)} items needing attention written to {REPORT_FILE}")


if __name__ == "__main__":
    main()
