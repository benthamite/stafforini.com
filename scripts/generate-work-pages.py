#!/usr/bin/env python3
"""Generate Hugo work pages and post-process exported quote markdown files.

Phase 5 of the quote migration pipeline:
1. Strip citation placeholders from exported quote markdown
2. Generate work pages for all cite keys referenced by quotes

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

# === Constants ===

SCRIPTS_DIR = Path(__file__).parent
HUGO_ROOT = SCRIPTS_DIR.parent
QUOTES_DIR = HUGO_ROOT / "content" / "quotes"
WORKS_DIR = HUGO_ROOT / "content" / "works"

BIB_FILES = [
    Path.home() / "Library/CloudStorage/Dropbox/bibliography/new.bib",
    Path.home() / "Library/CloudStorage/Dropbox/bibliography/old.bib",
    Path.home() / "Library/CloudStorage/Dropbox/repos/babel-refs/bib/fluid.bib",
    Path.home() / "Library/CloudStorage/Dropbox/repos/babel-refs/bib/stable.bib",
    Path.home() / "Library/CloudStorage/Dropbox/repos/babel-refs/bib/db.bib",
    Path.home() / "Library/CloudStorage/Dropbox/bibliography/migration.bib",
]


# === Bib file parsing (reused from write-quotes-to-org.py) ===


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
            "year": year,
        })

    return entries


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
    """Strip the citation line that appears after the blockquote in ox-hugo output.

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

    # Keep everything up to and including the last blockquote line,
    # plus a trailing newline
    result_lines = lines[:last_blockquote_idx + 1]

    # Ensure the file ends with a single newline
    return "\n".join(result_lines) + "\n"


def postprocess_quotes(dry_run: bool = False) -> dict:
    """Strip citation placeholders from all exported quote markdown files."""
    stats = {"processed": 0, "modified": 0, "skipped_no_blockquote": 0, "already_clean": 0}

    if not QUOTES_DIR.exists():
        print(f"  WARNING: {QUOTES_DIR} does not exist")
        return stats

    md_files = sorted(QUOTES_DIR.glob("*.md"))
    for md_file in md_files:
        if md_file.name == "_index.md":
            continue

        content = md_file.read_text()
        stats["processed"] += 1

        # Split front matter from content
        # Handle both TOML (+++) and YAML (---) front matter
        if content.startswith("+++"):
            parts = content.split("+++", 2)
            if len(parts) >= 3:
                front_matter = "+++" + parts[1] + "+++"
                body = parts[2]
            else:
                continue
        elif content.startswith("---"):
            parts = content.split("---", 2)
            if len(parts) >= 3:
                front_matter = "---" + parts[1] + "---"
                body = parts[2]
            else:
                continue
        else:
            continue

        # Check if body has a blockquote
        if ">" not in body:
            stats["skipped_no_blockquote"] += 1
            continue

        cleaned_body = strip_citation_from_content(body.lstrip("\n"))
        new_content = front_matter + "\n" + cleaned_body

        if new_content == content:
            stats["already_clean"] += 1
            continue

        stats["modified"] += 1
        if not dry_run:
            md_file.write_text(new_content)

    return stats


# === Work page generation ===


def cite_key_to_slug(cite_key: str) -> str:
    """Convert CamelCase cite key to kebab-case slug.

    Singer1972FamineAffluence -> singer-1972-famine-affluence
    """
    s = re.sub(r"([a-zA-Z])(\d)", r"\1-\2", cite_key)
    s = re.sub(r"(\d)([a-zA-Z])", r"\1-\2", s)
    s = re.sub(r"([a-z])([A-Z])", r"\1-\2", s)
    return s.lower()


def escape_yaml_string(s: str) -> str:
    """Escape a string for YAML double-quoted value."""
    return s.replace("\\", "\\\\").replace('"', '\\"')


def generate_work_page(entry: dict) -> str:
    """Generate YAML front matter for a work page."""
    title = entry["title"].replace("{", "").replace("}", "")
    author_raw = entry["author"] or entry["editor"]
    author = bib_author_to_display(author_raw)
    year = entry["year"]

    lines = [
        "---",
        f'title: "{escape_yaml_string(title)}"',
        f'author: "{escape_yaml_string(author)}"',
    ]
    if year:
        lines.append(f"year: {year}")
    lines.append("---")
    lines.append("")

    return "\n".join(lines)


def collect_work_slugs_from_quotes() -> set[str]:
    """Scan all quote markdown files and collect unique work slugs."""
    slugs = set()
    if not QUOTES_DIR.exists():
        return slugs

    for md_file in QUOTES_DIR.glob("*.md"):
        if md_file.name == "_index.md":
            continue
        content = md_file.read_text()
        # Match work = "slug" (TOML) or work: "slug" (YAML)
        m = re.search(r'work\s*[=:]\s*"([^"]+)"', content)
        if m:
            slugs.add(m.group(1))

    return slugs


def generate_work_pages(bib_by_key: dict, dry_run: bool = False, limit: int = 0) -> dict:
    """Generate work pages for all cite keys referenced by quotes."""
    stats = {"created": 0, "skipped_exists": 0, "missing_bib": 0}
    missing_bib_keys = []

    work_slugs = collect_work_slugs_from_quotes()
    print(f"  Found {len(work_slugs)} unique work slugs in quote files")

    if not WORKS_DIR.exists():
        WORKS_DIR.mkdir(parents=True, exist_ok=True)

    # Build slug -> cite_key mapping
    slug_to_key = {}
    for cite_key in bib_by_key:
        slug = cite_key_to_slug(cite_key)
        slug_to_key[slug] = cite_key

    processed = 0
    for slug in sorted(work_slugs):
        if limit and processed >= limit:
            break

        work_path = WORKS_DIR / f"{slug}.md"

        # Skip if work page already exists
        if work_path.exists():
            stats["skipped_exists"] += 1
            continue

        # Find bib entry for this slug
        cite_key = slug_to_key.get(slug)
        if not cite_key:
            stats["missing_bib"] += 1
            missing_bib_keys.append(slug)
            continue

        entry = bib_by_key[cite_key]
        page_content = generate_work_page(entry)

        if dry_run:
            if processed < 5:
                print(f"  [CREATE] {slug}.md")
        else:
            work_path.write_text(page_content)

        stats["created"] += 1
        processed += 1

        if processed % 200 == 0:
            print(f"  ... {processed} work pages created")

    if missing_bib_keys:
        print(f"\n  WARNING: {len(missing_bib_keys)} work slugs have no matching bib entry:")
        for slug in missing_bib_keys[:10]:
            print(f"    {slug}")
        if len(missing_bib_keys) > 10:
            print(f"    ... and {len(missing_bib_keys) - 10} more")

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
        print(f"  Files processed: {pp_stats['processed']}")
        print(f"  Files modified:  {pp_stats['modified']}")
        print(f"  Already clean:   {pp_stats['already_clean']}")
        print(f"  No blockquote:   {pp_stats['skipped_no_blockquote']}")
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
        for bib_path in BIB_FILES:
            if not bib_path.exists():
                print(f"  WARNING: {bib_path} not found, skipping")
                continue
            entries = parse_bib_entries(bib_path)
            for entry in entries:
                bib_by_key[entry["cite_key"]] = entry
            print(f"    {bib_path.name}: {len(entries)} entries")
        print(f"  Total unique keys: {len(bib_by_key)}")

        wp_stats = generate_work_pages(bib_by_key, dry_run=args.dry_run, limit=args.limit)
        print(f"\n  Work pages created:    {wp_stats['created']}")
        print(f"  Skipped (exists):      {wp_stats['skipped_exists']}")
        print(f"  Missing bib entries:   {wp_stats['missing_bib']}")
        if args.dry_run:
            print("  *** DRY RUN — no files created ***")

    print("\nDone.")


if __name__ == "__main__":
    main()
