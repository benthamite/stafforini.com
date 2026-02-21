#!/usr/bin/env python3
"""Match WordPress quote tags to org-mode note and people files.

Reads tags from wp-quotes-tags.json and attempts to match each tag to an
org file in ~/Dropbox/notes/ or ~/Dropbox/people/ using a tiered cascade:

  1. Exact normalized match against #+title: values
  2. Exact normalized match against filename stems
  3. Hyphenation/possessive normalization
  4. Plural/singular inflection
  5. Prefix match for truncated tags
  6. Fuzzy match (auto-accept high-confidence typos only)

Produces:
  - wp-quotes-tags-linked.json  (structured results)
  - tags-match-report.txt       (human-readable summary)
"""

import json
import re
from datetime import datetime, timezone
from pathlib import Path

from rapidfuzz import fuzz
from rapidfuzz.distance import Levenshtein

from lib import normalize

SCRIPTS_DIR = Path(__file__).parent
TAGS_JSON = SCRIPTS_DIR / "wp-quotes-tags.json"
OUTPUT_JSON = SCRIPTS_DIR / "wp-quotes-tags-linked.json"
REPORT_FILE = SCRIPTS_DIR / "tags-match-report.txt"

NOTES_DIR = Path.home() / "Library/CloudStorage/Dropbox/notes"
PEOPLE_DIR = Path.home() / "Library/CloudStorage/Dropbox/people"


def read_org_title(path: Path) -> str:
    """Read the #+title: value from the first 10 lines of an org file."""
    try:
        with open(path, encoding="utf-8", errors="replace") as f:
            for i, line in enumerate(f):
                if i >= 10:
                    break
                m = re.match(r"^#\+title:\s*(.+)", line, re.IGNORECASE)
                if m:
                    return m.group(1).strip()
    except OSError:
        pass
    return ""


def build_org_indices(
    directories: dict[str, Path],
) -> tuple[dict[str, tuple[str, str]], dict[str, tuple[str, str]]]:
    """Build title and filename indices from org files.

    Args:
        directories: Mapping of directory label -> Path (e.g. {"notes": ..., "people": ...})

    Returns:
        (title_index, filename_index) where each maps
        normalize(name) -> (filename, directory_label)
    """
    title_index: dict[str, tuple[str, str]] = {}
    filename_index: dict[str, tuple[str, str]] = {}

    for label, dirpath in directories.items():
        if not dirpath.is_dir():
            print(f"  WARNING: {dirpath} not found, skipping")
            continue

        org_files = sorted(dirpath.glob("*.org"))
        print(f"  {label}: {len(org_files)} org files")

        for org_path in org_files:
            filename = org_path.name
            entry = (filename, label)

            # Title index
            title = read_org_title(org_path)
            if title:
                norm_title = normalize(title)
                if norm_title and norm_title not in title_index:
                    title_index[norm_title] = entry

            # Filename index (stem with hyphens replaced by spaces)
            stem = org_path.stem
            norm_stem = normalize(stem.replace("-", " "))
            if norm_stem and norm_stem not in filename_index:
                filename_index[norm_stem] = entry

    return title_index, filename_index


def normalize_variants(tag_norm: str) -> list[str]:
    """Generate hyphenation/possessive variants of a normalized tag.

    Returns a list of alternative normalized forms to try.
    """
    variants = set()

    # Remove lone "s" words left over from possessives
    # normalize() turns "Chesterton's fence" into "chesterton s fence"
    # (apostrophe removed, leaving a standalone "s" token)
    words = tag_norm.split()
    if "s" in words:
        filtered = [w for w in words if w != "s"]
        if filtered:
            variants.add(" ".join(filtered))

    # Replace hyphens with spaces and vice versa
    if "-" in tag_norm:
        variants.add(tag_norm.replace("-", " "))
        variants.add(tag_norm.replace("-", ""))
    if " " in tag_norm:
        variants.add(tag_norm.replace(" ", "-"))
        variants.add(tag_norm.replace(" ", ""))

    # Collapse compounds: try removing spaces entirely
    no_spaces = tag_norm.replace(" ", "")
    if no_spaces != tag_norm:
        variants.add(no_spaces)

    # Try splitting compounds back: "selfcontrol" -> "self control"
    if "self" in tag_norm and "self " not in tag_norm and "self-" not in tag_norm:
        variants.add(tag_norm.replace("self", "self "))

    # Remove trailing "s" from words (possessive remnants)
    # e.g. "darwins problem" -> "darwin problem"
    for i, w in enumerate(words):
        if w.endswith("s") and len(w) > 3:
            new_words = words[:i] + [w[:-1]] + words[i + 1 :]
            variants.add(" ".join(new_words))

    variants.discard(tag_norm)
    return list(variants)


def inflect_variants(tag_norm: str) -> list[str]:
    """Generate plural/singular inflection variants."""
    variants = set()

    # Simple plural: add/remove 's'
    if tag_norm.endswith("s"):
        variants.add(tag_norm[:-1])  # dogs -> dog
    else:
        variants.add(tag_norm + "s")  # dog -> dogs

    # -es endings
    if tag_norm.endswith("es"):
        variants.add(tag_norm[:-2])  # watches -> watch
    elif tag_norm.endswith(("ch", "sh", "s", "x", "z")):
        variants.add(tag_norm + "es")

    # -ies / -y
    if tag_norm.endswith("ies"):
        variants.add(tag_norm[:-3] + "y")  # theories -> theory
    elif tag_norm.endswith("y") and len(tag_norm) > 2 and tag_norm[-2] not in "aeiou":
        variants.add(tag_norm[:-1] + "ies")  # theory -> theories

    # -ism / -isms
    if tag_norm.endswith("isms"):
        variants.add(tag_norm[:-1])  # -isms -> -ism
    elif tag_norm.endswith("ism"):
        variants.add(tag_norm + "s")

    variants.discard(tag_norm)
    return list(variants)


def is_likely_person(tag: str) -> bool:
    """Heuristic: a tag is likely a person if it has 2+ capitalized words."""
    words = tag.split()
    if len(words) < 2:
        return False
    capitalized = sum(1 for w in words if w[0].isupper())
    return capitalized >= 2


def match_tags(tags: list[str], title_index: dict, filename_index: dict) -> dict:
    """Match tags using the tiered cascade.

    Returns dict with 'linked' and 'unlinked' lists and 'stats'.
    """
    linked = []
    unlinked = []
    stats = {
        "exact_title": 0,
        "exact_filename": 0,
        "hyphenation": 0,
        "inflection": 0,
        "prefix": 0,
        "fuzzy": 0,
    }
    matched_tags = set()

    # Tier 1: Exact normalized match against titles
    for tag in tags:
        tag_norm = normalize(tag)
        if not tag_norm:
            continue
        if tag_norm in title_index:
            filename, directory = title_index[tag_norm]
            linked.append({
                "tag": tag,
                "file": filename,
                "directory": directory,
                "tier": "exact_title",
            })
            stats["exact_title"] += 1
            matched_tags.add(tag)

    # Tier 2: Exact normalized match against filename stems
    for tag in tags:
        if tag in matched_tags:
            continue
        tag_norm = normalize(tag)
        if not tag_norm:
            continue
        if tag_norm in filename_index:
            filename, directory = filename_index[tag_norm]
            linked.append({
                "tag": tag,
                "file": filename,
                "directory": directory,
                "tier": "exact_filename",
            })
            stats["exact_filename"] += 1
            matched_tags.add(tag)

    # Tier 3: Hyphenation/possessive normalization
    for tag in tags:
        if tag in matched_tags:
            continue
        tag_norm = normalize(tag)
        if not tag_norm:
            continue
        variants = normalize_variants(tag_norm)
        for variant in variants:
            if variant in title_index:
                filename, directory = title_index[variant]
                linked.append({
                    "tag": tag,
                    "file": filename,
                    "directory": directory,
                    "tier": "hyphenation",
                })
                stats["hyphenation"] += 1
                matched_tags.add(tag)
                break
            if variant in filename_index:
                filename, directory = filename_index[variant]
                linked.append({
                    "tag": tag,
                    "file": filename,
                    "directory": directory,
                    "tier": "hyphenation",
                })
                stats["hyphenation"] += 1
                matched_tags.add(tag)
                break

    # Tier 4: Plural/singular inflection
    for tag in tags:
        if tag in matched_tags:
            continue
        tag_norm = normalize(tag)
        if not tag_norm:
            continue
        variants = inflect_variants(tag_norm)
        found = False
        for variant in variants:
            if variant in title_index:
                filename, directory = title_index[variant]
                linked.append({
                    "tag": tag,
                    "file": filename,
                    "directory": directory,
                    "tier": "inflection",
                })
                stats["inflection"] += 1
                matched_tags.add(tag)
                found = True
                break
            if variant in filename_index:
                filename, directory = filename_index[variant]
                linked.append({
                    "tag": tag,
                    "file": filename,
                    "directory": directory,
                    "tier": "inflection",
                })
                stats["inflection"] += 1
                matched_tags.add(tag)
                found = True
                break
        if found:
            continue

        # Also try inflecting each word in multi-word tags
        words = tag_norm.split()
        if len(words) > 1:
            for i, word in enumerate(words):
                for word_variant in inflect_variants(word):
                    full_variant = " ".join(words[:i] + [word_variant] + words[i + 1 :])
                    if full_variant in title_index:
                        filename, directory = title_index[full_variant]
                        linked.append({
                            "tag": tag,
                            "file": filename,
                            "directory": directory,
                            "tier": "inflection",
                        })
                        stats["inflection"] += 1
                        matched_tags.add(tag)
                        found = True
                        break
                    if full_variant in filename_index:
                        filename, directory = filename_index[full_variant]
                        linked.append({
                            "tag": tag,
                            "file": filename,
                            "directory": directory,
                            "tier": "inflection",
                        })
                        stats["inflection"] += 1
                        matched_tags.add(tag)
                        found = True
                        break
                if found:
                    break

    # Tier 5: Prefix match for truncated tags
    # Only catches genuine mid-word truncations like "pleasu"→"pleasure".
    # Constraints: suffix ≤ 2 chars, no spaces in suffix, and the last
    # word of the tag must be ≥ 4 chars (avoids "World War I"→"World War II").
    all_titles = list(title_index.keys())
    for tag in tags:
        if tag in matched_tags:
            continue
        tag_norm = normalize(tag)
        if not tag_norm or len(tag_norm) < 5:
            continue
        # The last word of the tag must be ≥ 4 chars (genuine truncation)
        last_word = tag_norm.rsplit(" ", 1)[-1]
        if len(last_word) < 4:
            continue
        prefix_matches = [
            (title, title_index[title])
            for title in all_titles
            if title.startswith(tag_norm)
            and 0 < len(title) - len(tag_norm) <= 2
            and " " not in title[len(tag_norm):]
        ]
        if len(prefix_matches) == 1:
            title, (filename, directory) = prefix_matches[0]
            linked.append({
                "tag": tag,
                "file": filename,
                "directory": directory,
                "tier": "prefix",
                "matched_title": title,
            })
            stats["prefix"] += 1
            matched_tags.add(tag)

    # Tier 6: Fuzzy match (auto-accept at high confidence)
    # Catches genuine typos like "conterfactuals" → "counterfactuals".
    # Very strict thresholds (score >= 96, Levenshtein distance == 1) to
    # avoid false positives from similar-but-distinct concepts (e.g.
    # "immorality"/"immortality", "modal realism"/"moral realism").
    # Additional filters: no substring containment, same word count.
    for tag in tags:
        if tag in matched_tags:
            continue
        tag_norm = normalize(tag)
        if not tag_norm or len(tag_norm) < 6:
            continue
        best_score = 0
        best_match = None
        for title, entry in title_index.items():
            if abs(len(title) - len(tag_norm)) > 2:
                continue
            if len(tag_norm.split()) != len(title.split()):
                continue
            if tag_norm in title or title in tag_norm:
                continue
            score = fuzz.ratio(tag_norm, title)
            if score > best_score:
                best_score = score
                best_match = (title, entry)

        if best_match and best_score >= 96:
            title, (filename, directory) = best_match
            dist = Levenshtein.distance(tag_norm, title)
            if dist == 1:
                linked.append({
                    "tag": tag,
                    "file": filename,
                    "directory": directory,
                    "tier": "fuzzy",
                    "score": round(best_score, 1),
                    "distance": dist,
                })
                stats["fuzzy"] += 1
                matched_tags.add(tag)

    # Collect unlinked tags
    for tag in tags:
        if tag in matched_tags:
            continue
        tag_type = "person" if is_likely_person(tag) else "topic"
        unlinked.append({"tag": tag, "tag_type": tag_type})

    return {
        "linked": linked,
        "unlinked": unlinked,
        "stats": stats,
    }


def write_report(results: dict, total_tags: int) -> str:
    """Generate a human-readable report."""
    lines = []
    stats = results["stats"]

    lines.append("=" * 60)
    lines.append("TAG MATCHING REPORT")
    lines.append("=" * 60)
    lines.append(f"Generated: {datetime.now(timezone.utc).isoformat()}")
    lines.append(f"Total tags: {total_tags}")
    lines.append("")

    # Stats by tier
    lines.append("MATCHES BY TIER")
    lines.append("-" * 40)
    total_linked = len(results["linked"])
    total_unlinked = len(results["unlinked"])

    lines.append(f"  Tier 1 - Exact title:     {stats['exact_title']:>5}")
    lines.append(f"  Tier 2 - Exact filename:  {stats['exact_filename']:>5}")
    lines.append(f"  Tier 3 - Hyphenation:     {stats['hyphenation']:>5}")
    lines.append(f"  Tier 4 - Inflection:      {stats['inflection']:>5}")
    lines.append(f"  Tier 5 - Prefix:          {stats['prefix']:>5}")
    lines.append(f"  Tier 6 - Fuzzy (typo):    {stats['fuzzy']:>5}")
    lines.append(f"  {'─' * 38}")
    lines.append(f"  Total linked:             {total_linked:>5}")
    lines.append(f"  Unlinked:                 {total_unlinked:>5}")
    lines.append("")

    pct = (total_linked * 100 // total_tags) if total_tags else 0
    lines.append(f"  Linked rate: {pct}% ({total_linked}/{total_tags})")
    lines.append("")

    # Fuzzy matches
    fuzzy_items = [i for i in results["linked"] if i["tier"] == "fuzzy"]
    if fuzzy_items:
        lines.append("=" * 60)
        lines.append("FUZZY MATCHES (typo corrections)")
        lines.append("=" * 60)
        for item in fuzzy_items:
            lines.append(
                f"  \"{item['tag']}\" -> {item['file']} "
                f"(score: {item['score']}, dist: {item['distance']}, "
                f"dir: {item['directory']})"
            )
        lines.append("")

    # Prefix matches
    prefix_items = [i for i in results["linked"] if i["tier"] == "prefix"]
    if prefix_items:
        lines.append("=" * 60)
        lines.append("PREFIX MATCHES (truncated tags)")
        lines.append("=" * 60)
        for item in prefix_items:
            lines.append(
                f"  \"{item['tag']}\" -> {item['file']} "
                f"(matched: \"{item.get('matched_title', '')}\")"
            )
        lines.append("")

    # Unlinked summary
    unlinked_people = [i for i in results["unlinked"] if i["tag_type"] == "person"]
    unlinked_topics = [i for i in results["unlinked"] if i["tag_type"] == "topic"]

    lines.append("=" * 60)
    lines.append("UNLINKED TAGS SUMMARY")
    lines.append("=" * 60)
    lines.append(f"  People (heuristic): {len(unlinked_people)}")
    lines.append(f"  Topics:             {len(unlinked_topics)}")
    lines.append("")

    if unlinked_people:
        lines.append("Unlinked people:")
        for item in sorted(unlinked_people, key=lambda x: x["tag"]):
            lines.append(f"  {item['tag']}")
        lines.append("")

    if unlinked_topics:
        lines.append("Unlinked topics:")
        for item in sorted(unlinked_topics, key=lambda x: x["tag"]):
            lines.append(f"  {item['tag']}")

    return "\n".join(lines)


def main():
    print("Loading tags...")
    tags = json.loads(TAGS_JSON.read_text())
    print(f"  {len(tags)} tags loaded")

    print("\nBuilding org file indices...")
    directories = {
        "notes": NOTES_DIR,
        "people": PEOPLE_DIR,
    }
    title_index, filename_index = build_org_indices(directories)
    print(f"  Title index: {len(title_index)} entries")
    print(f"  Filename index: {len(filename_index)} entries")

    print("\nMatching tags...")
    results = match_tags(tags, title_index, filename_index)

    total_linked = len(results["linked"])
    total_unlinked = len(results["unlinked"])
    stats = results["stats"]

    print(f"\n{'=' * 60}")
    print("MATCHING RESULTS")
    print(f"{'=' * 60}")
    print(f"  Tier 1 - Exact title:     {stats['exact_title']:>5}")
    print(f"  Tier 2 - Exact filename:  {stats['exact_filename']:>5}")
    print(f"  Tier 3 - Hyphenation:     {stats['hyphenation']:>5}")
    print(f"  Tier 4 - Inflection:      {stats['inflection']:>5}")
    print(f"  Tier 5 - Prefix:          {stats['prefix']:>5}")
    print(f"  Tier 6 - Fuzzy (typo):    {stats['fuzzy']:>5}")
    print(f"  {'─' * 38}")
    print(f"  Total linked:             {total_linked:>5}")
    print(f"  Unlinked:                 {total_unlinked:>5}")

    pct = (total_linked * 100 // len(tags)) if tags else 0
    print(f"\n  Linked rate: {pct}% ({total_linked}/{len(tags)})")

    # Write output JSON
    output = {
        "meta": {
            "generated": datetime.now(timezone.utc).isoformat(),
            "total_tags": len(tags),
            "linked": total_linked,
            "unlinked": total_unlinked,
        },
        "linked": results["linked"],
        "unlinked": results["unlinked"],
    }
    OUTPUT_JSON.write_text(json.dumps(output, indent=2, ensure_ascii=False))
    print(f"\nResults written to {OUTPUT_JSON}")

    # Write report
    report = write_report(results, len(tags))
    REPORT_FILE.write_text(report)
    print(f"Report written to {REPORT_FILE}")


if __name__ == "__main__":
    main()
