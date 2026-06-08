#!/usr/bin/env python3
"""Generate citing-notes.json from cite shortcodes in content/notes/.

Scans all Markdown files in content/notes/ for {{< cite "Key" >}} shortcodes,
converts CamelCase cite keys to kebab-case slugs (matching the cite shortcode
logic), and writes data/citing-notes.json mapping each work slug to the list
of notes that cite it.

This replaces the expensive O(N*M) content scan that was previously done
inside the works/single.html Hugo template.
"""

import re
import sys

from lib import (
    REPO_ROOT,
    atomic_write_json,
    cite_key_to_slug,
    slug_title_sets_to_sorted_json,
    toml_front_matter,
)

NOTES_DIR = REPO_ROOT / "content" / "notes"
OUTPUT_PATH = REPO_ROOT / "data" / "citing-notes.json"

# Matches {{< cite "SomeKey" >}} or {{< cite "SomeKey" "pp. 1-2" >}}
CITE_RE = re.compile(r'\{\{<\s*cite\s+"([^"]+)"')


def collect_citing_notes(notes_dir):
    """Return work_slug -> set of (note_slug, note_title) for listed notes."""
    citing = {}

    for filepath in notes_dir.iterdir():
        if filepath.suffix != ".md" or filepath.name.startswith("_"):
            continue

        note_slug = filepath.stem
        content = filepath.read_text(encoding="utf-8")
        metadata = toml_front_matter(content)
        if metadata.get("unlisted") is True:
            continue

        note_title = metadata.get("title", note_slug)

        # Find all cite shortcode invocations
        for match in CITE_RE.finditer(content):
            cite_key = match.group(1)
            work_slug = cite_key_to_slug(cite_key)

            if work_slug not in citing:
                citing[work_slug] = set()
            citing[work_slug].add((note_slug, note_title))

    return citing


def main():
    if not NOTES_DIR.is_dir():
        print("Warning: content/notes/ not found — skipping citing-notes generation.", file=sys.stderr)
        # Write empty file so the template doesn't break
        OUTPUT_PATH.parent.mkdir(parents=True, exist_ok=True)
        atomic_write_json(OUTPUT_PATH, {})
        return

    citing = collect_citing_notes(NOTES_DIR)

    # Convert sets to sorted lists of dicts
    result = slug_title_sets_to_sorted_json(citing)

    OUTPUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    atomic_write_json(OUTPUT_PATH, result, ensure_ascii=False)

    total_citations = sum(len(v) for v in result.values())
    print(f"Generated citing-notes for {len(result)} works ({total_citations} total citations)")


if __name__ == "__main__":
    main()
