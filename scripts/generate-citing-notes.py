#!/usr/bin/env python3
"""Generate citing-notes.json from cite shortcodes in content/notes/.

Scans all Markdown files in content/notes/ for {{< cite "Key" >}} shortcodes,
converts CamelCase cite keys to kebab-case slugs (matching the cite shortcode
logic), and writes data/citing-notes.json mapping each work slug to the list
of notes that cite it.

This replaces the expensive O(N*M) content scan that was previously done
inside the works/single.html Hugo template.
"""

import json
import os
import re
import sys
import tempfile

from lib import cite_key_to_slug

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
NOTES_DIR = os.path.join(REPO_ROOT, "content", "notes")
OUTPUT_PATH = os.path.join(REPO_ROOT, "data", "citing-notes.json")

# Matches {{< cite "SomeKey" >}} or {{< cite "SomeKey" "pp. 1-2" >}}
CITE_RE = re.compile(r'\{\{<\s*cite\s+"([^"]+)"')


def main():
    if not os.path.isdir(NOTES_DIR):
        print("Warning: content/notes/ not found — skipping citing-notes generation.", file=sys.stderr)
        # Write empty file so the template doesn't break
        os.makedirs(os.path.dirname(OUTPUT_PATH), exist_ok=True)
        with open(OUTPUT_PATH, "w") as f:
            json.dump({}, f)
        return

    # work_slug -> set of (note_slug, note_title)
    citing = {}

    for filename in os.listdir(NOTES_DIR):
        if not filename.endswith(".md") or filename.startswith("_"):
            continue

        note_slug = filename[:-3]  # strip .md
        filepath = os.path.join(NOTES_DIR, filename)

        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()

        # Extract title from TOML or YAML front matter
        note_title = note_slug  # fallback
        title_match = re.search(r'^title\s*[=:]\s*"(.+)"', content, re.MULTILINE)
        if title_match:
            note_title = title_match.group(1)

        # Find all cite shortcode invocations
        for match in CITE_RE.finditer(content):
            cite_key = match.group(1)
            work_slug = cite_key_to_slug(cite_key)

            if work_slug not in citing:
                citing[work_slug] = set()
            citing[work_slug].add((note_slug, note_title))

    # Convert sets to sorted lists of dicts
    result = {}
    for work_slug, notes in sorted(citing.items()):
        result[work_slug] = sorted(
            [{"slug": s, "title": t} for s, t in notes],
            key=lambda x: x["title"].lower(),
        )

    from lib import atomic_write_json
    os.makedirs(os.path.dirname(OUTPUT_PATH), exist_ok=True)
    atomic_write_json(OUTPUT_PATH, result, ensure_ascii=False)

    total_citations = sum(len(v) for v in result.values())
    print(f"Generated citing-notes for {len(result)} works ({total_citations} total citations)")


if __name__ == "__main__":
    main()
