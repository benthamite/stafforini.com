#!/usr/bin/env python3
"""Generate minimal Hugo content pages for org-roam topic stubs.

Scans tag org files (notes/tags and people/tags) and creates placeholder
markdown pages under content/notes/ so that Hugo can resolve internal links
to topics that have no full note yet.

Usage:
    python generate-topic-pages.py              # Full run
    python generate-topic-pages.py --dry-run    # Preview without writing
"""

import argparse
import re
from pathlib import Path

from lib import escape_yaml_string

# === Constants ===

SCRIPTS_DIR = Path(__file__).parent
HUGO_ROOT = SCRIPTS_DIR.parent
NOTES_DIR = HUGO_ROOT / "content" / "notes"

DROPBOX = Path.home() / "Library" / "CloudStorage" / "Dropbox"
TAG_DIRS = [
    DROPBOX / "notes" / "tags",
    DROPBOX / "people" / "tags",
]


# === Helpers ===


def extract_title(org_path: Path) -> str | None:
    """Extract the #+title: value from an org file.

    Returns None if the file cannot be read or has no title line.
    """
    try:
        text = org_path.read_text()
    except (OSError, UnicodeDecodeError):
        return None

    for line in text.splitlines():
        m = re.match(r"#\+title:\s*(.+)", line, re.IGNORECASE)
        if m:
            return m.group(1).strip()
    return None


def generate_stub_page(title: str) -> str:
    """Generate YAML front matter for a topic stub page."""
    lines = [
        "---",
        f'title: "{escape_yaml_string(title)}"',
        "is_topic_stub: true",
        "draft: false",
        "---",
        "",
    ]
    return "\n".join(lines)


# === Main ===


def main():
    parser = argparse.ArgumentParser(
        description="Generate Hugo topic stub pages from org-roam tag files"
    )
    parser.add_argument("--dry-run", action="store_true", help="Preview without writing")
    args = parser.parse_args()

    stats = {"created": 0, "updated": 0, "unchanged": 0, "skipped": 0, "unreadable": 0}

    NOTES_DIR.mkdir(parents=True, exist_ok=True)

    # Collect all (slug, title) pairs from tag directories
    stubs: dict[str, str] = {}  # slug -> title
    for tag_dir in TAG_DIRS:
        if not tag_dir.exists():
            print(f"  WARNING: {tag_dir} does not exist, skipping")
            continue
        for org_file in sorted(tag_dir.glob("*.org")):
            slug = org_file.stem
            title = extract_title(org_file)
            if title is None:
                stats["unreadable"] += 1
                continue
            # If duplicate slug across directories, first one wins
            if slug not in stubs:
                stubs[slug] = title

    print(f"  Found {len(stubs)} topic stubs in tag directories")

    for slug in sorted(stubs):
        title = stubs[slug]
        md_path = NOTES_DIR / f"{slug}.md"
        page_content = generate_stub_page(title)

        if md_path.exists():
            existing = md_path.read_text()
            # Skip if the existing file is a real note (not a stub)
            if "is_topic_stub: true" not in existing:
                stats["skipped"] += 1
                continue
            # Existing stub — update if title changed
            if existing == page_content:
                stats["unchanged"] += 1
                continue
            stats["updated"] += 1
            if args.dry_run:
                print(f"  [UPDATE] {slug}.md")
            else:
                md_path.write_text(page_content)
        else:
            stats["created"] += 1
            if args.dry_run:
                print(f"  [CREATE] {slug}.md")
            else:
                md_path.write_text(page_content)

    print(f"\n  Topic pages created:   {stats['created']}")
    print(f"  Topic pages updated:   {stats['updated']}")
    print(f"  Unchanged:             {stats['unchanged']}")
    print(f"  Skipped (real notes):  {stats['skipped']}")
    print(f"  Unreadable org files:  {stats['unreadable']}")
    if args.dry_run:
        print("  *** DRY RUN — no files written ***")

    print("\nDone.")


if __name__ == "__main__":
    main()
