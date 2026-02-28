#!/usr/bin/env python3
"""Fix quoted_author front matter for quotes with 'quoted in' attribution.

Reads wp-quotes-matched.json and for entries where attribution_text contains
"quoted in", appends :quoted_author "{name}" to the EXPORT_HUGO_CUSTOM_FRONT_MATTER
property in the corresponding org subheading.

The `author` field in the JSON is the quoted person's name (e.g., "Niels Bohr").

Usage:
    python fix-quoted-authors.py                # Process all matching quotes
    python fix-quoted-authors.py --dry-run      # Preview changes without writing
"""

import argparse
import json
import re
from pathlib import Path

# === Constants ===

SCRIPTS_DIR = Path(__file__).parent
MATCHED_JSON = SCRIPTS_DIR / "wp-quotes-matched.json"
NOTES_DIR = Path.home() / "My Drive/bibliographic-notes"


# === Index building ===


def build_wp_post_id_index(notes_dir: Path) -> dict[str, Path]:
    """Build index mapping WP_POST_ID to org file path.

    Scans all .org files in notes_dir once upfront and extracts
    the :WP_POST_ID: property from level-2 subheadings.

    Returns:
        dict mapping post_id (str) -> file path (Path)
    """
    index = {}
    org_files = list(notes_dir.glob("*.org"))

    for org_path in org_files:
        try:
            content = org_path.read_text()
        except (OSError, UnicodeDecodeError) as exc:
            print(f"  Warning: skipping {org_path.name}: {exc}")
            continue

        # Find all level-2 subheadings and their properties blocks
        # Pattern: ** heading, then :PROPERTIES:, then lines with :KEY: value, then :END:
        for match in re.finditer(
            r"^\*\* .+?\n:PROPERTIES:\n(.*?)\n:END:",
            content,
            re.MULTILINE | re.DOTALL
        ):
            props_block = match.group(1)
            # Extract WP_POST_ID from properties
            wp_match = re.search(r"^:WP_POST_ID:\s*(\d+)\s*$", props_block, re.MULTILINE)
            if wp_match:
                post_id = wp_match.group(1)
                index[post_id] = org_path

    return index


# === Org file modification ===


def append_quoted_author_to_front_matter(content: str, post_id: str, quoted_author: str) -> tuple[str | None, str]:
    """Append :quoted_author to the EXPORT_HUGO_CUSTOM_FRONT_MATTER line.

    Finds the level-2 subheading with the matching WP_POST_ID, then
    locates the EXPORT_HUGO_CUSTOM_FRONT_MATTER line in that same
    properties block and appends :quoted_author "{name}".

    Idempotent: skips if :quoted_author already present.

    Args:
        content: Full org file content
        post_id: The WP_POST_ID to match
        quoted_author: The author name to append

    Returns:
        Tuple of (modified_content, status):
        - modified_content: new content if modified, None otherwise
        - status: "modified", "already_present", or "not_found"
    """
    # Find all level-2 subheadings with properties blocks
    pattern = r"(^\*\* .+?\n:PROPERTIES:\n)(.*?)(\n:END:)"

    # Track state using a mutable container
    state = {"modified": False, "already_present": False}

    def replace_properties(match):
        prefix = match.group(1)
        props_block = match.group(2)
        suffix = match.group(3)

        # Check if this block has the matching WP_POST_ID
        wp_match = re.search(r"^:WP_POST_ID:\s*" + re.escape(post_id) + r"\s*$", props_block, re.MULTILINE)
        if not wp_match:
            # Not the right subheading, leave unchanged
            return match.group(0)

        # Found the right subheading - now find EXPORT_HUGO_CUSTOM_FRONT_MATTER line
        fm_pattern = r"^(:EXPORT_HUGO_CUSTOM_FRONT_MATTER:\s*.*)$"
        fm_match = re.search(fm_pattern, props_block, re.MULTILINE)

        if not fm_match:
            # No EXPORT_HUGO_CUSTOM_FRONT_MATTER in this block - shouldn't happen but skip
            return match.group(0)

        fm_line = fm_match.group(1)

        # Check if :quoted_author already present (idempotent)
        if ":quoted_author" in fm_line:
            # Already has quoted_author, no change needed
            state["already_present"] = True
            return match.group(0)

        # Append :quoted_author "{name}" to the line
        new_fm_line = fm_line + f' :quoted_author "{quoted_author}"'

        # Replace the line in the properties block
        new_props_block = re.sub(fm_pattern, new_fm_line, props_block, count=1, flags=re.MULTILINE)

        state["modified"] = True

        return prefix + new_props_block + suffix

    new_content = re.sub(pattern, replace_properties, content, flags=re.MULTILINE | re.DOTALL)

    if state["already_present"]:
        return None, "already_present"
    elif state["modified"]:
        return new_content, "modified"
    else:
        return None, "not_found"


# === Main processing ===


def main():
    parser = argparse.ArgumentParser(
        description="Fix quoted_author front matter for 'quoted in' attributions"
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Preview changes without writing files"
    )
    args = parser.parse_args()

    print("Loading quotes...")
    quotes = json.loads(MATCHED_JSON.read_text())
    print(f"  {len(quotes)} quotes loaded")

    # Filter to quotes with "quoted in" in attribution_text
    quoted_in_quotes = [
        q for q in quotes
        if q.get("attribution_text", "") and "quoted in" in q["attribution_text"].lower()
    ]
    print(f"  {len(quoted_in_quotes)} quotes with 'quoted in' found")

    if not quoted_in_quotes:
        print("\nNo quotes to process.")
        return

    # Build WP_POST_ID index
    print("\nBuilding WP_POST_ID index...")
    post_id_index = build_wp_post_id_index(NOTES_DIR)
    print(f"  {len(post_id_index)} post IDs indexed")

    # Process quotes
    print("\nProcessing quotes...")
    stats = {
        "modified": 0,
        "already_has_quoted_author": 0,
        "no_org_file": 0,
        "no_post_id": 0,
    }

    for quote in quoted_in_quotes:
        post_id = quote.get("post_id", "")
        if not post_id:
            stats["no_post_id"] += 1
            continue

        quoted_author = quote.get("author", "")
        if not quoted_author:
            # Skip if no author name
            continue

        # Look up org file
        org_path = post_id_index.get(post_id)
        if not org_path:
            stats["no_org_file"] += 1
            if args.dry_run:
                print(f"  [NO_FILE] post_id={post_id}, author={quoted_author}")
            continue

        # Read file
        try:
            content = org_path.read_text()
        except Exception as e:
            print(f"  [ERROR] Failed to read {org_path}: {e}")
            continue

        # Modify content
        new_content, status = append_quoted_author_to_front_matter(content, post_id, quoted_author)

        if status == "already_present":
            stats["already_has_quoted_author"] += 1
            if args.dry_run:
                print(f"  [SKIP] {org_path.name} (post_id={post_id}) - already has :quoted_author")
            continue
        elif status == "not_found":
            # Couldn't find the properties block (shouldn't happen)
            if args.dry_run:
                print(f"  [ERROR] {org_path.name} (post_id={post_id}) - properties not found")
            continue

        # status == "modified"
        if args.dry_run:
            print(f"  [MODIFY] {org_path.name} (post_id={post_id})")
            print(f"           Add :quoted_author \"{quoted_author}\"")
        else:
            from lib import atomic_write_text
            atomic_write_text(org_path, new_content)
            stats["modified"] += 1

    # Print summary
    print(f"\n{'='*60}")
    print("RESULTS")
    print(f"{'='*60}")
    print(f"  Files modified:                {stats['modified']:>5}")
    print(f"  Already has :quoted_author:    {stats['already_has_quoted_author']:>5}")
    print(f"  No org file found:             {stats['no_org_file']:>5}")
    print(f"  No post_id in JSON:            {stats['no_post_id']:>5}")

    if args.dry_run:
        print("\n  *** DRY RUN — no files were modified ***")


if __name__ == "__main__":
    main()
