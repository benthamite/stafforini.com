#!/usr/bin/env python3
"""Move locators from org heading titles into proper org-cite citations.

Finds non-diary quote headings (level 3) that have locators like ", p. 245"
appended to the heading text, removes the locator from the heading, and
adds a [cite:@KEY, LOCATOR] line after the #+end_quote block.

Usage:
    python scripts/fix-quote-locators.py --dry-run    # preview changes
    python scripts/fix-quote-locators.py               # apply changes
"""

import re
import sys
from pathlib import Path

NOTES_DIR = Path("/Users/pablostafforini/My Drive/bibliographic-notes")

# Match a locator suffix at the end of an org heading.
# Captures the locator text (e.g. "p. 245", "pp. 36-37", "sec. 3").
LOCATOR_RE = re.compile(
    r',\s+'
    r'((?:pp?|ch|sec|vol|bk|pt|para|no|fn|loc|ll?|fig|tab|tbl|app)\.\s*.+?)'
    r'\s*$',
    re.IGNORECASE
)


def process_file(filepath: Path, dry_run: bool = False) -> list[dict]:
    """Process a single org file. Returns list of changes made."""
    citation_key = filepath.stem

    with open(filepath, 'r', encoding='utf-8') as f:
        lines = f.readlines()

    changes = []
    new_lines = list(lines)
    offset = 0  # tracks line insertions shifting indices

    i = 0
    while i < len(lines):
        line = lines[i]

        # Match exactly level-3 headings (*** but not ****)
        if not line.startswith('*** ') or line.startswith('**** '):
            i += 1
            continue

        heading = line.rstrip('\n')
        m = LOCATOR_RE.search(heading)
        if not m:
            i += 1
            continue

        locator = m.group(1).strip()
        clean_heading = heading[:m.start()].rstrip()

        # Skip diary quotes (have :EXPORT_FILE_NAME: in their properties)
        is_diary = False
        j = i + 1
        while j < len(lines) and not lines[j].startswith('*'):
            stripped = lines[j].strip()
            if stripped == ':PROPERTIES:':
                pass
            elif stripped == ':END:':
                break
            elif ':EXPORT_FILE_NAME:' in stripped:
                is_diary = True
                break
            j += 1

        if is_diary:
            i += 1
            continue

        # Find the #+end_quote after this heading
        end_quote_idx = None
        j = i + 1
        while j < len(lines):
            if lines[j].startswith('*'):
                break
            if lines[j].strip().lower() == '#+end_quote':
                end_quote_idx = j
                break
            j += 1

        if end_quote_idx is None:
            i += 1
            continue

        # Check for existing [cite:...] after #+end_quote
        existing_cite_idx = None
        k = end_quote_idx + 1
        while k < len(lines):
            stripped = lines[k].strip()
            if stripped == '':
                k += 1
                continue
            if stripped.startswith('[cite:'):
                existing_cite_idx = k
            break

        change = {
            'file': filepath.name,
            'line': i + 1,
            'old_heading': heading,
            'new_heading': clean_heading,
            'locator': locator,
        }

        # Clean the heading first, before any insertions shift the offset
        if not dry_run:
            new_lines[i + offset] = clean_heading + '\n'

        if existing_cite_idx is not None:
            existing_cite = lines[existing_cite_idx].strip()
            # Check if citation already has a locator (comma after the key)
            if ',' in existing_cite.split('@', 1)[-1]:
                change['action'] = 'clean_heading_only'
            else:
                # Citation without locator — add locator to it
                old_cite = existing_cite
                new_cite = existing_cite[:-1] + f', {locator}]'
                change['action'] = 'update_cite'
                change['old_cite'] = old_cite
                change['new_cite'] = new_cite
                if not dry_run:
                    new_lines[existing_cite_idx + offset] = \
                        lines[existing_cite_idx].replace(old_cite, new_cite)
        else:
            # No citation — add one after #+end_quote
            cite_line = f'\n[cite:@{citation_key}, {locator}]\n'
            change['action'] = 'add_cite'
            change['cite'] = cite_line.strip()
            if not dry_run:
                new_lines.insert(end_quote_idx + 1 + offset, cite_line)
                offset += 1

        changes.append(change)
        i += 1

    if changes and not dry_run:
        with open(filepath, 'w', encoding='utf-8') as f:
            f.writelines(new_lines)

    return changes


def main():
    dry_run = '--dry-run' in sys.argv

    if not NOTES_DIR.is_dir():
        print(f"Error: {NOTES_DIR} is not a directory", file=sys.stderr)
        sys.exit(1)

    total = 0
    files_changed = 0

    for filepath in sorted(NOTES_DIR.glob('*.org')):
        changes = process_file(filepath, dry_run=dry_run)
        if not changes:
            continue
        files_changed += 1
        total += len(changes)
        for c in changes:
            prefix = '[DRY RUN] ' if dry_run else ''
            print(f"{prefix}{c['file']}:{c['line']}")
            print(f"  - {c['old_heading']}")
            print(f"  + {c['new_heading']}")
            if c['action'] == 'add_cite':
                print(f"  + {c['cite']}")
            elif c['action'] == 'update_cite':
                print(f"  cite - {c['old_cite']}")
                print(f"  cite + {c['new_cite']}")
            elif c['action'] == 'clean_heading_only':
                print(f"  (citation already has locator)")
            print()

    label = '[DRY RUN] ' if dry_run else ''
    print(f"{label}Total: {total} changes across {files_changed} files")


if __name__ == '__main__':
    main()
