#!/usr/bin/env python3
"""Merge migration-rebuilt.bib entries into new.bib and remove the same
cite keys from migration.bib.

Workflow:
  1. Read every entry from ~/My Drive/bibliography/migration-rebuilt.bib.
  2. For each entry, ensure the same cite key isn't already present in
     ~/My Drive/bibliography/new.bib (avoid silent collisions).
  3. Append the rebuilt entry to new.bib.
  4. Remove the same key from migration.bib (skip silently if absent —
     manual additions like Borges1949Teologos won't be in migration.bib).

Backups are written alongside the originals as ``*.bak`` before any edits.
"""

import re
import shutil
import sys
from pathlib import Path

REBUILT = Path.home() / "My Drive/bibliography/migration-rebuilt.bib"
NEW = Path.home() / "My Drive/bibliography/new.bib"
MIGRATION = Path.home() / "My Drive/bibliography/migration.bib"

KEY_RE = re.compile(r"^@(\w+)\s*\{\s*([^,\s]+)\s*,", re.MULTILINE)
ENTRY_SPLIT_RE = re.compile(r"(?=^@\w+\s*\{)", re.MULTILINE)


def parse_entries(text: str) -> list[tuple[str, str]]:
    """Return [(cite_key, full_entry_text)] for every @entry in ``text``."""
    out = []
    for chunk in ENTRY_SPLIT_RE.split(text):
        m = KEY_RE.match(chunk)
        if m:
            out.append((m.group(2), chunk.rstrip() + "\n"))
    return out


def keys_in(text: str) -> set[str]:
    return {m.group(2) for m in KEY_RE.finditer(text)}


def remove_entries(text: str, keys: set[str]) -> tuple[str, set[str]]:
    """Return (new text, set of keys actually removed)."""
    removed = set()
    out_chunks = []
    for chunk in ENTRY_SPLIT_RE.split(text):
        m = KEY_RE.match(chunk)
        if m and m.group(2) in keys:
            removed.add(m.group(2))
            continue
        out_chunks.append(chunk)
    return "".join(out_chunks), removed


def main() -> int:
    if not REBUILT.exists():
        print(f"FATAL: {REBUILT} not found", file=sys.stderr)
        return 1

    rebuilt_text = REBUILT.read_text(encoding="utf-8")
    new_text = NEW.read_text(encoding="utf-8")
    mig_text = MIGRATION.read_text(encoding="utf-8")

    rebuilt_entries = parse_entries(rebuilt_text)
    rebuilt_keys = {k for k, _ in rebuilt_entries}
    new_keys = keys_in(new_text)
    mig_keys = keys_in(mig_text)

    print(f"Rebuilt entries to merge:        {len(rebuilt_entries)}")
    print(f"Existing cite keys in new.bib:   {len(new_keys)}")
    print(f"Existing cite keys in migration.bib: {len(mig_keys)}")
    print()

    # Detect collisions: a rebuilt key that already exists in new.bib means
    # the merge would create a duplicate. Refuse to proceed.
    collisions = rebuilt_keys & new_keys
    if collisions:
        print(f"ERROR: {len(collisions)} rebuilt cite keys already exist in new.bib:",
              file=sys.stderr)
        for k in sorted(collisions):
            print(f"  {k}", file=sys.stderr)
        print("Refusing to merge. Resolve these first.", file=sys.stderr)
        return 2

    # Backup
    shutil.copy2(NEW, NEW.with_suffix(".bib.bak"))
    shutil.copy2(MIGRATION, MIGRATION.with_suffix(".bib.bak"))
    print(f"Backups: {NEW.with_suffix('.bib.bak').name},"
          f" {MIGRATION.with_suffix('.bib.bak').name}")

    # Append rebuilt entries to new.bib
    body = []
    for key, entry in rebuilt_entries:
        body.append(entry)
    appended_text = (
        new_text.rstrip()
        + "\n\n"
        + "% --- Merged from migration-rebuilt.bib ---\n"
        + "\n".join(body)
        + "\n"
    )
    NEW.write_text(appended_text, encoding="utf-8")
    print(f"Appended {len(rebuilt_entries)} entries to {NEW.name}")

    # Remove from migration.bib
    new_mig_text, removed = remove_entries(mig_text, rebuilt_keys)
    not_in_mig = rebuilt_keys - removed
    MIGRATION.write_text(new_mig_text, encoding="utf-8")
    print(f"Removed {len(removed)} entries from {MIGRATION.name}")
    if not_in_mig:
        print(f"  ({len(not_in_mig)} rebuilt keys were not in migration.bib;"
              f" presumably manual additions)")
        for k in sorted(not_in_mig):
            print(f"    {k}")

    # Sanity check: re-parse and confirm
    new_after = keys_in(NEW.read_text(encoding="utf-8"))
    mig_after = keys_in(MIGRATION.read_text(encoding="utf-8"))
    print()
    print(f"After merge: new.bib has {len(new_after)} keys, migration.bib has {len(mig_after)}")
    missing_in_new = rebuilt_keys - new_after
    if missing_in_new:
        print(f"ERROR: {len(missing_in_new)} rebuilt keys not in new.bib after merge:",
              file=sys.stderr)
        for k in sorted(missing_in_new):
            print(f"  {k}", file=sys.stderr)
        return 3

    print("Merge complete.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
