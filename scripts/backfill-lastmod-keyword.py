#!/usr/bin/env python3
"""Seed #+lastmod: keyword in org note files from git history.

One-off migration step: writes the current git-derived lastmod into
each org file as a #+lastmod: keyword, giving the new authoritative
source (the keyword) a sensible baseline. After this runs, future
interactive edits are stamped by the Elisp save-hook, and
inject-lastmod.py reads the keyword instead of git.

Commits whose message matches --skip-patterns are excluded when
picking the date — this is how to strip known bulk-mechanical
commits (property migrations, merge commits) from the baseline.

The script is idempotent: files that already have #+lastmod: are
skipped. Safe to re-run.

Usage:
    python scripts/backfill-lastmod-keyword.py [--dry-run]
    python scripts/backfill-lastmod-keyword.py --skip-patterns "^Migrate " "^Merge "
"""

from __future__ import annotations

import argparse
import os
import re
import subprocess
import sys
from datetime import datetime
from pathlib import Path

from lib import (
    NOTES_DIR,
    atomic_write_text,
    extract_export_file_names,
    find_org_files,
    is_dataless,
)


# Default set of commit-message patterns to exclude when picking the
# baseline lastmod. Intentionally conservative: only obvious mechanical
# commits. Users can override via --skip-patterns.
DEFAULT_SKIP_PATTERNS: list[str] = [
    r"^Migrate ",   # bulk property / format migrations (e.g. CATEGORY → TOPICS)
    r"^Merge ",     # merge commits reconciling divergence across machines
    r"^Regenerate ",  # auto-regeneration commits
]


_LASTMOD_RE = re.compile(r"^#\+lastmod:", re.IGNORECASE | re.MULTILINE)
_TITLE_RE = re.compile(r"^#\+title:", re.IGNORECASE)

# `--format=%ad|%s` lines start with a fixed-width ISO date followed by '|'.
_HEADER_RE = re.compile(r"^(\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2})\|(.*)$")


def build_commit_history(
    repo_dir: Path, skip_patterns: list[re.Pattern]
) -> dict[Path, str]:
    """Return {org_file: most_recent_non_skipped_commit_date}.

    One git-log call, then filter. Dates are ISO-formatted strings
    (YYYY-MM-DDTHH:MM:SS). Paths are absolute and anchored at *repo_dir*.
    """
    # splitlines() swallows control characters like \x1e, so use a visible
    # separator between date and subject: `YYYY-MM-DDTHH:MM:SS|subject`.
    result = subprocess.run(
        [
            "git", "log",
            "--format=%ad|%s",
            "--date=format:%Y-%m-%dT%H:%M:%S",
            "--name-only",
            "--diff-filter=ACMR",
            "--", "*.org",
        ],
        capture_output=True, text=True, cwd=str(repo_dir), check=True,
    )

    newest: dict[Path, str] = {}
    current_date: str | None = None
    current_skipped = False
    for line in result.stdout.splitlines():
        header = _HEADER_RE.match(line)
        if header:
            current_date = header.group(1)
            current_skipped = any(p.search(header.group(2)) for p in skip_patterns)
            continue
        if not line.strip() or not line.endswith(".org"):
            continue
        if current_skipped or current_date is None:
            continue
        path = (repo_dir / line).resolve()
        newest.setdefault(path, current_date)
    return newest


def insert_lastmod_line(text: str, date: str) -> str:
    """Insert `#+lastmod: DATE` immediately after the first `#+title:` line.

    If no `#+title:` is found, prepend the keyword at the top of the file.
    """
    lines = text.splitlines(keepends=True)
    for i, line in enumerate(lines):
        if _TITLE_RE.match(line):
            suffix = "\n" if line.endswith("\n") else ""
            if not suffix:
                lines[i] = line + "\n"
            lines.insert(i + 1, f"#+lastmod: {date}\n")
            return "".join(lines)
    return f"#+lastmod: {date}\n" + text


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--dry-run", action="store_true",
                        help="Preview without writing.")
    parser.add_argument("--skip-patterns", nargs="*", default=None,
                        help="Regex patterns for commit messages to skip. "
                             "Overrides DEFAULT_SKIP_PATTERNS.")
    parser.add_argument("--notes-dir", default=str(NOTES_DIR),
                        help="Notes repo directory (default: lib.NOTES_DIR).")
    parser.add_argument("--limit", type=int, default=None,
                        help="Stop after N seeds (for testing).")
    parser.add_argument("--all-files", action="store_true",
                        help="Seed every org file under notes-dir. Default is "
                             "to only seed files with at least one "
                             ":EXPORT_FILE_NAME: property (i.e. files that "
                             "actually get published to the site).")
    args = parser.parse_args()

    patterns = DEFAULT_SKIP_PATTERNS if args.skip_patterns is None else args.skip_patterns
    skip_res = [re.compile(p) for p in patterns]
    notes_dir = Path(args.notes_dir).expanduser().resolve()

    print(f"Scanning {notes_dir}")
    print(f"Skip patterns: {patterns}")

    org_files = find_org_files(notes_dir)
    print(f"Found {len(org_files)} org files")

    if not args.all_files:
        org_files = [f for f in org_files if extract_export_file_names(f)]
        print(f"Filtered to {len(org_files)} published files "
              "(pass --all-files to include the rest)")

    print("Building git commit history...")
    history = build_commit_history(notes_dir, skip_res)
    print(f"Git history covers {len(history)} files")

    stats = {"seeded": 0, "already": 0, "dataless": 0, "mtime_fallback": 0}

    for org_file in org_files:
        rel = org_file.relative_to(notes_dir)

        if is_dataless(org_file):
            stats["dataless"] += 1
            continue

        try:
            text = org_file.read_text(errors="replace")
        except OSError as exc:
            print(f"  [SKIP] {rel}: {exc}", file=sys.stderr)
            continue

        if _LASTMOD_RE.search(text):
            stats["already"] += 1
            continue

        resolved = org_file.resolve()
        date = history.get(resolved)
        if not date:
            mtime = os.path.getmtime(org_file)
            date = datetime.fromtimestamp(mtime).strftime("%Y-%m-%dT%H:%M:%S")
            stats["mtime_fallback"] += 1

        new_text = insert_lastmod_line(text, date)
        if not args.dry_run:
            atomic_write_text(org_file, new_text)

        stats["seeded"] += 1
        verb = "would seed" if args.dry_run else "[SEED]"
        if stats["seeded"] <= 10 or stats["seeded"] % 500 == 0:
            print(f"  {verb} {rel} -> {date}")

        if args.limit and stats["seeded"] >= args.limit:
            break

    print()
    print(f"Seeded:          {stats['seeded']}")
    print(f"Already present: {stats['already']}")
    print(f"Mtime fallback:  {stats['mtime_fallback']}")
    print(f"Dataless:        {stats['dataless']}")
    if args.dry_run:
        print("*** DRY RUN — no files written ***")

    return 0


if __name__ == "__main__":
    sys.exit(main())
