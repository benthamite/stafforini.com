#!/usr/bin/env python3
"""Find PDFs with highlights not yet exported to their org notes.

Compares the number of highlight annotations in each PDF against the
number of #+begin_quote blocks in the corresponding org file. Reports
files where highlights > quotes (likely unexported passages).

Note: a single highlighted passage spanning two pages produces two PDF
annotations, so the highlight count may be slightly inflated. Small
discrepancies (1-2) may be false positives.

Usage:
    python check-unexported-highlights.py             # Full scan
    python check-unexported-highlights.py --limit 50  # Scan first 50
    python check-unexported-highlights.py --min-gap 3  # Only report gaps >= 3
"""

import argparse
import os
import re
from pathlib import Path

import pikepdf
from pikepdf import Name

from lib import is_dataless

# === Constants ===

BIBLIO_NOTES_DIR = Path.home() / "My Drive" / "bibliographic-notes"



# === Helpers ===


def resolve_noter_document(raw_path: str) -> Path | None:
    """Resolve a :NOTER_DOCUMENT: value to an absolute Path."""
    if not raw_path or not raw_path.strip():
        return None
    p = raw_path.strip()
    # Normalise legacy path prefixes
    p = p.replace("~/google drive/", "~/My Drive/")
    p = p.replace("~/Google Drive/", "~/My Drive/")
    # Handle relative paths (../../../../My Drive/...)
    if p.startswith("../"):
        p = str((BIBLIO_NOTES_DIR / p).resolve())
    expanded = Path(p).expanduser()
    if expanded.suffix.lower() != ".pdf":
        return None
    return expanded


def count_pdf_highlights(pdf_path: Path) -> int:
    """Count highlight annotations in a PDF."""
    count = 0
    try:
        with pikepdf.open(pdf_path) as pdf:
            for page in pdf.pages:
                if Name.Annots not in page:
                    continue
                for annot in page[Name.Annots]:
                    if annot is None:
                        continue
                    try:
                        if annot.get(Name.Subtype) == Name.Highlight:
                            count += 1
                    except (AttributeError, pikepdf.PdfError):
                        continue
    except (pikepdf.PdfError, pikepdf.PasswordError, OSError):
        return -1  # signal error
    return count


def count_org_quotes(org_path: Path) -> int:
    """Count #+begin_quote blocks in an org file."""
    text = org_path.read_text(errors="replace")
    return len(re.findall(r"#\+begin_quote", text, re.IGNORECASE))


# === Main ===


def main():
    parser = argparse.ArgumentParser(
        description="Find PDFs with unexported highlights"
    )
    parser.add_argument("--limit", type=int, default=0, help="Max files to scan")
    parser.add_argument(
        "--min-gap", type=int, default=1,
        help="Minimum (highlights - quotes) to report (default: 1)"
    )
    args = parser.parse_args()

    if not BIBLIO_NOTES_DIR.exists():
        print(f"ERROR: {BIBLIO_NOTES_DIR} does not exist")
        return

    org_files = sorted(BIBLIO_NOTES_DIR.glob("*.org"))
    stats = {
        "scanned": 0,
        "skipped_dataless": 0,
        "skipped_no_pdf": 0,
        "skipped_pdf_missing": 0,
        "skipped_pdf_dataless": 0,
        "pdf_errors": 0,
        "no_highlights": 0,
        "fully_exported": 0,
        "with_gap": 0,
    }
    gaps = []

    for org_path in org_files:
        if args.limit and stats["scanned"] >= args.limit:
            break

        if is_dataless(org_path):
            stats["skipped_dataless"] += 1
            continue

        stats["scanned"] += 1

        # Extract :NOTER_DOCUMENT:
        text = org_path.read_text(errors="replace")
        m = re.search(r":NOTER_DOCUMENT:\s+(.+)", text)
        if not m:
            stats["skipped_no_pdf"] += 1
            continue

        pdf_path = resolve_noter_document(m.group(1))
        if not pdf_path:
            stats["skipped_no_pdf"] += 1
            continue

        if not pdf_path.exists():
            stats["skipped_pdf_missing"] += 1
            continue

        if is_dataless(pdf_path):
            stats["skipped_pdf_dataless"] += 1
            continue

        highlight_count = count_pdf_highlights(pdf_path)
        if highlight_count < 0:
            stats["pdf_errors"] += 1
            continue

        if highlight_count == 0:
            stats["no_highlights"] += 1
            continue

        quote_count = count_org_quotes(org_path)
        gap = highlight_count - quote_count

        if gap >= args.min_gap:
            stats["with_gap"] += 1
            gaps.append({
                "file": org_path.name,
                "highlights": highlight_count,
                "quotes": quote_count,
                "gap": gap,
            })
        else:
            stats["fully_exported"] += 1

        if stats["scanned"] % 500 == 0:
            print(f"  ... {stats['scanned']} files scanned")

    # Sort by gap descending
    gaps.sort(key=lambda g: g["gap"], reverse=True)

    # Report
    print(f"\n{'=' * 60}")
    print(f"Scan complete: {stats['scanned']} org files checked")
    print(f"{'=' * 60}")
    print(f"  Skipped (dataless org):  {stats['skipped_dataless']}")
    print(f"  Skipped (no PDF ref):    {stats['skipped_no_pdf']}")
    print(f"  Skipped (PDF missing):   {stats['skipped_pdf_missing']}")
    print(f"  Skipped (PDF dataless):  {stats['skipped_pdf_dataless']}")
    print(f"  PDF read errors:         {stats['pdf_errors']}")
    print(f"  No highlights in PDF:    {stats['no_highlights']}")
    print(f"  Fully exported:          {stats['fully_exported']}")
    print(f"  With unexported (gap>={args.min_gap}): {stats['with_gap']}")

    if gaps:
        print(f"\n{'=' * 60}")
        print(f"Files with unexported highlights (gap >= {args.min_gap}):")
        print(f"{'=' * 60}")
        print(f"{'File':<55} {'HL':>4} {'QT':>4} {'Gap':>4}")
        print(f"{'-'*55} {'-'*4} {'-'*4} {'-'*4}")
        for g in gaps:
            name = g["file"]
            if len(name) > 54:
                name = name[:51] + "..."
            print(f"{name:<55} {g['highlights']:>4} {g['quotes']:>4} {g['gap']:>4}")
        print(
            f"\nNote: cross-page highlights inflate the HL count slightly. "
            f"Small gaps (1-2) may be false positives."
        )


if __name__ == "__main__":
    main()
