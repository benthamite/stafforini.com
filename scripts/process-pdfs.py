#!/usr/bin/env python3
"""Process PDFs for work pages: strip annotations and generate first-page thumbnails.

For each BibTeX entry with a ``file`` field pointing to a PDF:
  1. Strip annotations/highlights using pikepdf (produces a clean copy).
  2. Render the first page as a PNG thumbnail via ``mutool draw``.

Outputs are written to ``static/pdfs/`` and ``static/pdf-thumbnails/``.
An incremental manifest avoids reprocessing unchanged files.

Usage:
    python process-pdfs.py                # Full incremental run
    python process-pdfs.py --limit 10     # Process at most 10 PDFs
    python process-pdfs.py --force        # Reprocess everything
    python process-pdfs.py --dry-run      # Preview without writing
"""

import argparse
import json
import subprocess
import sys
from pathlib import Path

import pikepdf
from pikepdf import Name

from lib import BIB_FILES, cite_key_to_slug, extract_pdf_path, parse_bib_entries

# === Constants ===

SCRIPTS_DIR = Path(__file__).parent
HUGO_ROOT = SCRIPTS_DIR.parent
PDFS_DIR = HUGO_ROOT / "static" / "pdfs"
THUMBS_DIR = HUGO_ROOT / "static" / "pdf-thumbnails"
MANIFEST_PATH = PDFS_DIR / ".manifest.json"

MUTOOL = "/opt/homebrew/bin/mutool"
THUMB_DPI = 150

DB_BIB = Path.home() / "Library/CloudStorage/Dropbox/repos/babel-refs/bib/db.bib"


# === Helpers ===


def load_manifest() -> dict:
    """Load the incremental processing manifest."""
    if MANIFEST_PATH.exists():
        return json.loads(MANIFEST_PATH.read_text())
    return {}


def save_manifest(manifest: dict) -> None:
    """Persist the manifest to disk."""
    MANIFEST_PATH.write_text(json.dumps(manifest, indent=2) + "\n")


def needs_processing(slug: str, src_path: Path, manifest: dict, force: bool) -> bool:
    """Check whether a PDF needs (re)processing."""
    if force:
        return True
    entry = manifest.get(slug)
    if not entry:
        return True
    try:
        current_mtime = src_path.stat().st_mtime
    except OSError:
        return True
    return current_mtime != entry.get("src_mtime")


def strip_annotations(src: Path, dst: Path) -> None:
    """Copy *src* to *dst* with all page annotations removed."""
    with pikepdf.open(src) as pdf:
        for page in pdf.pages:
            if Name.Annots in page:
                del page[Name.Annots]
        pdf.save(dst)


def render_thumbnail(pdf_path: Path, out_png: Path) -> bool:
    """Render the first page of *pdf_path* to *out_png* at THUMB_DPI.

    Returns True on success, False on failure.
    """
    result = subprocess.run(
        [
            MUTOOL, "draw",
            "-o", str(out_png),
            "-r", str(THUMB_DPI),
            "-F", "png",
            str(pdf_path),
            "1",  # first page only
        ],
        capture_output=True,
        text=True,
    )
    return result.returncode == 0


# === Main logic ===


def collect_entries() -> list[dict]:
    """Parse all bib files and return entries with PDF paths.

    Skips db.bib entries that have a non-empty ``translator`` field.
    """
    entries = []
    for bib_path in BIB_FILES:
        if not bib_path.exists():
            print(f"  WARNING: {bib_path} not found, skipping")
            continue

        is_db = bib_path.resolve() == DB_BIB.resolve()

        parsed = parse_bib_entries(
            bib_path,
            strip_braces=False,
            extra_fields=["file", "translator"],
        )

        for entry in parsed:
            # Skip db.bib translations
            if is_db and entry.get("translator", "").strip():
                continue

            file_field = entry.get("file", "")
            if not file_field:
                continue

            pdf_path = extract_pdf_path(file_field)
            if pdf_path is None:
                continue

            entries.append({
                "cite_key": entry["cite_key"],
                "slug": cite_key_to_slug(entry["cite_key"]),
                "pdf_path": pdf_path,
            })

    return entries


def process_pdfs(entries: list[dict], *, dry_run: bool, force: bool, limit: int) -> dict:
    """Process PDF entries: strip annotations + render thumbnails."""
    stats = {
        "processed": 0,
        "skipped_cached": 0,
        "skipped_missing": 0,
        "errors": 0,
    }

    manifest = load_manifest() if not dry_run else {}

    for entry in entries:
        if limit and stats["processed"] >= limit:
            break

        slug = entry["slug"]
        src = entry["pdf_path"]

        if not src.exists():
            stats["skipped_missing"] += 1
            continue

        if not needs_processing(slug, src, manifest, force):
            stats["skipped_cached"] += 1
            continue

        dst_pdf = PDFS_DIR / f"{slug}.pdf"
        dst_thumb = THUMBS_DIR / f"{slug}.png"

        if dry_run:
            stats["processed"] += 1
            if stats["processed"] <= 10:
                print(f"  [DRY RUN] {slug}")
            continue

        try:
            # Strip annotations
            strip_annotations(src, dst_pdf)

            # Render thumbnail
            if not render_thumbnail(dst_pdf, dst_thumb):
                print(f"  WARNING: mutool failed for {slug}, removing partial output")
                dst_pdf.unlink(missing_ok=True)
                dst_thumb.unlink(missing_ok=True)
                stats["errors"] += 1
                continue

            # Update manifest
            manifest[slug] = {
                "src_mtime": src.stat().st_mtime,
                "src_path": str(src),
            }
            stats["processed"] += 1

            if stats["processed"] % 200 == 0:
                print(f"  ... {stats['processed']} PDFs processed")
                # Checkpoint manifest periodically
                save_manifest(manifest)

        except pikepdf.PasswordError:
            print(f"  WARNING: password-protected, skipping: {slug}")
            stats["errors"] += 1
        except Exception as exc:
            print(f"  WARNING: error processing {slug}: {exc}")
            dst_pdf.unlink(missing_ok=True)
            dst_thumb.unlink(missing_ok=True)
            stats["errors"] += 1

    if not dry_run:
        save_manifest(manifest)

    return stats


def main():
    parser = argparse.ArgumentParser(
        description="Strip PDF annotations and generate first-page thumbnails"
    )
    parser.add_argument("--dry-run", action="store_true", help="Preview without writing")
    parser.add_argument("--force", action="store_true", help="Reprocess all PDFs")
    parser.add_argument("--limit", type=int, default=0, help="Max PDFs to process")
    args = parser.parse_args()

    print("=" * 60)
    print("Processing PDFs for work pages")
    print("=" * 60)

    # Ensure output directories exist
    if not args.dry_run:
        PDFS_DIR.mkdir(parents=True, exist_ok=True)
        THUMBS_DIR.mkdir(parents=True, exist_ok=True)

    # Collect entries
    print("\n  Parsing bib files...")
    entries = collect_entries()
    print(f"  Found {len(entries)} entries with PDF paths")

    # Deduplicate by slug (first occurrence wins)
    seen = set()
    unique = []
    for e in entries:
        if e["slug"] not in seen:
            seen.add(e["slug"])
            unique.append(e)
    entries = unique
    print(f"  Unique slugs: {len(entries)}")

    # Process
    print("\n  Processing...")
    stats = process_pdfs(
        entries,
        dry_run=args.dry_run,
        force=args.force,
        limit=args.limit,
    )

    print(f"\n  Processed:       {stats['processed']}")
    print(f"  Cached (skipped): {stats['skipped_cached']}")
    print(f"  Missing source:  {stats['skipped_missing']}")
    print(f"  Errors:          {stats['errors']}")
    if args.dry_run:
        print("  *** DRY RUN — no files written ***")

    print("\nDone.")

    # Exit with error code if there were failures but no successes
    if stats["errors"] and not stats["processed"]:
        sys.exit(1)


if __name__ == "__main__":
    main()
