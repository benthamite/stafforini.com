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
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

import pikepdf
from PIL import Image
from pikepdf import Name

from lib import BIB_FILES, cite_key_to_slug, extract_pdf_path, parse_bib_entries, safe_remove

# === Constants ===

SCRIPTS_DIR = Path(__file__).parent
HUGO_ROOT = SCRIPTS_DIR.parent
PDFS_DIR = HUGO_ROOT / "static" / "pdfs"
THUMBS_DIR = HUGO_ROOT / "static" / "pdf-thumbnails"
MANIFEST_PATH = PDFS_DIR / ".manifest.json"

MUTOOL = shutil.which("mutool") or "/opt/homebrew/bin/mutool"
THUMB_DPI = 150

DB_BIB = Path.home() / "My Drive/repos/babel-refs/bib/db.bib"


# === Helpers ===


def load_manifest() -> dict:
    """Load the incremental processing manifest."""
    if MANIFEST_PATH.exists():
        return json.loads(MANIFEST_PATH.read_text())
    return {}


def save_manifest(manifest: dict) -> None:
    """Persist the manifest to disk atomically."""
    tmp_fd, tmp_path = tempfile.mkstemp(dir=str(MANIFEST_PATH.parent), suffix=".tmp")
    try:
        with os.fdopen(tmp_fd, "w") as f:
            json.dump(manifest, f, indent=2)
            f.write("\n")
        os.replace(tmp_path, str(MANIFEST_PATH))
    except BaseException:
        os.unlink(tmp_path)
        raise


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
    return abs(current_mtime - entry.get("src_mtime", 0)) > 0.001


def strip_annotations(src: Path, dst: Path) -> None:
    """Copy *src* to *dst* with all page annotations removed."""
    with pikepdf.open(src) as pdf:
        for page in pdf.pages:
            if Name.Annots in page:
                del page[Name.Annots]
        try:
            pdf.save(dst)
        except pikepdf.PdfError:
            pdf.save(dst, fix_metadata_version=False)
            print(f"  NOTICE: {src.name} has malformed XMP metadata (saved without fixing)")
            return



def _render_page(pdf_path: Path, out_png: Path, page_num: int) -> bool:
    """Render a single page of *pdf_path* to *out_png*. Returns True on success."""
    result = subprocess.run(
        [
            MUTOOL, "draw",
            "-o", str(out_png),
            "-r", str(THUMB_DPI),
            "-F", "png",
            str(pdf_path),
            str(page_num),
        ],
        capture_output=True,
        text=True,
    )
    return result.returncode == 0


# A page is considered blank when the fraction of near-white pixels exceeds
# this threshold.  Near-white = every RGB channel >= BLANK_CHANNEL_MIN.
BLANK_THRESHOLD = 0.99
BLANK_CHANNEL_MIN = 250
MAX_PAGES_TO_CHECK = 5


def _is_blank(png_path: Path) -> bool:
    """Return True if the rendered page image is essentially blank (all white)."""
    img = Image.open(png_path).convert("RGB")
    pixels = img.getdata()
    white_count = sum(
        1 for r, g, b in pixels
        if r >= BLANK_CHANNEL_MIN and g >= BLANK_CHANNEL_MIN and b >= BLANK_CHANNEL_MIN
    )
    total = len(pixels)
    if total == 0:
        return True  # treat empty/degenerate image as blank
    return white_count / total >= BLANK_THRESHOLD


def render_thumbnail(pdf_path: Path, out_png: Path) -> bool:
    """Render the first non-blank page of *pdf_path* to *out_png* at THUMB_DPI.

    Checks up to MAX_PAGES_TO_CHECK pages, skipping blank ones.
    Returns True on success, False on failure.
    """
    with pikepdf.open(pdf_path) as pdf:
        num_pages = len(pdf.pages)

    pages_to_check = min(num_pages, MAX_PAGES_TO_CHECK)

    for page_num in range(1, pages_to_check + 1):
        if page_num == 1:
            # Render directly to the output path for the common case
            if not _render_page(pdf_path, out_png, page_num):
                return False
            if not _is_blank(out_png):
                return True
        else:
            # Render to a temp file, then move if non-blank
            with tempfile.NamedTemporaryFile(suffix=".png", delete=False) as tmp:
                tmp_path = Path(tmp.name)
            try:
                if not _render_page(pdf_path, tmp_path, page_num):
                    tmp_path.unlink(missing_ok=True)
                    continue
                if not _is_blank(tmp_path):
                    tmp_path.replace(out_png)
                    return True
                tmp_path.unlink(missing_ok=True)
            except Exception:
                tmp_path.unlink(missing_ok=True)
                raise

    # All checked pages were blank — keep the first page render
    if not out_png.exists():
        _render_page(pdf_path, out_png, 1)
    return True


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
        "removed": 0,
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
        except (pikepdf.PdfError, OSError, subprocess.SubprocessError, ValueError) as exc:
            print(f"  WARNING: error processing {slug}: {exc}")
            dst_pdf.unlink(missing_ok=True)
            dst_thumb.unlink(missing_ok=True)
            stats["errors"] += 1

    if not dry_run:
        # Remove stale outputs no longer backed by a bib entry
        valid_slugs = {e["slug"] for e in entries}
        for pdf_file in sorted(PDFS_DIR.glob("*.pdf")):
            if pdf_file.stem not in valid_slugs:
                safe_remove(pdf_file)
                stats["removed"] += 1
        for thumb_file in sorted(THUMBS_DIR.glob("*.png")):
            if thumb_file.stem not in valid_slugs:
                safe_remove(thumb_file)
                stats["removed"] += 1
        # Prune manifest entries for removed slugs
        for stale_key in [k for k in manifest if k not in valid_slugs]:
            del manifest[stale_key]
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
    print(f"  Stale removed:   {stats['removed']}")
    print(f"  Errors:          {stats['errors']}")
    if args.dry_run:
        print("  *** DRY RUN — no files written ***")

    print("\nDone.")

    # Exit with error code if there were failures but no successes at all
    if stats["errors"] and not stats["processed"] and not stats["skipped_cached"]:
        sys.exit(1)


if __name__ == "__main__":
    main()
