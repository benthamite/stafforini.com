#!/usr/bin/env python3
"""Incremental org-to-Hugo export orchestrator.

Tracks file modification times in a manifest to only re-export changed files.
Handles stale output cleanup (deleted sources, renamed EXPORT_FILE_NAMEs).

Usage:
    python3 scripts/incremental-export.py quotes [--full]
    python3 scripts/incremental-export.py notes [--full]
"""

import argparse
import json
import os
import re
import subprocess
import sys
import tempfile
from pathlib import Path

from lib import is_dataless

SCRIPT_DIR = Path(__file__).resolve().parent
REPO_ROOT = SCRIPT_DIR.parent

SECTIONS = {
    "quotes": {
        "source_dir": Path.home()
        / "Library/CloudStorage/Dropbox/bibliographic-notes",
        "output_dir": REPO_ROOT / "content/quotes",
        "elisp": SCRIPT_DIR / "export-quotes.el",
        "pre_filter": lambda content: ":public:" in content,
        "skip_files": set(),  # no exclusions for quotes
    },
    "notes": {
        "source_dir": Path.home()
        / "Library/CloudStorage/Dropbox/websites/pablos-miscellany",
        "output_dir": REPO_ROOT / "content/notes",
        "elisp": SCRIPT_DIR / "export-notes.el",
        "pre_filter": lambda content: ":EXPORT_FILE_NAME:" in content,
        # pablos-miscellany.org is the org-roam top-level index file, not an exportable note
        "skip_files": {"pablos-miscellany.org"},
    },
}

EXPORT_FILE_NAME_RE = re.compile(r":EXPORT_FILE_NAME:\s+(\S+)")


# === Manifest helpers ===


def manifest_path(section: str) -> Path:
    return SCRIPT_DIR / f".export-{section}-manifest.json"


def load_manifest(section: str) -> dict | None:
    path = manifest_path(section)
    if not path.exists():
        return None
    with open(path) as f:
        return json.load(f)


def save_manifest(section: str, data: dict) -> None:
    path = manifest_path(section)
    tmp_fd, tmp_path = tempfile.mkstemp(dir=str(path.parent), suffix=".tmp")
    try:
        with os.fdopen(tmp_fd, "w") as f:
            json.dump(data, f, indent=2)
        os.replace(tmp_path, str(path))
    except BaseException:
        os.unlink(tmp_path)
        raise
    print(f"Manifest saved: {path}")


# === File helpers ===


def extract_export_file_names(filepath: Path) -> list[str]:
    """Extract all :EXPORT_FILE_NAME: values from an org file."""
    names = []
    with open(filepath, errors="replace") as f:
        for line in f:
            m = EXPORT_FILE_NAME_RE.search(line)
            if m:
                names.append(m.group(1) + ".md")
    return names


def fix_dataless_files() -> None:
    """Run the dataless fixer script to restore dehydrated Dropbox files from git."""
    fixer = SCRIPT_DIR / "fix-dataless-files.sh"
    if fixer.exists():
        subprocess.run(["bash", str(fixer)], timeout=120)


def scan_source_files(cfg: dict) -> dict[str, float]:
    """Scan source directory, return {filename: mtime} for exportable files."""
    source_dir = cfg["source_dir"]
    pre_filter = cfg["pre_filter"]
    skip_files = cfg["skip_files"]

    result = {}
    skipped_dataless = []
    org_files = sorted(source_dir.glob("*.org"))
    total = len(org_files)
    for i, f in enumerate(org_files, 1):
        if i % 200 == 0 or i == total:
            print(f"  Scanned {i}/{total} files...", flush=True)
        if f.name in skip_files:
            continue
        if is_dataless(f):
            skipped_dataless.append(f.name)
            continue
        try:
            content = f.read_text(errors="replace")
        except OSError:
            continue
        if pre_filter(content):
            result[f.name] = f.stat().st_mtime
    if skipped_dataless:
        print(
            f"WARNING: skipped {len(skipped_dataless)} dataless file(s) "
            f"(Dropbox dehydrated): {', '.join(skipped_dataless[:5])}"
            + (f" ... and {len(skipped_dataless) - 5} more" if len(skipped_dataless) > 5 else ""),
            file=sys.stderr,
        )
    return result


def delete_md_files(output_dir: Path, filenames: list[str]) -> int:
    """Delete specific .md files from the output directory. Returns count."""
    count = 0
    for name in filenames:
        path = output_dir / name
        if path.exists():
            path.unlink()
            print(f"  Deleted stale: {name}")
            count += 1
    return count


def wipe_output_dir(output_dir: Path) -> None:
    """Delete all .md files except _index.md from the output directory."""
    if not output_dir.exists():
        return
    for f in output_dir.glob("*.md"):
        if f.name != "_index.md":
            f.unlink()
    print(f"Wiped all .md files from {output_dir}")


# === Export logic ===


def run_emacs(elisp: Path, file_list_path: str | None = None) -> int:
    """Run Emacs batch export. Returns the exit code."""
    env = os.environ.copy()
    if file_list_path:
        env["EXPORT_FILE_LIST"] = file_list_path

    print(f"Starting Emacs batch export...")
    print(f"Script: {elisp}\n")

    result = subprocess.run(["emacs", "--batch", "-l", str(elisp)], env=env, timeout=600)

    if result.returncode != 0:
        print(
            f"\nWARNING: Emacs exited with code {result.returncode}", file=sys.stderr
        )
    return result.returncode


def run_full_export(section: str, cfg: dict, current_files: dict[str, float]) -> None:
    """Full export: wipe outputs, export everything, build fresh manifest."""
    source_dir = cfg["source_dir"]
    output_dir = cfg["output_dir"]

    wipe_output_dir(output_dir)

    # Call Emacs without EXPORT_FILE_LIST — it uses its own pre-filtering
    run_emacs(cfg["elisp"])

    # Build fresh manifest from all current exportable files
    print("\nBuilding manifest...")
    new_manifest = {"files": {}}
    for name in current_files:
        filepath = source_dir / name
        if filepath.exists():
            new_manifest["files"][name] = {
                "mtime": filepath.stat().st_mtime,
                "outputs": extract_export_file_names(filepath),
            }
    save_manifest(section, new_manifest)


def run_incremental_export(
    section: str,
    cfg: dict,
    manifest: dict,
    current_files: dict[str, float],
) -> None:
    """Incremental export: only re-export changed/new files."""
    source_dir = cfg["source_dir"]
    output_dir = cfg["output_dir"]
    old_files = manifest["files"]

    # Determine changed/new/deleted files
    to_export: list[str] = []
    stale_outputs: list[str] = []

    for name, mtime in current_files.items():
        old = old_files.get(name)
        if old is None:
            to_export.append(name)
        elif abs(mtime - old["mtime"]) > 0.001:
            to_export.append(name)
            stale_outputs.extend(old.get("outputs", []))

    for name, info in old_files.items():
        if name not in current_files:
            stale_outputs.extend(info.get("outputs", []))
            print(f"  Source deleted: {name}")

    # Delete stale outputs
    if stale_outputs:
        print(f"Cleaning {len(stale_outputs)} stale output(s)...")
        delete_md_files(output_dir, stale_outputs)

    # Nothing to export?
    if not to_export:
        print("Nothing to export — all files up to date.")
        return

    print(f"\n{len(to_export)} file(s) to export:")
    for name in to_export[:20]:
        print(f"  {name}")
    if len(to_export) > 20:
        print(f"  ... and {len(to_export) - 20} more")

    # Write file list to temp file and call Emacs
    full_paths = [str(source_dir / name) for name in to_export]
    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".txt", prefix="export-list-", delete=False
    ) as tmp:
        tmp.write("\n".join(full_paths))
        file_list_path = tmp.name

    try:
        run_emacs(cfg["elisp"], file_list_path=file_list_path)
    finally:
        os.unlink(file_list_path)

    # Update manifest
    print("\nUpdating manifest...")
    export_set = set(to_export)
    new_manifest = {"files": {}}
    for name in current_files:
        if name in export_set:
            filepath = source_dir / name
            new_manifest["files"][name] = {
                "mtime": filepath.stat().st_mtime,
                "outputs": extract_export_file_names(filepath),
            }
        else:
            new_manifest["files"][name] = old_files[name]
    save_manifest(section, new_manifest)


# === Entry point ===


def run_export(section: str, full: bool = False) -> None:
    cfg = SECTIONS[section]
    source_dir = cfg["source_dir"]
    output_dir = cfg["output_dir"]

    if not source_dir.exists():
        print(f"ERROR: Source directory not found: {source_dir}", file=sys.stderr)
        sys.exit(1)

    output_dir.mkdir(parents=True, exist_ok=True)

    # Restore any Dropbox-dehydrated files from git before scanning
    fix_dataless_files()

    # Load manifest
    manifest = None if full else load_manifest(section)
    is_full = full or manifest is None

    if is_full and not full:
        print("No manifest found — falling back to full export.")

    print(f"{'Full' if is_full else 'Incremental'} export for {section}")
    print(f"Scanning {source_dir} ...")
    current_files = scan_source_files(cfg)
    print(f"Found {len(current_files)} exportable files.")

    if is_full:
        run_full_export(section, cfg, current_files)
    else:
        run_incremental_export(section, cfg, manifest, current_files)

    print("\nDone.")


def main():
    parser = argparse.ArgumentParser(
        description="Incremental org-to-Hugo export orchestrator"
    )
    parser.add_argument(
        "section",
        choices=["quotes", "notes"],
        help="Which section to export",
    )
    parser.add_argument(
        "--full",
        action="store_true",
        help="Force full re-export (wipe outputs, ignore manifest)",
    )
    args = parser.parse_args()
    run_export(args.section, full=args.full)


if __name__ == "__main__":
    main()
