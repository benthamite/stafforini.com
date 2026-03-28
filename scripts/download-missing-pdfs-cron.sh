#!/usr/bin/env bash
# Scheduled wrapper for download-missing-pdfs.py
# Run by launchd daily; downloads PDFs and commits bib changes.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PYTHON="/Users/pablostafforini/.pyenv/versions/3.11.9/bin/python3"
BIB_DIR="$HOME/My Drive/bibliography"
LOG="$SCRIPT_DIR/download-missing-pdfs-cron.log"

{
  echo "===== $(date '+%Y-%m-%d %H:%M:%S') ====="

  # Run the download script
  "$PYTHON" "$SCRIPT_DIR/download-missing-pdfs.py" \
    --resume --retry-errors --delay 3 2>&1

  # Commit bib changes if any
  cd "$BIB_DIR"
  if ! git diff --quiet old.bib 2>/dev/null; then
    git add old.bib
    git commit -m "Add file fields for downloaded PDFs"
    echo "Committed bib changes."
  else
    echo "No bib changes to commit."
  fi

  echo "===== done ====="
  echo
} >> "$LOG" 2>&1
