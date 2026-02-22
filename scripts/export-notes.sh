#!/usr/bin/env bash
# Batch export blog org files to Hugo markdown via ox-hugo.
# Uses incremental export by default (only changed files).
# Pass --full to force a complete re-export.
# Usage: bash scripts/export-notes.sh [--full]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

python3 "$SCRIPT_DIR/incremental-export.py" notes "$@"

echo "--- Injecting lastmod dates ---"
python3 "$SCRIPT_DIR/inject-lastmod.py"

echo "--- Generating backlinks ---"
python3 "$SCRIPT_DIR/generate-backlinks.py"

echo "--- Generating citing-notes index ---"
python3 "$SCRIPT_DIR/generate-citing-notes.py"
