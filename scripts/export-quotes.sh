#!/usr/bin/env bash
# Batch export :public:-tagged org subheadings to Hugo markdown via ox-hugo.
# Uses incremental export by default (only changed files).
# Pass --full to force a complete re-export.
# Usage: bash scripts/export-quotes.sh [--full]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "--- Generating ID→slug map ---"
python3 "$SCRIPT_DIR/generate-id-slug-map.py"

python3 "$SCRIPT_DIR/incremental-export.py" quotes "$@"

echo "--- Extracting non-diary quotes ---"
python3 "$SCRIPT_DIR/extract-non-diary-quotes.py"

echo "--- Generating work pages ---"
python3 "$SCRIPT_DIR/generate-work-pages.py"

echo "--- Generating topic pages ---"
python3 "$SCRIPT_DIR/generate-topic-pages.py"
