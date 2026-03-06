#!/usr/bin/env bash
# Batch export :public:-tagged org subheadings to Hugo markdown via ox-hugo.
# Uses incremental export by default (only changed files).
# Pass --full to force a complete re-export.
# Usage: bash scripts/export-quotes.sh [--full]
source "$(dirname "$0")/common.sh"

# Pre-export: ensure the ID-slug map is fresh (the export itself needs it)
run_step "Generating ID→slug map" python3 "$SCRIPT_DIR/generate-id-slug-map.py"

python3 "$SCRIPT_DIR/incremental-export.py" quotes "$@"

bash "$SCRIPT_DIR/regenerate-data.sh" --quotes
