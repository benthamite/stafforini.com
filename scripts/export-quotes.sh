#!/usr/bin/env bash
# Batch export :public:-tagged org subheadings to Hugo markdown via ox-hugo.
# Always does a full source scan/export.  The optional --full flag is accepted
# for compatibility with older Emacs commands and shell habits.
# Usage: bash scripts/export-quotes.sh [--full]
source "$(dirname "$0")/common.sh"

# Pre-export: ensure the ID-slug map is fresh (the export itself needs it)
run_step "Generating ID→slug map" python3 "$SCRIPT_DIR/generate-id-slug-map.py"

python3 "$SCRIPT_DIR/export-org.py" quotes "$@"

bash "$SCRIPT_DIR/regenerate-data.sh" --quotes

run_step "Verifying dev site" python3 "$SCRIPT_DIR/verify-site.py" --build dev

stop_hugo_servers_after_content_change "quotes export"
