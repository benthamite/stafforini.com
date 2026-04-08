#!/usr/bin/env bash
# Regenerate pre-computed data files (backlinks, citing-notes, etc.).
# Called by export and deploy scripts.
#
# Usage: bash scripts/regenerate-data.sh [--notes] [--quotes] [--all]
# --notes: regenerate data for notes (backlinks, citing-notes, categories, lastmod)
# --quotes: regenerate data for quotes (quote-topics, work-pages, topic-pages)
# --all: regenerate everything (default if no flags given, used by deploy)
source "$(dirname "${BASH_SOURCE[0]}")/common.sh"

do_notes=false
do_quotes=false

for arg in "$@"; do
  case "$arg" in
    --all|--notes|--quotes) ;;
    *) echo "Error: unknown flag '$arg'" >&2; exit 1 ;;
  esac
done

if [ $# -eq 0 ] || [[ " $* " == *" --all "* ]]; then
  do_notes=true
  do_quotes=true
else
  [[ " $* " == *" --notes "* ]] && do_notes=true
  [[ " $* " == *" --quotes "* ]] && do_quotes=true
fi

# Shared: always run ID-slug map and tag injection first (needed by both pipelines)
run_step "Generating ID→slug map" python3 "$SCRIPT_DIR/generate-id-slug-map.py"
run_step "Injecting tags" python3 "$SCRIPT_DIR/inject-tags.py"

if $do_notes; then
  run_step "Injecting lastmod dates" python3 "$SCRIPT_DIR/inject-lastmod.py"
  run_step "Generating backlinks" python3 "$SCRIPT_DIR/generate-backlinks.py"
  run_step "Generating citing-notes index" python3 "$SCRIPT_DIR/generate-citing-notes.py"
fi

if $do_quotes; then
  run_step "Extracting non-diary quotes" python3 "$SCRIPT_DIR/extract-non-diary-quotes.py"
  run_step "Generating work pages" python3 "$SCRIPT_DIR/generate-work-pages.py"
fi
