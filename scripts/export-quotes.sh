#!/usr/bin/env bash
# Batch export :public:-tagged org subheadings to Hugo markdown via ox-hugo.
# Load paths are set up within the elisp script itself.
# Usage: bash scripts/export-quotes.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
ELISP="$SCRIPT_DIR/export-quotes.el"

CONTENT_QUOTES="$REPO_ROOT/content/quotes"

# Remove stale content before exporting — content/quotes/ is entirely
# generated, so any file not recreated by the export is an orphan
# (renamed org file, deleted quote, migration artifact, etc.)
if [ -d "$CONTENT_QUOTES" ]; then
  echo "Cleaning stale files from content/quotes/..."
  find "$CONTENT_QUOTES" -name '*.md' ! -name '_index.md' -delete
fi

echo "Starting batch export..."
echo "Script: $ELISP"
echo ""

# Run Emacs in batch mode — load paths are configured in the elisp script
emacs --batch -l "$ELISP" 2>&1

echo ""
echo "Export script finished."
