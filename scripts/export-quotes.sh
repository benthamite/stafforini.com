#!/usr/bin/env bash
# Batch export :public:-tagged org subheadings to Hugo markdown via ox-hugo.
# Load paths are set up within the elisp script itself.
# Usage: bash scripts/export-quotes.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ELISP="$SCRIPT_DIR/export-quotes.el"

echo "Starting batch export..."
echo "Script: $ELISP"
echo ""

# Run Emacs in batch mode — load paths are configured in the elisp script
emacs --batch -l "$ELISP" 2>&1

echo ""
echo "Export script finished."
