#!/usr/bin/env bash
# Build the Hugo site and deploy it to Netlify.
#
# content/ and static/ox-hugo/ are gitignored on main but present on
# disk after ox-hugo export.  This script builds Hugo (which reads
# them), then pushes the built site directly to Netlify via CLI.
source "$(dirname "$0")/common.sh"

cleanup() {
  local exit_code=$?
  if [ "$exit_code" -ne 0 ]; then
    echo "Deploy interrupted (exit $exit_code). Build state may be inconsistent." >&2
  fi
  exit "$exit_code"
}
trap cleanup EXIT

# Verify content exists
if [ ! -d content ]; then
  echo "Error: content/ not found. Export from org-mode first." >&2
  exit 1
fi

# Regenerate all pre-computed data
bash "$SCRIPT_DIR/regenerate-data.sh" --all

# Process PDFs (strip annotations, generate thumbnails)
run_step "Processing PDFs" python3 "$SCRIPT_DIR/process-pdfs.py"

# Clean stale build output (Hugo doesn't remove deleted/renamed pages)
echo "Cleaning previous build..."
# Use find -delete instead of trash to preserve the public/ symlink (nosync)
find public -mindepth 1 -delete 2>/dev/null || true

# Build
run_step "Building site" hugo --minify
run_step "Building search index" npx --yes pagefind --site public

# Deploy
run_step "Deploying to Netlify" npx --yes netlify deploy --prod --dir=public --no-build

echo "Done."
