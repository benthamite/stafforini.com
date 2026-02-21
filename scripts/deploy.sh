#!/usr/bin/env bash
# Build the Hugo site and deploy it to Netlify.
#
# content/ and static/ox-hugo/ are gitignored on main but present on
# disk after ox-hugo export.  This script builds Hugo (which reads
# them), then pushes the built site directly to Netlify via CLI.
set -euo pipefail

cleanup() {
  echo "Deploy interrupted. Build state may be inconsistent." >&2
}
trap cleanup EXIT

REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT"

# Verify content exists
if [ ! -d content ]; then
  echo "Error: content/ not found. Export from org-mode first." >&2
  exit 1
fi

# Inject lastmod dates from org file modification times
echo "Injecting lastmod dates..."
python3 scripts/inject-lastmod.py

# Process PDFs (strip annotations, generate thumbnails)
echo "Processing PDFs..."
python3 scripts/process-pdfs.py

# Generate citing-notes data (pre-computed work->note reverse index)
echo "Generating citing-notes data..."
python3 scripts/generate-citing-notes.py

# Clean stale build output (Hugo doesn't remove deleted/renamed pages)
echo "Cleaning previous build..."
trash public 2>/dev/null || true

# Build
echo "Building site..."
hugo --minify

echo "Building search index..."
npx pagefind --site public

# Deploy
echo "Deploying to Netlify..."
npx netlify deploy --prod --dir=public --no-build

trap - EXIT
echo "Done."
