#!/usr/bin/env bash
# Build the Hugo site and deploy it to Netlify.
#
# content/ and static/ox-hugo/ are gitignored on main but present on
# disk after ox-hugo export.  This script builds Hugo (which reads
# them), then pushes the built site directly to Netlify via CLI.
set -euo pipefail

cleanup() {
  local exit_code=$?
  if [ "$exit_code" -ne 0 ]; then
    echo "Deploy interrupted (exit $exit_code). Build state may be inconsistent." >&2
  fi
}
trap cleanup EXIT

REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT"

# Verify content exists
if [ ! -d content ]; then
  echo "Error: content/ not found. Export from org-mode first." >&2
  exit 1
fi

# Several of these steps overlap with export-notes.sh / export-quotes.sh.
# Running them here ensures the data is fresh even if the user exports
# content and deploys in separate sessions.

# Helper: run a script and abort deploy on failure
run_step() {
  echo "$1..."
  if ! $2; then
    echo "Error: $1 failed. Aborting deploy." >&2
    exit 1
  fi
}

# Generate org-id → slug mapping (used by export-quotes.el for topic links;
# also run here so the map is always fresh before topic page generation)
run_step "Generating ID→slug map" "python3 scripts/generate-id-slug-map.py"

# Generate topic pages from org-roam tag stubs
run_step "Generating topic pages" "python3 scripts/generate-topic-pages.py"

# Inject lastmod dates from org file modification times
run_step "Injecting lastmod dates" "python3 scripts/inject-lastmod.py"

# Process PDFs (strip annotations, generate thumbnails)
run_step "Processing PDFs" "python3 scripts/process-pdfs.py"

# Generate citing-notes data (pre-computed work->note reverse index)
run_step "Generating citing-notes data" "python3 scripts/generate-citing-notes.py"

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

echo "Done."
