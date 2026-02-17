#!/usr/bin/env bash
# Build the Hugo site and deploy it to Netlify.
#
# content/ and static/ox-hugo/ are gitignored on main but present on
# disk after ox-hugo export.  This script builds Hugo (which reads
# them), then pushes the built site directly to Netlify via CLI.
set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT"

# Verify content exists
if [ ! -d content ]; then
  echo "Error: content/ not found. Export from org-mode first." >&2
  exit 1
fi

# Build
echo "Building site..."
hugo --minify

echo "Building search index..."
npx pagefind --site public

# Deploy
echo "Deploying to Netlify..."
npx netlify deploy --prod --dir=public

echo "Done."
