#!/usr/bin/env bash
# Build the Pagefind search index for local development.
#
# Runs a Hugo build, indexes with Pagefind, then copies the index to
# static/ so the dev server serves it.  Safe to run while the dev
# server is running — Hugo server watches static/ and will pick up
# the new index automatically.
set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT"

if [ ! -d content ]; then
  echo "Warning: content/ not found — skipping search index build." >&2
  exit 0
fi

echo "Building site for search indexing..."
hugo --quiet

echo "Building search index..."
npx pagefind --site public

echo "Copying search index to static/..."
rm -rf static/pagefind
cp -R public/pagefind static/pagefind

echo "Search index ready."
