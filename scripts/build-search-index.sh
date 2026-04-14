#!/usr/bin/env bash
# Build the Pagefind search index for local development.
#
# Runs a Hugo build, indexes with Pagefind, then copies the index to
# static/ so the dev server serves it.  Safe to run while the dev
# server is running — Hugo server watches static/ and will pick up
# the new index automatically.
source "$(dirname "$0")/common.sh"

if [ ! -d content ]; then
  echo "Warning: content/ not found — skipping search index build." >&2
  exit 0
fi

run_step "Generating citing-notes data" python3 "$SCRIPT_DIR/generate-citing-notes.py"

# Clean stale build output (Hugo doesn't remove deleted/renamed pages)
clean_dir public

run_step "Building site for search indexing" hugo --quiet
run_step "Building search index" npx --yes pagefind --site public

# Replace index contents in-place to preserve the static/pagefind symlink (nosync)
echo "Copying search index to static/..."
clean_dir static/pagefind
cp -R public/pagefind/. static/pagefind/

# Restore dev-server symlinks if the server is running (cleaning public/ removed them)
if [ -n "$(find_hugo_servers)" ]; then
  ensure_static_symlinks
fi

echo "Search index ready."
