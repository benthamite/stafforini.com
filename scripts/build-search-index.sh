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
trash public 2>/dev/null || true

run_step "Building site for search indexing" hugo --quiet
run_step "Building search index" npx --yes pagefind --site public

# Atomic swap: copy to .new, remove old, rename — so the dev server
# never sees a partially-written index directory
echo "Copying search index to static/..."
cp -R public/pagefind static/pagefind.new
trash static/pagefind 2>/dev/null || true
mv static/pagefind.new static/pagefind

# Restore dev-server symlinks if the server is running (trash public removed them)
if pgrep -U "$(id -u)" -f "hugo server.*--config" >/dev/null 2>&1; then
  for dir in pdfs pdf-thumbnails; do
    [ -d "static/$dir" ] && [ ! -e "public/$dir" ] && \
      ln -s "$REPO_ROOT/static/$dir" "public/$dir"
  done
fi

echo "Search index ready."
