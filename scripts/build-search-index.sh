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

acquire_public_tree_lock

run_step "Generating citing-notes data" python3 "$SCRIPT_DIR/generate-citing-notes.py"

# Clean stale build output (Hugo doesn't remove deleted/renamed pages)
clean_hugo_output public
ensure_static_symlinks

run_step "Building site for search indexing" hugo --quiet --config hugo.toml,hugo.deploy.toml

tmp_index="static/pagefind.tmp.$$"
rm -rf "$tmp_index"
trap 'rm -rf "$tmp_index"; release_public_tree_lock' EXIT
run_step "Building search index" npx --yes pagefind --site public --output-path "$tmp_index"

# Replace index contents in-place to preserve the public/pagefind symlink
# (nosync) while avoiding public/pagefind as Pagefind's output directory.
echo "Copying search index to static/..."
clean_dir static/pagefind
mkdir -p static/pagefind
cp -R "$tmp_index"/. static/pagefind/
rm -rf "$tmp_index"

# Restore the dev-server symlink even when no server is running.  The plain
# Hugo build above copies static/pagefind into public/pagefind as a real
# directory; leaving that behind makes the next server startup rotate it as
# stale output.
ensure_static_symlinks

echo "Search index ready."
