#!/usr/bin/env bash
# Start the Hugo dev server for local development.
#
# - Kills any existing Hugo server processes to avoid stale cache
# - Regenerates citing-notes data from cite shortcodes
# - Starts Hugo server with live reload
#
# Search is not built by default (it's slow). Run `npm run reindex`
# separately if you need search.
set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT"

# Kill any existing Hugo server processes to ensure a fresh build
# (Hugo's incremental rebuild doesn't track cross-page shortcode
# dependencies, so a stale server can show outdated citations).
# -U scopes to current user; -f matches the full command line so we
# only kill Hugo servers started with --config (our dev servers).
existing=$(pgrep -U "$(id -u)" -f "hugo server.*--config" 2>/dev/null || true)
if [ -n "$existing" ]; then
  echo "Killing existing Hugo server(s)..."
  echo "$existing" | xargs kill 2>/dev/null || true
  sleep 1
fi

# Regenerate pre-computed data
if [ -d content/notes ]; then
  python3 scripts/generate-citing-notes.py
fi

# Symlink heavy static directories into public/ so Hugo can serve them
# without processing 30K+ files during startup.  hugo.dev.toml excludes
# pdfs, pdf-thumbnails, and pagefind from its static mounts; the symlinks
# make them available at their normal URLs anyway.
mkdir -p public
for dir in pdfs pdf-thumbnails; do
  [ -d "static/$dir" ] && [ ! -e "public/$dir" ] && \
    ln -s "$REPO_ROOT/static/$dir" "public/$dir"
done

# Start the dev server
# --renderStaticToDisk serves static files from public/ (including our
# symlinks) while keeping rendered pages in memory for speed.
# --disableFastRender forces full re-renders on every change; without it
# Hugo skips pages it thinks are unaffected, causing stale templates.
exec hugo server --config hugo.toml,hugo.dev.toml --renderStaticToDisk --navigateToChanged --disableFastRender
