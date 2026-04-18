#!/usr/bin/env bash
# Start the Hugo dev server for local note development.
#
# - Kills any existing Hugo server processes to avoid stale cache
# - Regenerates citing-notes data from cite shortcodes
# - Starts Hugo server with live reload and a narrowed dev content set
#
# Search is not built by default (it's slow). Run `npm run reindex`
# separately if you need search.
source "$(dirname "$0")/common.sh"

# Kill any existing Hugo server processes to ensure a fresh build
# (Hugo's incremental rebuild doesn't track cross-page shortcode
# dependencies, so a stale server can show outdated citations).
# find_hugo_servers scopes matches to this repo so unrelated Hugo projects are
# left alone.
existing=$(find_hugo_servers)
if [ -n "$existing" ]; then
  echo "Killing existing Hugo server(s)..."
  stop_hugo_servers "$existing"
fi

# Regenerate pre-computed data
if [ -d content/notes ]; then
  python3 "$SCRIPT_DIR/generate-citing-notes.py"
fi

# Symlink heavy static directories into public/ so Hugo can serve them
# without processing 30K+ files during startup.  hugo.dev.toml excludes
# pdfs, pdf-thumbnails, and pagefind from its static mounts; the symlinks
# make them available at their normal URLs anyway.
ensure_static_symlinks

# Start the dev server.
#
# --noBuildLock is appropriate here because this is a single-user local
# preview command that already kills existing Hugo servers before starting.
# Without it, a wedged previous server can hold .hugo_build.lock and make the
# replacement server appear to start while browser requests hang indefinitely.
#
# --renderStaticToDisk serves static files from public/ (including our
# symlinks) while keeping rendered pages in memory for speed.
# --disableFastRender forces full re-renders on every change; without it
# Hugo skips pages it thinks are unaffected, causing stale templates.
exec hugo server --config hugo.toml,hugo.dev.toml --renderStaticToDisk --navigateToChanged --disableFastRender --noBuildLock
