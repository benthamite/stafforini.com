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
# dependencies, so a stale server can show outdated citations)
existing=$(pgrep -f "hugo server" 2>/dev/null || true)
if [ -n "$existing" ]; then
  echo "Killing existing Hugo server(s)..."
  echo "$existing" | xargs kill 2>/dev/null || true
  sleep 1
fi

# Regenerate pre-computed data
if [ -d content/notes ]; then
  python3 scripts/generate-citing-notes.py
fi

# Start the dev server
# hugo.dev.toml excludes pdfs/ and pdf-thumbnails/ (30K+ files, 73 GB)
# from static mounts, reducing startup from ~2 minutes to seconds.
# PDF/thumbnail links will 404 during dev; run `npm run reindex` for a
# full build that includes them.
exec hugo server --config hugo.toml,hugo.dev.toml --renderToMemory --navigateToChanged
