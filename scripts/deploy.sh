#!/usr/bin/env bash
# Export content, build the Hugo site, upload PDFs to R2, and deploy to Netlify.
#
# PDFs and thumbnails are served from Cloudflare R2, not Netlify, so the
# Netlify deploy only carries HTML/CSS/JS (small, fast).  On each full
# deploy, scripts/upload-pdfs.sh incrementally syncs static/pdfs/ and
# static/pdf-thumbnails/ to the R2 bucket using `aws s3 sync`.
#
# Required before the first deploy:
#   - Create an R2 bucket and API token (see docs/pdf-hosting-policy.md)
#   - Copy scripts/r2.env.sh.example -> scripts/r2.env.sh and fill in
#   - Update hugo.deploy.toml params.pdfBaseURL / thumbBaseURL
#
# Usage: bash scripts/deploy.sh [--quick]
# --quick: skip content export, PDF processing, and R2 upload (just clean,
#          build, index, deploy).  Use when only templates/styles changed
#          since the last full deploy.
source "$(dirname "$0")/common.sh"

quick=false

usage() {
  sed -n '2,17p' "$0" | sed 's/^# \{0,1\}//'
}

for arg in "$@"; do
  case "$arg" in
    --quick) quick=true ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Error: unknown flag '$arg'" >&2; exit 1 ;;
  esac
done

# Quick deploy skips export, so it needs generated content to already exist.
if $quick && [ ! -d content ]; then
  echo "Error: content/ not found. Run a full deploy/export before --quick." >&2
  exit 1
fi

if ! $quick; then
  # Export source content.  Each export script regenerates its derived data
  # in the right order and fails the deploy if export or post-processing fails.
  run_step "Exporting notes" bash "$SCRIPT_DIR/export-notes.sh"
  run_step "Exporting quotes" bash "$SCRIPT_DIR/export-quotes.sh"

  # Process PDFs (strip annotations, generate thumbnails) into static/.
  run_step "Processing PDFs" python3 "$SCRIPT_DIR/process-pdfs.py"

  # Sync any new/changed PDFs to R2.  Source r2.env.sh if it exists and the
  # required env vars aren't already set.
  if [ -z "${R2_BUCKET:-}" ] && [ -f "$SCRIPT_DIR/r2.env.sh" ]; then
    # shellcheck disable=SC1091
    source "$SCRIPT_DIR/r2.env.sh"
  fi
  run_step "Uploading PDFs to R2" bash "$SCRIPT_DIR/upload-pdfs.sh"
fi

if [ ! -d content ]; then
  echo "Error: content/ not found after export." >&2
  exit 1
fi

# Clean stale Hugo output but preserve the pagefind index (it lives in
# public/pagefind via a symlink created by ensure_static_symlinks).
echo "Cleaning previous build..."
clean_hugo_output public

# Recreate the pagefind symlink if it was lost.
ensure_static_symlinks

# Build with the deploy overlay (excludes heavy static dirs from mounts
# and points PDF URLs at R2 via params.pdfBaseURL).
run_step "Building site" hugo --minify --config hugo.toml,hugo.deploy.toml
run_step "Building search index" npx --yes pagefind --site public
run_step "Verifying built site" python3 "$SCRIPT_DIR/verify-site.py" --dir public

# Deploy -- resolve the public/ symlink so Netlify CLI reads the real
# directory (CLI does not follow symlinks), and extend the default 20-min
# timeout since per-file upload latency adds up.
DEPLOY_DIR="$(cd public && pwd -P)"

# public/pagefind is also a symlink (to static/pagefind, so the index
# persists across builds), and Netlify CLI does not follow it either --
# leaving /pagefind/* 404 in production and breaking site search.  Replace
# the symlink with a real copy of the index for the deploy, then restore
# it on exit so the next build's index lands in static/pagefind again.
PAGEFIND_LINK="$DEPLOY_DIR/pagefind"
PAGEFIND_TARGET=""
restore_pagefind_symlink() {
  if [ -n "$PAGEFIND_TARGET" ]; then
    rm -rf "$PAGEFIND_LINK"
    ln -s "$PAGEFIND_TARGET" "$PAGEFIND_LINK"
  fi
}
if [ -L "$PAGEFIND_LINK" ]; then
  PAGEFIND_TARGET="$(readlink "$PAGEFIND_LINK")"
  trap restore_pagefind_symlink EXIT
  rm "$PAGEFIND_LINK"
  cp -R "$PAGEFIND_TARGET" "$PAGEFIND_LINK"
fi

run_step "Deploying to Netlify" npx --yes netlify deploy --prod --dir="$DEPLOY_DIR" --no-build --timeout 3600

echo "Done."
