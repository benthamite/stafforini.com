#!/usr/bin/env bash
# Export content, build the Hugo site, and deploy it to Netlify.
#
# content/ and static/ox-hugo/ are gitignored on main but present on
# disk after ox-hugo export.  This script builds Hugo (which reads
# them), then pushes the built site directly to Netlify via CLI.
#
# Usage: bash scripts/deploy.sh [--quick] [--include-pdfs]
# --quick: skip content export, data regeneration, and PDF processing (just
#          clean, build, index, deploy).  Use when only templates or styles
#          changed after a recent full export.
# --include-pdfs: include pdfs/ and pdf-thumbnails/ in the Netlify deploy.
#                 This temporarily disables .netlifyignore and can be slow.
source "$(dirname "$0")/common.sh"

quick=false
include_pdfs=false

usage() {
  sed -n '2,13p' "$0" | sed 's/^# \{0,1\}//'
}

for arg in "$@"; do
  case "$arg" in
    --quick) quick=true ;;
    --include-pdfs) include_pdfs=true ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Error: unknown flag '$arg'" >&2; exit 1 ;;
  esac
done

netlifyignore_disabled=""

restore_netlifyignore() {
  if [ -n "$netlifyignore_disabled" ] && [ -e "$netlifyignore_disabled" ]; then
    mv "$netlifyignore_disabled" "$REPO_ROOT/.netlifyignore"
  fi
}

cleanup() {
  local exit_code=$?
  restore_netlifyignore
  if [ "$exit_code" -ne 0 ]; then
    echo "Deploy interrupted (exit $exit_code). Build state may be inconsistent." >&2
  fi
  exit "$exit_code"
}
trap cleanup EXIT

include_pdf_assets_for_netlify() {
  local ignore_file="$REPO_ROOT/.netlifyignore"

  if [ ! -f "$ignore_file" ]; then
    return
  fi

  netlifyignore_disabled="$(mktemp "$REPO_ROOT/.netlifyignore.disabled.XXXXXX")"
  mv "$ignore_file" "$netlifyignore_disabled"
}

# Quick deploy skips export, so it needs generated content to already exist.
if $quick && [ ! -d content ]; then
  echo "Error: content/ not found. Run a full deploy/export before --quick." >&2
  exit 1
fi

if ! $quick; then
  # Export source content first.  The export scripts regenerate the derived
  # data they depend on, in the correct order, and fail before deployment if
  # org export or post-processing fails.
  run_step "Exporting notes" bash "$SCRIPT_DIR/export-notes.sh"
  run_step "Exporting quotes" bash "$SCRIPT_DIR/export-quotes.sh"

  # Process PDFs (strip annotations, generate thumbnails)
  run_step "Processing PDFs" python3 "$SCRIPT_DIR/process-pdfs.py"
fi

if [ ! -d content ]; then
  echo "Error: content/ not found after export." >&2
  exit 1
fi

# Clean stale Hugo output (notes, quotes, works, tags, etc.) but
# preserve heavy static assets (pdfs, pdf-thumbnails, pagefind) that
# don't change often and are expensive to re-copy.
echo "Cleaning previous build..."
clean_hugo_output public

# Ensure symlinks for heavy static directories.  Hugo's deploy config
# excludes these from static mounts, so they persist in public/ via
# symlinks rather than being copied on every build.
ensure_static_symlinks

# Build with deploy overlay (excludes heavy static dirs from mounts)
run_step "Building site" hugo --minify --config hugo.toml,hugo.deploy.toml
run_step "Building search index" npx --yes pagefind --site public
run_step "Verifying built site" python3 "$SCRIPT_DIR/verify-site.py" --dir public

# .netlifyignore normally excludes the 50 GB PDF trees.  Full deploys still
# process those assets locally, but publishing them is an explicit opt-in
# because Netlify CLI deploys are otherwise too slow and fragile.
if $include_pdfs; then
  echo "Including PDF assets in Netlify deploy; this may scan/upload ~50 GB."
  include_pdf_assets_for_netlify
else
  echo "Skipping PDF assets in Netlify deploy. Use --include-pdfs to publish PDF changes."
fi

# Deploy — resolve symlink so Netlify CLI reads the actual directory.
# --timeout 3600: the default 20-minute timeout is too short when many
# files have changed (the CLI uploads files one-by-one, which is slow
# from high-latency connections).
DEPLOY_DIR="$(cd public && pwd -P)"
run_step "Deploying to Netlify" npx --yes netlify deploy --prod --dir="$DEPLOY_DIR" --no-build --timeout 3600

echo "Done."
