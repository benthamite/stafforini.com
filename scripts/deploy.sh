#!/usr/bin/env bash
# Build the Hugo site and deploy it to Netlify.
#
# content/ and static/ox-hugo/ are gitignored on main but present on
# disk after ox-hugo export.  This script builds Hugo (which reads
# them), then pushes the built site directly to Netlify via CLI.
#
# Usage: bash scripts/deploy.sh [--quick]
# --quick: skip data regeneration and PDF processing (just clean, build,
#          index, deploy).  Use after a recent full export when only
#          templates or styles changed.
source "$(dirname "$0")/common.sh"

quick=false
for arg in "$@"; do
  case "$arg" in
    --quick) quick=true ;;
    *) echo "Error: unknown flag '$arg'" >&2; exit 1 ;;
  esac
done

cleanup() {
  local exit_code=$?
  if [ "$exit_code" -ne 0 ]; then
    echo "Deploy interrupted (exit $exit_code). Build state may be inconsistent." >&2
  fi
  exit "$exit_code"
}
trap cleanup EXIT

# Verify content exists
if [ ! -d content ]; then
  echo "Error: content/ not found. Export from org-mode first." >&2
  exit 1
fi

if ! $quick; then
  # Regenerate all pre-computed data
  bash "$SCRIPT_DIR/regenerate-data.sh" --all

  # Process PDFs (strip annotations, generate thumbnails)
  run_step "Processing PDFs" python3 "$SCRIPT_DIR/process-pdfs.py"
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

# Deploy — resolve symlink so Netlify CLI reads the actual directory.
# --timeout 3600: the default 20-minute timeout is too short when many
# files have changed (the CLI uploads files one-by-one, which is slow
# from high-latency connections).
DEPLOY_DIR="$(cd public && pwd -P)"
run_step "Deploying to Netlify" npx --yes netlify deploy --prod --dir="$DEPLOY_DIR" --no-build --timeout 3600

echo "Done."
