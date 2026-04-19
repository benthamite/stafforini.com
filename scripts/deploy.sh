#!/usr/bin/env bash
# Export content, build the Hugo site, and deploy it to Netlify.
#
# content/ and static/ox-hugo/ are gitignored on main but present on
# disk after ox-hugo export.  This script builds Hugo (which reads
# them), then pushes the built site directly to Netlify via CLI.
#
# Usage: bash scripts/deploy.sh [--quick]
# --quick: skip content export, data regeneration, and PDF processing (just
#          clean, build, index, deploy).  Use when only templates or styles
#          changed after a recent full export.
#
# PDF uploads are auto-gated: the script tracks when PDFs were last pushed
# (via .last-pdf-deploy) and re-includes static/pdfs/ and static/pdf-thumbnails/
# in the Netlify upload only when it finds newer files there.  No flag needed.
source "$(dirname "$0")/common.sh"

quick=false

usage() {
  sed -n '2,15p' "$0" | sed 's/^# \{0,1\}//'
}

for arg in "$@"; do
  case "$arg" in
    --quick) quick=true ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Error: unknown flag '$arg'" >&2; exit 1 ;;
  esac
done

PDF_DEPLOY_MARKER="$REPO_ROOT/.last-pdf-deploy"
include_pdfs=false
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

# True when static/pdfs/ or static/pdf-thumbnails/ contain a non-hidden file
# newer than the last PDF-including deploy (or the marker doesn't exist yet).
# Dotfiles are excluded so process-pdfs.py's manifest rewrite doesn't trigger
# a false positive.
detect_pdf_changes() {
  if [ ! -f "$PDF_DEPLOY_MARKER" ]; then
    return 0
  fi
  local match
  match="$(find -H "$REPO_ROOT/static/pdfs" "$REPO_ROOT/static/pdf-thumbnails" \
                -newer "$PDF_DEPLOY_MARKER" -type f ! -name '.*' \
                -print -quit 2>/dev/null)"
  [ -n "$match" ]
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

  # Auto-gate: include PDF assets in the Netlify deploy only when process-pdfs
  # produced new/changed files since the last successful PDF-including deploy.
  if detect_pdf_changes; then
    include_pdfs=true
    if [ -f "$PDF_DEPLOY_MARKER" ]; then
      echo "PDF assets changed since last deploy; including them in this upload."
    else
      echo "No prior PDF-deploy marker; including PDF assets in this upload."
    fi
  else
    echo "No PDF changes since last deploy; skipping PDF assets."
  fi
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

# .netlifyignore normally excludes the 50 GB PDF trees from Netlify uploads.
# Disable it only when PDFs actually changed (see detect_pdf_changes above).
if $include_pdfs; then
  echo "Including PDF assets in Netlify deploy; this may scan/upload ~50 GB."
  include_pdf_assets_for_netlify
fi

# Deploy — resolve symlink so Netlify CLI reads the actual directory.
# --timeout 3600: the default 20-minute timeout is too short when many
# files have changed (the CLI uploads files one-by-one, which is slow
# from high-latency connections).
DEPLOY_DIR="$(cd public && pwd -P)"
run_step "Deploying to Netlify" npx --yes netlify deploy --prod --dir="$DEPLOY_DIR" --no-build --timeout 3600

# Record a successful PDF-including deploy so the next run can skip the upload
# if nothing under static/pdfs/ or static/pdf-thumbnails/ has changed.
if $include_pdfs; then
  touch "$PDF_DEPLOY_MARKER"
fi

echo "Done."
