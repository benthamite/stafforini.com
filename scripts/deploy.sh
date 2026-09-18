#!/usr/bin/env bash
# Export content, build the Hugo site, upload PDFs to R2, and deploy to Netlify.
#
# PDFs and thumbnails are served from Cloudflare R2, not Netlify, so the
# Netlify deploy does not carry the PDF tree.  On each full deploy,
# scripts/upload-pdfs.sh incrementally syncs static/pdfs/ and
# static/pdf-thumbnails/ to the R2 bucket using `aws s3 sync`.
#
# Required before the first deploy:
#   - Create an R2 bucket and API token (see docs/pdf-hosting-policy.md)
#   - Copy scripts/r2.env.sh.example -> scripts/r2.env.sh and fill in
#   - Update hugo.deploy.toml params.pdfBaseURL / thumbBaseURL
#
# Usage: bash scripts/deploy.sh [--quick|--fast-note|--fast-quote=WORK-SLUG...]
# --quick: skip content export, PDF processing, and R2 upload, but still clean,
#          rebuild the whole site, regenerate Pagefind, and deploy the full
#          atomic tree.  Use when templates/styles/config changed since the
#          last full deploy.
# --fast-note: skip export, clean, and search indexing; render only the
#              note-oriented Hugo segment into the existing public/ snapshot,
#              copy lightweight static assets, then deploy. Use for
#              already-exported note body edits and generated note-local assets
#              such as /images/sa-lp-calculator.html.
# --fast-quote=WORK-SLUG: like --fast-note, but render the quote-oriented
#              segment (home, quotes feed and pages, tags, sitemap) plus the
#              named work page.  Repeat the flag for each work whose quotes
#              changed.  Use for already-exported diary quotes that were
#              added or edited; the search index is left stale.
source "$(dirname "$0")/common.sh"

quick=false
fast_note=false
fast_quote=false
fast_quote_works=()

usage() {
  sed -n '2,28p' "$0" | sed 's/^# \{0,1\}//'
}

for arg in "$@"; do
  case "$arg" in
    --quick) quick=true ;;
    --fast-note) quick=true; fast_note=true ;;
    --fast-quote=*) quick=true; fast_quote=true
                    fast_quote_works+=("${arg#--fast-quote=}") ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Error: unknown flag '$arg'" >&2; exit 1 ;;
  esac
done

if $fast_note && $fast_quote; then
  echo "Error: --fast-note and --fast-quote cannot be combined." >&2
  exit 1
fi

# Quick and fast deploys skip export, so generated content must exist.
if $quick && [ ! -d content ]; then
  echo "Error: content/ not found. Run a full deploy/export before this deploy mode." >&2
  exit 1
fi

# The work slugs are interpolated into a Hugo config file below, so accept
# only real work pages with plain slugs.
if $fast_quote; then
  for work in "${fast_quote_works[@]}"; do
    if [[ ! "$work" =~ ^[a-z0-9][a-z0-9-]*$ ]] || [ ! -f "content/works/$work.md" ]; then
      echo "Error: --fast-quote: no work page for '$work' in content/works/." >&2
      exit 1
    fi
  done
fi

fast_render=false
if $fast_note; then
  fast_render=true
  fast_flag=--fast-note
elif $fast_quote; then
  fast_render=true
  fast_flag=--fast-quote
fi

if $fast_render; then
  if [ ! -f public/index.html ]; then
    echo "Error: $fast_flag requires an existing public/ build. Run a full deploy first." >&2
    exit 1
  fi
  if [ ! -f static/pagefind/pagefind.js ]; then
    echo "Error: $fast_flag requires an existing search index. Run a full deploy first." >&2
    exit 1
  fi
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

acquire_public_tree_lock

run_step "Refreshing bibliography PDF links" python3 "$SCRIPT_DIR/generate-pdf-links.py"

# A fast render preserves (nearly all) work pages, so attachment changes
# require the complete quick build, even when the caller asked for a fast one.
if $fast_render && ! python3 "$SCRIPT_DIR/verify-site.py" --dir public --profile pdf-links; then
  echo "PDF attachments changed; rebuilding the full site to update work links."
  fast_render=false
fi

# A fast render only adds and overwrites pages, so a quote that was deleted or
# renamed since the last build would stay live under its old URL.
if $fast_render && $fast_quote \
    && ! python3 "$SCRIPT_DIR/verify-site.py" --dir public --profile quote-pages; then
  echo "Rendered quote pages have no source; rebuilding the full site to drop them."
  fast_render=false
fi

# Recreate the pagefind symlink if it was lost.
ensure_static_symlinks

if $fast_render && $fast_note; then
  echo "Preserving existing public/ snapshot for fast note deploy..."
  # Build with the deploy overlay (excludes heavy static dirs from mounts
  # and points PDF URLs at R2 via params.pdfBaseURL), but render only the
  # pages most likely to change after a minor note body edit.
  run_step "Building fast note segment" \
    hugo --minify --config hugo.toml,hugo.deploy.toml --renderSegments fast_note
  echo "Preserving existing search index for fast note deploy."
elif $fast_render && $fast_quote; then
  echo "Preserving existing public/ snapshot for fast quote deploy..."
  # Segments are static configuration, so the per-run work pages go in a
  # throwaway config layer defining a second segment rendered alongside
  # hugo.deploy.toml's fast_quote.
  segment_dir="$(mktemp -d "${TMPDIR:-/tmp}/fast-quote.XXXXXX")"
  work_paths="$(printf '/works/%s,' "${fast_quote_works[@]}")"
  cat > "$segment_dir/segment.toml" <<EOF
[segments.fast_quote_works]
  [[segments.fast_quote_works.includes]]
    path = "{${work_paths%,}}"
EOF
  run_step "Building fast quote segment" \
    hugo --minify --config "hugo.toml,hugo.deploy.toml,$segment_dir/segment.toml" \
      --renderSegments fast_quote,fast_quote_works
  rm -r "$segment_dir"
  echo "Preserving existing search index for fast quote deploy."
else
  # Clean stale Hugo output but preserve the pagefind symlink (it points to
  # static/pagefind, which is rebuilt below).
  echo "Cleaning previous build..."
  clean_hugo_output public
  ensure_static_symlinks

  # Build with the deploy overlay (excludes heavy static dirs from mounts
  # and points PDF URLs at R2 via params.pdfBaseURL).
  run_step "Building site" hugo --minify --config hugo.toml,hugo.deploy.toml
  echo "Cleaning previous search index..."
  clean_dir static/pagefind
  run_step "Building search index" npx --yes pagefind --site public
fi

if $fast_render && $fast_note; then
  run_step "Verifying fast note deploy" \
    python3 "$SCRIPT_DIR/verify-site.py" --dir public --profile fast-note
elif $fast_render && $fast_quote; then
  run_step "Verifying fast quote deploy" \
    python3 "$SCRIPT_DIR/verify-site.py" --dir public --profile fast-quote
else
  run_step "Verifying built site" python3 "$SCRIPT_DIR/verify-site.py" --dir public
fi

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
  trap 'restore_pagefind_symlink; release_public_tree_lock' EXIT
  rm "$PAGEFIND_LINK"
  cp -cR "$PAGEFIND_TARGET" "$PAGEFIND_LINK" 2>/dev/null \
    || cp -R "$PAGEFIND_TARGET" "$PAGEFIND_LINK"
fi

run_step "Deploying to Netlify" \
  python3 "$SCRIPT_DIR/netlify_deploy.py" --repo-root "$REPO_ROOT" -- \
    deploy --prod --dir="$DEPLOY_DIR" --no-build --timeout 3600

echo "Done."
