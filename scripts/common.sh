#!/usr/bin/env bash
# Common shell preamble for stafforini.com scripts.
set -euo pipefail

# Resolve paths — works whether sourced from scripts/ or repo root.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

# Run a labeled step, aborting on failure.
run_step() {
  local label="$1"; shift
  echo "$label..."
  if ! "$@"; then
    echo "Error: $label failed." >&2
    exit 1
  fi
}

# Return PIDs of running Hugo dev servers owned by the current user,
# or empty string if none.
find_hugo_servers() {
  pgrep -U "$(id -u)" -f "hugo server.*--config" 2>/dev/null || true
}

# Delete contents of a directory, following symlinks at the starting path.
# macOS find does NOT follow starting-path symlinks by default (unlike GNU
# find), so -H is required.  Without it, `find public -delete` is a no-op
# when public/ is a symlink — the root cause of ghost pages persisting
# across builds.
clean_dir() {
  local dir="$1"
  if [ -d "$dir" ]; then
    find -H "$dir" -mindepth 1 -delete
  fi
}

# Delete Hugo-generated content from public/, preserving heavy static
# assets (pdfs, pdf-thumbnails, pagefind) that persist across builds.
# This is MUCH faster than clean_dir for the deploy workflow because
# Hugo doesn't need to re-copy ~50 GB of PDFs and thumbnails.
clean_hugo_output() {
  local dir="$1"
  if [ ! -d "$dir" ]; then
    return
  fi
  # Delete everything except the directories we want to keep.
  # Uses find -delete (like clean_dir) to avoid rm -rf.
  for entry in "$dir"/*; do
    case "$(basename "$entry")" in
      pdfs|pdf-thumbnails|pagefind) continue ;;
      *) find -H "$entry" -depth -delete 2>/dev/null ;;
    esac
  done
}

# Create symlinks for heavy static directories (pdfs, pdf-thumbnails,
# pagefind) in public/ so Hugo doesn't need to copy them.
ensure_static_symlinks() {
  mkdir -p public
  for dir in pdfs pdf-thumbnails pagefind; do
    if [ -d "static/$dir" ] && [ ! -e "public/$dir" ]; then
      ln -s "$REPO_ROOT/static/$dir" "public/$dir"
    fi
  done
}
