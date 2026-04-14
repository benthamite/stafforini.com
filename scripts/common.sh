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

# Create symlinks for heavy static directories (pdfs, pdf-thumbnails)
# in public/ so the dev server can serve them without copying.
ensure_static_symlinks() {
  mkdir -p public
  for dir in pdfs pdf-thumbnails; do
    if [ -d "static/$dir" ] && [ ! -e "public/$dir" ]; then
      ln -s "$REPO_ROOT/static/$dir" "public/$dir"
    fi
  done
}
