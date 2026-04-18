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

# Return PIDs of running Hugo dev servers for this repo owned by the current
# user, or empty string if none.  Prefer pgrep for command-line matching, but
# fall back to lsof because macOS privacy/sandbox contexts can make pgrep fail.
find_hugo_servers() {
  local candidates=""

  if command -v pgrep >/dev/null 2>&1; then
    candidates="$(pgrep -U "$(id -u)" -f "hugo server.*--config" 2>/dev/null || true)"
  fi

  if command -v lsof >/dev/null 2>&1; then
    candidates="$candidates
$(lsof -nP -t -a -c hugo -iTCP -sTCP:LISTEN 2>/dev/null || true)"
  fi

  if [ -z "$candidates" ]; then
    return 0
  fi

  printf '%s\n' "$candidates" \
    | awk 'NF && !seen[$0]++' \
    | while read -r pid; do
        if command -v lsof >/dev/null 2>&1; then
          local cwd
          cwd="$(lsof -a -p "$pid" -d cwd -Fn 2>/dev/null \
            | sed -n 's/^n//p' \
            | head -n 1 \
            || true)"
          [ "$cwd" = "$REPO_ROOT" ] || continue
        fi
        printf '%s\n' "$pid"
      done
}

# Stop running Hugo dev servers for this repo.  Accepts an optional newline-
# separated PID list from find_hugo_servers to avoid racing a second lookup.
stop_hugo_servers() {
  local pids="${1:-}"
  if [ -z "$pids" ]; then
    pids="$(find_hugo_servers)"
  fi
  if [ -z "$pids" ]; then
    return 1
  fi

  printf '%s\n' "$pids" | while read -r pid; do
    [ -n "$pid" ] && kill "$pid" 2>/dev/null || true
  done

  sleep 1

  local remaining=""
  while read -r pid; do
    if [ -n "$pid" ] && kill -0 "$pid" 2>/dev/null; then
      remaining="$remaining
$pid"
    fi
  done <<< "$pids"

  if [ -n "$remaining" ]; then
    printf '%s\n' "$remaining" | while read -r pid; do
      [ -n "$pid" ] && kill -KILL "$pid" 2>/dev/null || true
    done
  fi
}

# Generated content changes can leave a running Hugo server serving stale
# in-memory pages.  Stop it explicitly so local previews cannot lie.
stop_hugo_servers_after_content_change() {
  local reason="$1"
  local pids
  pids="$(find_hugo_servers)"
  if [ -z "$pids" ]; then
    return 0
  fi

  echo "Stopping Hugo dev server because $reason changed generated content..."
  stop_hugo_servers "$pids"
  echo "Restart preview with stafforini-start-server (s), or run: bash scripts/start-dev-server.sh"
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
  # Do not pass -H here: each globbed entry is already inside public/'s
  # resolved target when public/ is a symlink, and following arbitrary child
  # symlinks would risk deleting the linked target rather than the link.
  local old_nullglob
  old_nullglob="$(shopt -p nullglob || true)"
  shopt -s nullglob
  for entry in "$dir"/*; do
    case "$(basename "$entry")" in
      pdfs|pdf-thumbnails|pagefind) continue ;;
      *) find "$entry" -depth -delete 2>/dev/null ;;
    esac
  done
  eval "$old_nullglob"
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
