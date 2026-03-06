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
