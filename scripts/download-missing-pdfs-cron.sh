#!/usr/bin/env bash
# Scheduled wrapper for reviewed, unattended PDF acquisition.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PYTHON="$HOME/.local/share/paper-fetch/venv/bin/python"
LOG="$SCRIPT_DIR/download-missing-pdfs-cron.log"

{
  echo "===== $(date '+%Y-%m-%d %H:%M:%S') ====="

  if [[ ! -x "$PYTHON" ]]; then
    echo "Dedicated paper-fetch runtime missing: $PYTHON" >&2
    exit 1
  fi
  exec "$PYTHON" -B "$SCRIPT_DIR/download-missing-pdfs-batch.py" "$@"
} >> "$LOG" 2>&1
