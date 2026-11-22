#!/usr/bin/env bash
# Daily launchd entry point (com.stafforini.model-refresh) for the SALP and
# VARA notes: runs both refreshes without deploying, then deploys once.
#
# Each Netlify production deploy costs 15 credits. Until 2026-11-22 the two
# refreshes ran as separate jobs and deployed separately, ~450 credits a month.
# SALP runs first because VARA embeds SALP's model. VARA still runs when SALP
# fails, as it did when the jobs were separate. The deploy runs when at least
# one refresh ran to the end. The heartbeat is touched only when both ran to
# the end and the deploy completed; an Emacs-lock skip exits 0 without it.
# Run either refresh script directly (it deploys by itself) for a manual
# refresh, e.g. after a filing with --with-sensitivity.

set -uo pipefail

export PATH="/opt/homebrew/bin:$HOME/.pyenv/shims:$PATH"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
STAFFORINI_REPO="${STAFFORINI_REPO:-$(dirname "$SCRIPT_DIR")}"
LOG_PATH="$HOME/.cache/model-refresh.log"
SUCCESS_DIR="${XDG_STATE_HOME:-$HOME/.local/state}/launchd"
HEARTBEAT="$SUCCESS_DIR/com.stafforini.model-refresh.success"
NAMES=(sa-lp-refresh vara-refresh)

MARKERS="$(mktemp -d)"
cleanup() {
  local name
  for name in "${NAMES[@]}"; do rm -f "$MARKERS/$name"; done
  rmdir "$MARKERS" 2>/dev/null || true
}
trap cleanup EXIT

echo "=== [$(date '+%Y-%m-%d %H:%M:%S')] model refresh starting ==="

failed=0
completed=0
for name in "${NAMES[@]}"; do
  echo "--- [$(date '+%Y-%m-%d %H:%M:%S')] $name ---"
  if REFRESH_COMPLETED_FILE="$MARKERS/$name" \
       bash "$STAFFORINI_REPO/scripts/$name.sh" --no-deploy; then
    :
  else
    echo "$name failed (exit $?)." >&2
    failed=1
  fi
  if [[ -e "$MARKERS/$name" ]]; then
    completed=$((completed + 1))
  fi
done

deployed=0
if [[ "$completed" -eq 0 ]]; then
  echo "--- No refresh ran to the end; not deploying ---"
elif [[ "${DRY_RUN:-0}" == "1" ]]; then
  echo "--- [dry-run] would deploy with --fast-note ---"
else
  echo "--- Deploying to Netlify (fast note/static asset path) ---"
  if bash "$STAFFORINI_REPO/scripts/deploy.sh" --fast-note; then
    deployed=1
  else
    echo "Deploy failed (exit $?)." >&2
    failed=1
  fi
fi

# Only a completed deployment of both refreshes establishes freshness.
if [[ "$completed" -eq ${#NAMES[@]} && "$deployed" -eq 1 ]]; then
  mkdir -p "$SUCCESS_DIR"
  touch "$HEARTBEAT"
fi

if [[ "$failed" -ne 0 ]]; then
  osascript -e \
    "display notification \"See ${LOG_PATH}\" with title \"SALP/VARA refresh failed\"" \
    >/dev/null 2>&1 || true
  echo "=== [$(date '+%Y-%m-%d %H:%M:%S')] model refresh failed ==="
  exit 1
fi
echo "=== [$(date '+%Y-%m-%d %H:%M:%S')] model refresh done ==="
