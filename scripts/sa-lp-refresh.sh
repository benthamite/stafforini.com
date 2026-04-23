#!/usr/bin/env bash
# Daily auto-evaluation of the SA-LP org file's dynamic blocks.
#
# Executes sa-data, sa-perf, sa-chart, sa-sensitivity, sa-delay, and sa-calc
# in situational-awareness-lp.org, regenerating the returns table, returns
# chart, sensitivity table, copycat-delays table, and portfolio calculator
# HTML. Then re-exports the note to Hugo and commits + pushes any changes
# in both the notes and stafforini.com repos.
#
# Designed to be invoked by launchd. Exits non-zero on error so failures
# surface in the launchd log.

set -euo pipefail

# launchd runs with a minimal PATH; make sure Homebrew + pyenv are findable.
export PATH="/opt/homebrew/bin:$HOME/.pyenv/shims:$PATH"

ORG_FILE="$HOME/My Drive/notes/situational-awareness-lp.org"
NOTES_REPO="$HOME/My Drive/notes"
STAFFORINI_REPO="$HOME/My Drive/repos/stafforini.com"

# Fetch the MarketData token from pass. If gpg-agent doesn't have the
# passphrase cached, this will fail and the job exits cleanly — the next
# interactive pass usage will re-warm the cache.
if ! MARKETDATA_KEY="$(pass env/marketdata-token 2>/dev/null)"; then
  echo "Error: could not read env/marketdata-token from pass. Is gpg-agent warm?" >&2
  exit 1
fi
export MARKETDATA_KEY
export SEC_USER_AGENT="stafforini.com situational-awareness-lp research; Pablo Stafforini <pablo@stafforini.com>"

BLOCKS=(sa-data sa-perf sa-chart sa-sensitivity sa-delay sa-calc)

echo "=== [$(date '+%Y-%m-%d %H:%M:%S')] sa-lp refresh starting ==="

# Bail out if the user is actively editing the file in an Emacs session —
# Emacs writes a .#<filename> lockfile for buffers with unsaved changes.
LOCKFILE="$(dirname "$ORG_FILE")/.#$(basename "$ORG_FILE")"
if [[ -e "$LOCKFILE" || -L "$LOCKFILE" ]]; then
  echo "Skipping: $ORG_FILE is locked by an Emacs session ($LOCKFILE)."
  exit 0
fi

echo "--- Evaluating babel blocks in $(basename "$ORG_FILE") ---"
# Run in an isolated batch Emacs with a minimal init so the user's active
# session stays fully responsive. -Q skips site + user init entirely; we
# explicitly load sa-lp-init.el which pulls in just org + ob-python and
# defines sa-lp-refresh (with per-block retry + backoff).
INIT_FILE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/sa-lp-init.el"

emacs -Q --batch --load "$INIT_FILE" \
  --eval "(sa-lp-refresh \"$ORG_FILE\")"

echo "--- Re-exporting notes to Hugo ---"
bash "$STAFFORINI_REPO/scripts/export-notes.sh"

commit_if_changed() {
  local repo="$1"; shift
  local message="$1"; shift
  local paths=("$@")
  cd "$repo"
  if [[ -z "$(git status --porcelain -- "${paths[@]}" 2>/dev/null)" ]]; then
    echo "$repo: no changes to commit."
    return 0
  fi
  if [[ "${DRY_RUN:-0}" == "1" ]]; then
    echo "$repo: [dry-run] would commit and push:"
    git status --porcelain -- "${paths[@]}"
    return 0
  fi
  git add -- "${paths[@]}"
  git commit -m "$message"
  git push
  echo "$repo: committed and pushed."
}

commit_if_changed "$NOTES_REPO" \
  "sa-lp: refresh returns/chart/sensitivity/delays/calculator (auto-eval)" \
  "situational-awareness-lp.org" ".sa-lp-option-cache"

commit_if_changed "$STAFFORINI_REPO" \
  "sa-lp: refresh returns/chart/calculator HTML" \
  "static/images/sa-lp-returns.html" \
  "static/images/sa-lp-calculator.html"

echo "=== [$(date '+%Y-%m-%d %H:%M:%S')] sa-lp refresh done ==="
