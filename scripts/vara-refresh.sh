#!/usr/bin/env bash
# Daily auto-evaluation of the VARA org file's dynamic blocks.
#
# Executes the daily data, performance, chart, delay, and calculator blocks in
# value-aligned-research-advisors.org. Pass --with-sensitivity after a new
# filing or a model change to also rerun the expensive contract-choice
# sensitivity sweep. Then re-exports the note to Hugo and commits + pushes any
# changes in both the notes and stafforini.com repos.
#
# Designed to be invoked by launchd. Exits non-zero on error so failures
# surface in the launchd log.

set -euo pipefail

INCLUDE_SENSITIVITY=0
if [[ "$#" -gt 1 ]]; then
  echo "Usage: $0 [--with-sensitivity]" >&2
  exit 2
fi
case "${1:-}" in
  "") ;;
  --with-sensitivity) INCLUDE_SENSITIVITY=1 ;;
  *)
    echo "Usage: $0 [--with-sensitivity]" >&2
    exit 2
    ;;
esac

# launchd runs with a minimal PATH; make sure Homebrew + pyenv are findable.
export PATH="/opt/homebrew/bin:$HOME/.pyenv/shims:$PATH"

ORG_FILE="$HOME/My Drive/notes/public/value-aligned-research-advisors.org"
NOTES_REPO="$HOME/My Drive/notes"
STAFFORINI_REPO="$HOME/repos/stafforini.com"
NOTES_ORG_PATH="public/value-aligned-research-advisors.org"
LOG_PATH="$HOME/.cache/vara-refresh.log"

notify_failure() {
  local exit_code="$1"
  /usr/bin/osascript -e \
    "display notification \"Exit ${exit_code}; see ${LOG_PATH}\" with title \"VARA refresh failed\"" \
    >/dev/null 2>&1 || true
}

on_exit() {
  local status="$?"
  if [[ "$status" -ne 0 ]]; then
    notify_failure "$status"
  fi
}

trap on_exit EXIT

# Fetch the MarketData token from pass. If gpg-agent doesn't have the
# passphrase cached, this will fail and the job exits cleanly — the next
# interactive pass usage will re-warm the cache.
if ! MARKETDATA_KEY="$(pass env/marketdata-token 2>/dev/null)"; then
  echo "Error: could not read env/marketdata-token from pass. Is gpg-agent warm?" >&2
  exit 1
fi
export MARKETDATA_KEY
export SEC_USER_AGENT="stafforini.com value-aligned-research-advisors research; Pablo Stafforini <pablo@stafforini.com>"

BLOCKS=(vara-data vara-perf vara-chart vara-chart-salp vara-delay vara-calc)
ELISP_INCLUDE_SENSITIVITY=nil
if [[ "$INCLUDE_SENSITIVITY" -eq 1 ]]; then
  BLOCKS+=(vara-sensitivity)
  ELISP_INCLUDE_SENSITIVITY=t
fi

echo "=== [$(date '+%Y-%m-%d %H:%M:%S')] vara refresh starting ==="

if [[ ! -f "$ORG_FILE" ]]; then
  echo "Error: org file not found: $ORG_FILE" >&2
  exit 1
fi

for block in "${BLOCKS[@]}"; do
  if ! grep -Fqx "#+name: $block" "$ORG_FILE"; then
    echo "Error: expected Babel block '$block' not found in $ORG_FILE" >&2
    exit 1
  fi
done

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
  --eval "(sa-lp-refresh \"$ORG_FILE\" $ELISP_INCLUDE_SENSITIVITY \"vara\")"

# The Emacs save-hook that maintains "#+lastmod:" doesn't fire in batch
# mode, so the keyword stays stale across programmatic refreshes. Update
# it here so inject-lastmod.py (which prefers the keyword over git mtime)
# sees a fresh timestamp.
echo "--- Updating #+lastmod: keyword ---"
LASTMOD_TS="$(date +"%Y-%m-%dT%H:%M:%S")"
sed -i '' "s|^#+lastmod: .*|#+lastmod: ${LASTMOD_TS}|" "$ORG_FILE"
if ! grep -Fqx "#+lastmod: ${LASTMOD_TS}" "$ORG_FILE"; then
  echo "Error: failed to verify refreshed #+lastmod keyword in $ORG_FILE" >&2
  exit 1
fi

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

NOTES_COMMIT_MESSAGE="vara: refresh returns/charts/delays/calculator (auto-eval)"
if [[ "$INCLUDE_SENSITIVITY" -eq 1 ]]; then
  NOTES_COMMIT_MESSAGE="vara: refresh returns/charts/sensitivity/delays/calculator (auto-eval)"
fi

commit_if_changed "$NOTES_REPO" \
  "$NOTES_COMMIT_MESSAGE" \
  "$NOTES_ORG_PATH" ".value-aligned-research-advisors-option-cache"

commit_if_changed "$STAFFORINI_REPO" \
  "vara: refresh returns/chart/calculator HTML" \
  "static/images/value-aligned-research-advisors-returns.html" \
  "static/images/value-aligned-research-advisors-returns-salp.html" \
  "static/images/value-aligned-research-advisors-calculator.html"

echo "--- Deploying to Netlify (fast note/static asset path) ---"
if [[ "${DRY_RUN:-0}" == "1" ]]; then
  echo "$STAFFORINI_REPO: [dry-run] would deploy with --fast-note."
else
  bash "$STAFFORINI_REPO/scripts/deploy.sh" --fast-note
fi

echo "=== [$(date '+%Y-%m-%d %H:%M:%S')] vara refresh done ==="
