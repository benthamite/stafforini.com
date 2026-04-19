#!/usr/bin/env bash
# Incrementally sync static/pdfs/ and static/pdf-thumbnails/ to an
# S3-compatible object store (Cloudflare R2).  Run by scripts/deploy.sh
# after process-pdfs so the bucket always matches the local source of
# truth.  `aws s3 sync` is hash/mtime-aware, so subsequent runs upload
# only changed files.
#
# Required environment variables (typically sourced from scripts/r2.env.sh
# or injected via `op run --env-file=...`):
#
#   R2_ENDPOINT             https://<account-id>.r2.cloudflarestorage.com
#   R2_BUCKET               stafforini-pdfs
#   AWS_ACCESS_KEY_ID       R2 API token access key id
#   AWS_SECRET_ACCESS_KEY   R2 API token secret
#
# See scripts/r2.env.sh.example for a template.
source "$(dirname "$0")/common.sh"

: "${R2_ENDPOINT:?set R2_ENDPOINT (see scripts/r2.env.sh.example)}"
: "${R2_BUCKET:?set R2_BUCKET (see scripts/r2.env.sh.example)}"
: "${AWS_ACCESS_KEY_ID:?set AWS_ACCESS_KEY_ID (R2 API token key)}"
: "${AWS_SECRET_ACCESS_KEY:?set AWS_SECRET_ACCESS_KEY (R2 API token secret)}"

# R2 requires an explicit region string; "auto" is Cloudflare's convention.
export AWS_REGION="auto"

sync_tree() {
  local src="$1"
  local prefix="$2"
  if [ ! -d "$src" ]; then
    echo "Skipping $src (not found)"
    return 0
  fi
  echo "Syncing $(basename "$src")/ -> s3://$R2_BUCKET/$prefix/"
  aws s3 sync "$src/" "s3://$R2_BUCKET/$prefix/" \
    --endpoint-url "$R2_ENDPOINT" \
    --only-show-errors
}

sync_tree "$REPO_ROOT/static/pdfs" "pdfs"
sync_tree "$REPO_ROOT/static/pdf-thumbnails" "pdf-thumbnails"

echo "R2 sync complete."
