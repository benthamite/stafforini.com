#!/usr/bin/env bash
# Detect and restore Dropbox "dataless" (dehydrated) .org files from git.
#
# Dropbox's FileProvider can dehydrate files even when "sync everything locally"
# is enabled. A dataless file blocks indefinitely on read(), which hangs
# batch Emacs export. This script restores dataless files from git before export.
#
# Usage: bash scripts/fix-dataless-files.sh
# Called automatically by export-notes.sh and export-quotes.sh.

set -euo pipefail

BIB_DIR="${HOME}/Library/CloudStorage/Dropbox/bibliographic-notes"
BIB_GIT="${HOME}/git-dirs/bibliographic-notes"
NOTES_DIR="${HOME}/Library/CloudStorage/Dropbox/websites/pablos-miscellany"
NOTES_GIT=""

# Detect notes git directory
if [ -f "$NOTES_DIR/.git" ]; then
    local_gitdir=$(sed 's/gitdir: //' < "$NOTES_DIR/.git")
    # Resolve relative paths against the directory containing .git
    NOTES_GIT=$(cd "$NOTES_DIR" && realpath "$local_gitdir")
elif [ -d "$NOTES_DIR/.git" ]; then
    NOTES_GIT="$NOTES_DIR/.git"
fi

restore_dir() {
    local dir="$1"
    local gitdir="$2"
    local label="$3"

    # Find dataless .org files by iterating with a glob (safe for filenames with spaces)
    local dataless_files=()
    for f in "$dir"/*.org; do
        [ -e "$f" ] || continue
        if ls -lO "$f" 2>/dev/null | grep -q "dataless"; then
            dataless_files+=("$f")
        fi
    done

    local count=${#dataless_files[@]}
    [ "$count" -eq 0 ] && return 0

    echo "[$label] Found $count dataless .org files — restoring from git..."

    local restored=0
    local failed=0
    for fullpath in "${dataless_files[@]}"; do
        local fname
        fname=$(basename "$fullpath")
        local tmpfile
        tmpfile=$(mktemp /tmp/_fix_dataless_XXXXXX)

        if GIT_DIR="$gitdir" GIT_WORK_TREE="$dir" git show "HEAD:$fname" > "$tmpfile" 2>/dev/null; then
            local sz
            sz=$(wc -c < "$tmpfile" | tr -d ' ')
            if [ "$sz" -gt 0 ] && timeout 5 trash "$fullpath" 2>/dev/null && timeout 10 mv "$tmpfile" "$fullpath" 2>/dev/null; then
                restored=$((restored + 1))
            else
                echo "  WARNING: could not restore $fname"
                rm -f "$tmpfile"
                failed=$((failed + 1))
            fi
        else
            echo "  WARNING: $fname not found in git"
            rm -f "$tmpfile"
            failed=$((failed + 1))
        fi
    done

    echo "[$label] Restored $restored / $count files"
    [ "$failed" -gt 0 ] && echo "[$label] Could not restore $failed files"
    return 0
}

# Restore bibliographic notes
if [ -d "$BIB_DIR" ] && [ -d "$BIB_GIT" ]; then
    restore_dir "$BIB_DIR" "$BIB_GIT" "quotes"
fi

# Restore notes
if [ -d "$NOTES_DIR" ] && [ -n "$NOTES_GIT" ] && [ -d "$NOTES_GIT" ]; then
    restore_dir "$NOTES_DIR" "$NOTES_GIT" "notes"
fi
