# ox-hugo dropped-title bug

`org-hugo--get-sanitized-title` intermittently returns nil during **interactive** Emacs exports. `(plist-get info :title)` is nil, tomelr drops the title from TOML front matter, and the note appears titleless on the site. Batch exports are never affected.

## Root cause (not fully resolved)

In `org-export--get-subtree-options` (ox.el), `looking-at org-complex-heading-regexp` is not checked for nil. If it fails, `match-string-no-properties 4` returns garbage from stale match data. What causes `looking-at` to fail in the interactive session is unknown -- possibly babel processing or async Emacs mechanisms clobbering match data.

## Protection layers

1. **`stafforini--ensure-title` advice** on `org-hugo--get-sanitized-title`. Detects nil title and recovers it via `save-excursion` + `save-restriction` + `widen` + `org-get-heading`. Uses inline expansion (not `org-with-wide-buffer` macro) to avoid byte-compilation mismatch.
2. **Duplicate-drawer monitor** (`after-save-hook` in org-mode). Warns if consecutive `:PROPERTIES:` drawers exist under the first heading.
3. **Batch export as fallback.** If title is missing on disk, re-export in batch mode (always works).

## Design principle

Do NOT add template-level fallbacks. The title must be present in front matter. (Enforced as a Critical rule in `CLAUDE.md`.)

## Byte-compilation discipline

Never byte-compile Elisp in `emacs --batch` when the target is the interactive Emacs. Always compile via `emacsclient -e '(byte-compile-file ...)'` in the running session. `elpaca-rebuild` can also miscompile if dependencies are not yet loaded. (Enforced as a Critical rule in `CLAUDE.md`.)

## After Emacs profile migration

1. Pull all package repos to match the remote.
2. After rebuild, verify the advice is active: `(fboundp 'stafforini--ensure-title)`.
3. If not, force-load the `.el` and recompile via `emacsclient`.
