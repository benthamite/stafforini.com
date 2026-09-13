#!/usr/bin/env bash
# Batch-load the notes export setup and verify the ox-blackfriday
# details-summary override in scripts/export-common.el: it must agree with
# the original regexp on small inputs, and a details block too large for that
# regexp must export with the override but fail without it. Used as commit
# evidence for changes to the export Elisp.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
empty_list="$(mktemp)"
emacs_log="$(mktemp)"
trap 'rm -f "$empty_list" "$emacs_log"' EXIT

# Loading the export setup prints one progress line per notes file, so keep
# Emacs output in a log and show it only when the check fails.
if ! EXPORT_FILE_LIST="$empty_list" emacs --batch -l "$SCRIPT_DIR/export-notes.el" --eval '
(progn
  (dolist (s (list "" "no summary here" "<summary>x</summary>rest"
                   "pre<summary>a</summary>mid</summary>tail"
                   "</summary>before<summary>open only"
                   "<summary>a\nb</summary>\n<summary>c</summary>\nbody"
                   "<summary></summary>" "<summary>"
                   "x</summary>y<summary>z</summary>"))
    (let ((expected (and (string-match export--blackfriday-summary-regexp s)
                         (list (match-beginning 0) (match-end 0)
                               (match-beginning 1) (match-end 1))))
          (actual (and (export--blackfriday-summary-match
                        (lambda (&rest _) (error "Original string-match called"))
                        export--blackfriday-summary-regexp s)
                       (list (match-beginning 0) (match-end 0)
                             (match-beginning 1) (match-end 1)))))
      (unless (equal expected actual)
        (error "Summary match differs for %S: expected %S, got %S"
               s expected actual))))
  (let* ((code (mapconcat (lambda (i) (format "value_%d = %d  # padding line" i i))
                          (number-sequence 1 8000) "\n"))
         (org (concat "#+begin_details\n#+begin_summary\nCode\n#+end_summary\n"
                      "#+begin_src python\n" code "\n#+end_src\n#+end_details\n"))
         (md (org-export-string-as org (quote hugo) t)))
    (unless (> (length md) 200000)
      (error "Large details fixture too small: %d characters" (length md)))
    (unless (string-match-p "<summary>Code</summary>\n<div class=\"details\">" md)
      (error "Large details block exported without its summary wrapper"))
    (advice-remove (quote org-blackfriday-special-block)
                   (function export--blackfriday-special-block-linear-summary))
    (unless (condition-case nil
                (progn (org-export-string-as org (quote hugo) t) nil)
              (error t))
      (error "Fixture no longer reproduces the unpatched overflow")))
  (message "export Elisp OK: details-summary override verified"))' >"$emacs_log" 2>&1; then
  cat "$emacs_log" >&2
  exit 1
fi
echo "export Elisp OK: details-summary override verified"
