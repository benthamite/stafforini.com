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
  (dolist (state (list "TODO" "DOING" "IMPORTANT" "URGENT" "SOMEDAY"
                      "MAYBE" "WAITING" "PROJECT" "NEXT" "LATER"
                      "DELEGATED" "DONE" "CANCELLED" ""))
    (with-temp-buffer
      (insert (format "* %s%s[#3] Parent\n** Quote\n:PROPERTIES:\n:EXPORT_FILE_NAME: quote\n:END:\nText\n"
                      state (if (equal state "") "" " ")))
      (org-mode)
      (goto-char (point-min))
      (unless (equal (org-get-todo-state) (unless (equal state "") state))
        (error "Batch Org does not recognize task state %S" state))
      (re-search-forward "^\\*\\* Quote")
      (let ((path (org-get-outline-path t))
            (buffer (org-hugo--get-pre-processed-buffer)))
        (unwind-protect
            (with-current-buffer buffer
              (goto-char (org-find-olp path t))
              (unless (equal (org-get-heading t t t t) "Quote")
                (error "Preprocessing lost the quote under task state %S" state)))
          (kill-buffer buffer)))))
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
if ! EXPORT_FILE_LIST="$empty_list" emacs --batch -l "$SCRIPT_DIR/export-quotes.el" --eval '
(let ((export-hugo-base-dir (make-temp-file "quote-export-check-" t)))
  (unwind-protect
      (dolist (header (list "" "#+hugo_base_dir: /obsolete/site\n"
                           "#+HUGO_BASE_DIR: /obsolete/site\n"))
        (with-temp-buffer
          (insert header "* DOING [#3] Parent\n** Quote :public:\n:PROPERTIES:\n:EXPORT_FILE_NAME: regression-quote\n:EXPORT_HUGO_SECTION: quotes\n:END:\nA quote preserved through export.\n")
          (org-mode)
          (export--ensure-hugo-base-dir)
          (let ((once (buffer-string)))
            (export--ensure-hugo-base-dir)
            (unless (equal once (buffer-string))
              (error "Setting the quote destination is not idempotent")))
          (org-hugo-export-wim-to-md :all-subtrees)
          (let ((output (expand-file-name "content/quotes/regression-quote.md"
                                          export-hugo-base-dir)))
            (unless (file-exists-p output)
              (error "Quote was not exported for header %S" header))
            (with-temp-buffer
              (insert-file-contents output)
              (unless (search-forward "A quote preserved through export." nil t)
                (error "Exported quote lost its body")))
            (delete-file output))))
    (delete-directory export-hugo-base-dir t)))' >"$emacs_log" 2>&1; then
  cat "$emacs_log" >&2
  exit 1
fi
for section in notes quotes; do
  if ! EXPORT_SECTION="$section" EXPORT_FILE_LIST="$empty_list" emacs --batch \
      -l "$SCRIPT_DIR/export-$section.el" --eval '
(let ((file-list (make-temp-file "export-evicted-list-")))
  (unwind-protect
      (progn
        (with-temp-file file-list
          (insert "/fixture/cloud-evicted.org\n"))
        (setenv "EXPORT_FILE_LIST" file-list)
        (cl-letf (((symbol-function (quote export--file-dataless-p))
                   (lambda (_file) t)))
          (let ((failure (condition-case err
                             (progn
                               (funcall (intern (format "export-%s-batch"
                                                        (getenv "EXPORT_SECTION"))))
                               nil)
                           (error (error-message-string err)))))
            (unless (and failure (string-prefix-p "Incomplete source scan:" failure))
              (error "Cloud eviction after Python discovery did not abort: %S" failure)))))
    (delete-file file-list)))' >"$emacs_log" 2>&1; then
    cat "$emacs_log" >&2
    exit 1
  fi
done
echo "export Elisp OK: task headings, quote destinations, details blocks, and cloud eviction verified"
