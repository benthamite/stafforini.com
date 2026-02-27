;;; export-notes.el --- Batch export blog org files to Hugo markdown -*- lexical-binding: t; -*-

;; Run via: emacs --batch -l scripts/export-notes.el
;;
;; Exports all org files in the notes directory to Hugo
;; markdown in content/notes/, using ox-hugo per-subtree export.
;;
;; Notes must have ox-hugo metadata (:EXPORT_FILE_NAME:, etc.)
;; added via M-x stafforini-publish-note before export.

;;; Code:

;; Shared setup: elpaca load-path and export--file-dataless-p
(load (expand-file-name "export-common.el"
                        (file-name-directory load-file-name)))

(require 'ox-hugo)

;; Allow broken links (some old WordPress links may be dead)
(setq org-export-with-broken-links t)
;; Exclude :ARCHIVE: and :noexport: tagged headings from export
(setq org-export-exclude-tags '("noexport" "ARCHIVE"))
;; Don't render TODO keywords in exported headings
(setq org-export-with-todo-keywords nil)
;; Exclude entire headings that have any TODO keyword (TODO, WAITING, DONE, DELEGATED, etc.)
(setq org-export-with-tasks nil)
;; Always include lastmod in front matter (0 = no suppression period)
(setq org-hugo-suppress-lastmod-period 0)
;; Never evaluate source blocks during export (some notes have :cache yes
;; Python blocks that fail in batch mode — we only need the markup, not results)
(setq org-export-use-babel nil)

;; Register a cite export processor that emits Hugo {{< cite >}} shortcodes.
;; This keeps citation rendering in Hugo (via the cite.html shortcode and
;; work pages) rather than running slow citeproc at export time.
(org-cite-register-processor 'hugo-cite
  :export-citation
  (lambda (citation _style _backend _info)
    (let* ((refs (org-cite-get-references citation))
           (parts
            (mapcar
             (lambda (ref)
               (let* ((key (org-element-property :key ref))
                      ;; Extract suffix (locator) if present
                      (suffix (org-element-property :suffix ref))
                      (suffix-str (if suffix
                                      (string-trim (org-element-interpret-data suffix))
                                    "")))
                 (if (string-empty-p suffix-str)
                     (format "{{< cite \"%s\" >}}" key)
                   ;; Escape quotes in locator for Hugo shortcode arg
                   (format "{{< cite \"%s\" \"%s\" >}}"
                           key
                           (replace-regexp-in-string "\"" "\\\\\"" suffix-str)))))
             refs)))
      (mapconcat #'identity parts ", ")))

  :export-bibliography
  (lambda (_keys _files _style _props _backend _info) ""))

(setq org-cite-export-processors '((t . (hugo-cite))))

;; Enable org-id tracking (required by ox-hugo for ID-based exports)
(setq org-id-track-globally t)
(setq org-id-locations-file (expand-file-name "/tmp/org-id-locations-export-notes"))

;; Suppress local variable evaluation
(setq enable-local-variables nil)

;; Define dummy functions not available in batch mode
(unless (fboundp 'git-auto-commit-mode)
  (defun git-auto-commit-mode (&rest _) nil))
;; init-tangle-conditionally is called by some org files on visit;
;; return "no" to skip tangling in batch mode
(unless (fboundp 'init-tangle-conditionally)
  (defun init-tangle-conditionally (&rest _) "no"))

;; Pre-scan all org files to build the ID→file mapping so that [[id:...]]
;; links between notes resolve correctly during export.
(let ((notes-dir (expand-file-name
                  "~/My Drive/notes/")))
  (org-id-update-id-locations
   (directory-files-recursively notes-dir "\\.org$")))

;; Track stats
(defvar export-notes-total 0)
(defvar export-notes-errors '())
(defvar export-notes-skipped-dataless '())

(defun export-notes-batch ()
  "Export all blog org files from notes to Hugo."
  (let* ((file-list-path (getenv "EXPORT_FILE_LIST"))
         (notes-dir (expand-file-name
                     "~/My Drive/notes/"))
         (exportable
          (if file-list-path
              ;; Incremental: only export files from the list
              (progn
                (message "Incremental mode: reading file list from %s" file-list-path)
                (with-temp-buffer
                  (insert-file-contents file-list-path)
                  (split-string (buffer-string) "\n" t)))
            ;; Full: scan recursively and pre-filter
            (let* ((all-files (directory-files-recursively notes-dir "\\.org$"))
                   (files (seq-remove
                           ;; pablos-miscellany.org is the org-roam index file, not a blog post
                           (lambda (f) (string= (file-name-nondirectory f) "pablos-miscellany.org"))
                           all-files)))
              (seq-filter
               (lambda (f)
                 (if (export--file-dataless-p f)
                     (progn
                       (push f export-notes-skipped-dataless)
                       (message "SKIP dataless: %s" (file-name-nondirectory f))
                       nil)
                   (with-temp-buffer
                     (insert-file-contents f)
                     (goto-char (point-min))
                     (re-search-forward ":EXPORT_FILE_NAME:" nil t))))
               files))))
         (total (length exportable))
         (processed 0))

    (message "Found %d exportable org files in %s" total notes-dir)

    (dolist (file exportable)
      (setq processed (1+ processed))
      (when (= (% processed 10) 0)
        (message "Progress: %d/%d files" processed total))
      (if (export--file-dataless-p file)
          (progn
            (push file export-notes-skipped-dataless)
            (message "SKIP dataless: %s" (file-name-nondirectory file)))
        (condition-case err
            (let ((buf (find-file-noselect file)))
              (unwind-protect
                  (with-current-buffer buf
                    (org-hugo-export-wim-to-md :all-subtrees)
                    (setq export-notes-total (1+ export-notes-total))
                    ;; Fix titles dropped by ox-hugo #+INCLUDE bug.
                    ;; Patch the .md immediately so Hugo never serves
                    ;; a page without a title.
                    (export--fix-missing-titles
                     file
                     (expand-file-name
                      "~/My Drive/repos/stafforini.com/")
                     "notes"))
                (kill-buffer buf)))
          (error
           (push (cons file (error-message-string err)) export-notes-errors)
           (message "ERROR in %s: %s"
                    (file-name-nondirectory file)
                    (error-message-string err))))))

    (message "\n========================================")
    (message "EXPORT COMPLETE")
    (message "========================================")
    (message "Files exported: %d" export-notes-total)
    (when export-notes-skipped-dataless
      (message "Skipped (dataless): %d" (length export-notes-skipped-dataless)))
    (when export-notes-errors
      (message "Errors: %d" (length export-notes-errors))
      (dolist (err export-notes-errors)
        (message "  %s: %s" (file-name-nondirectory (car err)) (cdr err))))))

;; Run the export
(export-notes-batch)

;;; export-notes.el ends here
