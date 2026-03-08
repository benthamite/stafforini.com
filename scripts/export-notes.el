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

;; Build a set of known published slugs from the map values, used to
;; validate file: links (which reference filenames, not IDs).
(defvar export-published-slugs
  (let ((slugs (make-hash-table :test 'equal)))
    (maphash (lambda (_id slug)
               (puthash slug t slugs))
             export-id-slug-map)
    slugs)
  "Set of published slugs (values from export-id-slug-map).")

;; Intercept ID and file: links in org-hugo-link BEFORE ox-hugo generates
;; relref shortcodes.  Published targets get proper markdown links;
;; unpublished targets become plain text.  Non-ID/file broken links also
;; degrade gracefully.
(define-advice org-hugo-link (:around (orig-fn link contents info) resolve-links)
  "For id:/file: links, resolve via slug map; for others, catch broken links."
  (let ((type (org-element-property :type link))
        (path (org-element-property :path link)))
    (cond
     ;; ID links: look up in the id-slug-map
     ((string= type "id")
      (let* ((id (upcase path))
             (slug (gethash id export-id-slug-map)))
        (if slug
            (format "[%s](/notes/%s/)" (or contents slug) slug)
          (or contents ""))))
     ;; file: links to .org files: check if the target slug is published
     ((and (string= type "file")
           (string-suffix-p ".org" path))
      (let* ((slug (file-name-sans-extension (file-name-nondirectory path)))
             (published (gethash slug export-published-slugs)))
        (if published
            (format "[%s](/notes/%s/)" (or contents slug) slug)
          (or contents ""))))
     ;; Everything else: delegate to ox-hugo, catch broken links
     (t
      (condition-case _err
          (funcall orig-fn link contents info)
        (org-link-broken
         (or contents (org-element-property :path link))))))))
;; Don't render TODO keywords in exported headings
(setq org-export-with-todo-keywords nil)
;; Exclude entire headings that have any TODO keyword (TODO, WAITING, DONE, DELEGATED, etc.)
(setq org-export-with-tasks nil)
;; Allow Babel processing during export (needed for noweb expansion) but
;; never actually evaluate code blocks (Python etc. aren't available in
;; batch mode).  org-confirm-babel-evaluate nil suppresses interactive
;; prompts; :eval never-export prevents execution while preserving noweb.
(setq org-confirm-babel-evaluate nil)
(setq org-babel-default-header-args
      (cons '(:eval . "never-export")
            (assq-delete-all :eval org-babel-default-header-args)))

;; Register a cite export processor that emits Hugo {{< cite >}} shortcodes.
;; This keeps citation rendering in Hugo (via the cite.html shortcode and
;; work pages) rather than running slow citeproc at export time.
;;
;; Styles:
;;   [cite:@Key]         → full citation
;;   [cite/t:@Key]       → short (title-only) citation
;;   [cite/short:@Key]   → short (title-only) citation (alias)
(org-cite-register-processor 'hugo-cite
  :export-citation
  (lambda (citation style _backend _info)
    (let* ((refs (org-cite-get-references citation))
           (short (member (car style) '("t" "short")))
           (parts
            (mapcar
             (lambda (ref)
               (let* ((key (org-element-property :key ref))
                      (cite-key (if short (concat "-" key) key))
                      ;; Extract suffix (locator) if present
                      (suffix (org-element-property :suffix ref))
                      (suffix-str (if suffix
                                      (string-trim (org-element-interpret-data suffix))
                                    "")))
                 (if (string-empty-p suffix-str)
                     (format "{{< cite \"%s\" >}}" cite-key)
                   ;; Escape quotes in locator for Hugo shortcode arg
                   (format "{{< cite \"%s\" \"%s\" >}}"
                           cite-key
                           (replace-regexp-in-string "\"" "\\\\\"" suffix-str)))))
             refs)))
      (mapconcat #'identity parts ", ")))

  :export-bibliography
  (lambda (_keys _files _style _props _backend _info) ""))

(setq org-cite-export-processors '((t . (hugo-cite))))

;; Use a notes-specific temp file for the ID locations database
(setq org-id-locations-file (expand-file-name "/tmp/org-id-locations-export-notes"))

;; Track stats
(defvar export-notes-exported 0)
(defvar export-notes-errors '())
(defvar export-notes-skipped-dataless '())

(defun export-notes-batch ()
  "Export all blog org files from notes to Hugo."
  (let* ((file-list-path (getenv "EXPORT_FILE_LIST"))
         (notes-dir (expand-file-name
                     "~/My Drive/notes/"))
         (exportable
          (if file-list-path
              ;; Incremental: only export files from the list, filtering dataless
              (progn
                (message "Incremental mode: reading file list from %s" file-list-path)
                (let ((files (with-temp-buffer
                               (insert-file-contents file-list-path)
                               (split-string (buffer-string) "\n" t))))
                  (seq-remove
                   (lambda (f)
                     (when (export--file-dataless-p f)
                       (push f export-notes-skipped-dataless)
                       (message "SKIP dataless: %s" (file-name-nondirectory f))
                       t))
                   files)))
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
      (condition-case err
          (let ((buf (find-file-noselect file)))
            (unwind-protect
                (with-current-buffer buf
                  (export--expand-transclusions)
                  (org-hugo-export-wim-to-md :all-subtrees)
                  (setq export-notes-exported (1+ export-notes-exported))
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
                  (error-message-string err)))))

    (message "\n========================================")
    (message "EXPORT COMPLETE")
    (message "========================================")
    (message "Files exported: %d" export-notes-exported)
    (when export-notes-skipped-dataless
      (message "Skipped (dataless): %d" (length export-notes-skipped-dataless)))
    (when export-notes-errors
      (message "Errors: %d" (length export-notes-errors))
      (dolist (err export-notes-errors)
        (message "  %s: %s" (file-name-nondirectory (car err)) (cdr err))))))

;; Run the export
(export-notes-batch)

;;; export-notes.el ends here
