;;; export-quotes.el --- Batch export :public: org subheadings to Hugo markdown -*- lexical-binding: t; -*-

;; Run via: emacs --batch -l scripts/export-quotes.el

;;; Code:

;; Shared setup: elpaca load-path and export--file-dataless-p
(load (expand-file-name "export-common.el"
                        (file-name-directory load-file-name)))

(require 'ox-hugo)

;; Do NOT set org-cite-global-bibliography — we don't need citation resolution.
;; Citations are stripped in post-processing; setting the bibliography causes
;; org-cite-basic to parse all bib files per subtree, which is extremely slow
;; and errors out in batch mode.

;; Override hugo_base_dir: org files still reference the old Dropbox path,
;; but the repo now lives under Google Drive.  ox-hugo reads #+hugo_base_dir:
;; from the buffer as an export option; there is no `org-hugo--get-basedir'
;; function to advise.  Instead, rewrite the keyword in each buffer before export.
(defvar export-hugo-base-dir
  (file-name-directory (directory-file-name
                        (file-name-directory load-file-name)))
  "Hugo base directory, derived from this script's location (repo root).")

(defun export--rewrite-hugo-base-dir ()
  "Replace any #+hugo_base_dir: line in the current buffer with the repo root."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           "^#\\+hugo_base_dir:.*$" nil t)
      (replace-match
       (format "#+hugo_base_dir: %s" export-hugo-base-dir)))))

;; Disable citation processing entirely — register a no-op export processor
;; that just renders the raw [cite:@key] as empty text
(org-cite-register-processor 'noop
  :export-citation (lambda (_citation _style _backend _info) "")
  :export-bibliography (lambda (_keys _files _style _props _backend _info) ""))
(setq org-cite-export-processors '((t . (noop))))

;; Resolve id: links to proper Hugo note URLs when possible.
;; Some topic/person notes may lack EXPORT_FILE_NAME, so ox-hugo silently
;; drops the entire link element.
;; We look up the org ID in export-id-slug-map and emit a markdown link
;; when found; otherwise fall back to plain text description.  Notes with
;; :EXPORT_HUGO_URL: overrides (e.g. /about/, /contact/) are resolved via
;; export-id-url-overrides so the emitted URL matches the rendered page.
(org-link-set-parameters
 "id"
 :export (lambda (path desc _backend _info)
           (let* ((id (upcase path))
                  (slug (gethash id export-id-slug-map))
                  (url (and slug
                            (or (gethash id export-id-url-overrides)
                                (format "/notes/%s/" slug)))))
             (if slug
                 (format "[%s](%s)" (or desc slug) url)
               (or desc "")))))

;; Track stats
(defvar export-total-files 0)
(defvar export-total-subtrees 0)
(defvar export-errors '())
(defvar export-skipped-dataless '())

(defun export-quotes-batch ()
  "Export all :public:-tagged subheadings from bibliographic notes."
  (let* ((file-list-path (getenv "EXPORT_FILE_LIST"))
         (notes-dir (expand-file-name "~/My Drive/bibliographic-notes/"))
         (public-files
          (if file-list-path
              ;; Batch wrapper passes the full exportable file list.
              (progn
                (message "Reading export file list from %s" file-list-path)
                (let ((files (with-temp-buffer
                               (insert-file-contents file-list-path)
                               (split-string (buffer-string) "\n" t))))
                  (seq-remove
                   (lambda (f)
                     (when (export--file-dataless-p f)
                       (push f export-skipped-dataless)
                       (message "SKIP dataless: %s" (file-name-nondirectory f))
                       t))
                   files)))
            ;; Full: scan and pre-filter
            (let ((all-files (directory-files notes-dir t "\\.org$")))
              (message "Found %d .org files in %s" (length all-files) notes-dir)
              (message "Filtering for files with :public: tag...")
              (seq-filter
               (lambda (f)
                 (if (export--file-dataless-p f)
                     (progn
                       (push f export-skipped-dataless)
                       (message "SKIP dataless: %s" (file-name-nondirectory f))
                       nil)
                   (with-temp-buffer
                     (insert-file-contents f)
                     (goto-char (point-min))
                     (re-search-forward ":public:" nil t))))
               all-files))))
         (processed 0))
    (message "Found %d files with :public: subheadings" (length public-files))

    (dolist (file public-files)
      (setq processed (1+ processed))
      (when (= (% processed 10) 0)
        (message "Progress: %d/%d files (%d%%)"
                 processed (length public-files)
                 (/ (* 100 processed) (length public-files))))
      (condition-case err
          (let ((buf (find-file-noselect file)))
            (unwind-protect
                (with-current-buffer buf
                  (export--rewrite-hugo-base-dir)
                  (export--expand-includes)
                  (export--expand-transclusions)
                  (let ((count (org-hugo-export-wim-to-md :all-subtrees)))
                    (when count
                      (setq export-total-subtrees (+ export-total-subtrees
                                                    (if (numberp count) count 1)))
                      (setq export-total-files (1+ export-total-files)))))
              ;; Always kill buffer to free memory
              (kill-buffer buf)))
        (error
         (push (cons file (error-message-string err)) export-errors)
         (message "ERROR in %s: %s" (file-name-nondirectory file)
                  (error-message-string err)))))

    (message "\n========================================")
    (message "EXPORT COMPLETE")
    (message "========================================")
    (message "Files exported: %d" export-total-files)
    (message "Subtrees exported: %d" export-total-subtrees)
    (when export-skipped-dataless
      (message "Skipped (dataless): %d" (length export-skipped-dataless)))
    (when export-errors
      (message "Errors: %d" (length export-errors))
      (dolist (err export-errors)
        (message "  %s: %s" (file-name-nondirectory (car err)) (cdr err)))
      (kill-emacs 1))))

;; Run the export
(export-quotes-batch)

;;; export-quotes.el ends here
