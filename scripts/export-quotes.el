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

;; Silence unnecessary output during batch export
(setq org-export-with-broken-links t)
;; Exclude :ARCHIVE: tagged headings from export
(setq org-export-exclude-tags '("noexport" "ARCHIVE"))
;; Always include lastmod in front matter (0 = no suppression period)
(setq org-hugo-suppress-lastmod-period 0)

;; Disable citation processing entirely — register a no-op export processor
;; that just renders the raw [cite:@key] as empty text
(org-cite-register-processor 'noop
  :export-citation (lambda (_citation _style _backend _info) "")
  :export-bibliography (lambda (_keys _files _style _props _backend _info) ""))
(setq org-cite-export-processors '((t . (noop))))

;; Load org-id → slug mapping for topic links
(defvar export-id-slug-map
  (let ((map-file "/tmp/id-slug-map.json"))
    (if (file-exists-p map-file)
        (with-temp-buffer
          (insert-file-contents map-file)
          (json-parse-string (buffer-string) :object-type 'hash-table))
      (progn
        (message "WARNING: %s not found — id: links will render as plain text" map-file)
        (make-hash-table :test 'equal))))
  "Hash-table mapping org IDs to Hugo slugs for topic links.")

;; Resolve id: links to proper Hugo note URLs when possible.
;; Topic notes (~/notes/tags/, ~/people/tags/) are org-roam stubs without
;; EXPORT_FILE_NAME, so ox-hugo silently drops the entire link element.
;; We look up the org ID in export-id-slug-map and emit a markdown link
;; when found; otherwise fall back to plain text description.
(org-link-set-parameters
 "id"
 :export (lambda (path desc _backend _info)
           ;; Upcase to match org-roam's convention for stored IDs
           (let* ((id (upcase path))
                  (slug (gethash id export-id-slug-map)))
             (if slug
                 ;; All topic stubs live under /notes/ regardless of their source directory
                 (format "[%s](/notes/%s/)" (or desc slug) slug)
               (or desc "")))))

;; Enable org-id tracking (required by ox-hugo for ID-based exports)
(setq org-id-track-globally t)
;; Use a temp file for the ID locations database (avoid polluting real one)
(setq org-id-locations-file (expand-file-name "/tmp/org-id-locations-export"))

;; Suppress local variable evaluation (some files use git-auto-commit-mode etc.)
(setq enable-local-variables nil)

;; Define dummy git-auto-commit-mode in case it's referenced despite suppression
(unless (fboundp 'git-auto-commit-mode)
  (defun git-auto-commit-mode (&rest _) nil))

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
              ;; Incremental: only export files from the list
              (progn
                (message "Incremental mode: reading file list from %s" file-list-path)
                (with-temp-buffer
                  (insert-file-contents file-list-path)
                  (split-string (buffer-string) "\n" t)))
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
      (if (export--file-dataless-p file)
          (progn
            (push file export-skipped-dataless)
            (message "SKIP dataless: %s" (file-name-nondirectory file)))
        (condition-case err
            (let ((buf (find-file-noselect file)))
              (unwind-protect
                  (with-current-buffer buf
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
                    (error-message-string err))))))

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
        (message "  %s: %s" (file-name-nondirectory (car err)) (cdr err))))))

;; Run the export
(export-quotes-batch)

;;; export-quotes.el ends here
