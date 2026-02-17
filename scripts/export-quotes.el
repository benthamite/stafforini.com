;;; export-quotes.el --- Batch export :public: org subheadings to Hugo markdown -*- lexical-binding: t; -*-

;; Run via: emacs --batch -l scripts/export-quotes.el

;;; Code:

;; Add all elpaca build directories to load-path
(let ((builds-dir (expand-file-name "~/.config/emacs-profiles/7.1.29/elpaca/builds/")))
  (when (file-directory-p builds-dir)
    (dolist (dir (directory-files builds-dir t "^[^.]"))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))

(require 'ox-hugo)

;; Do NOT set org-cite-global-bibliography — we don't need citation resolution.
;; Citations are stripped in post-processing; setting the bibliography causes
;; org-cite-basic to parse all bib files per subtree, which is extremely slow
;; and errors out in batch mode.

;; Silence unnecessary output during batch export
(setq org-export-with-broken-links t)
;; Exclude :ARCHIVE: tagged headings from export
(setq org-export-exclude-tags '("noexport" "ARCHIVE"))
(setq org-hugo-suppress-lastmod-period 0)

;; Disable citation processing entirely — register a no-op export processor
;; that just renders the raw [cite:@key] as empty text
(org-cite-register-processor 'noop
  :export-citation (lambda (_citation _style _backend _info) "")
  :export-bibliography (lambda (_keys _files _style _props _backend _info) ""))
(setq org-cite-export-processors '((t . (noop))))

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

(defun export-quotes-batch ()
  "Export all :public:-tagged subheadings from bibliographic notes."
  (let* ((notes-dir (expand-file-name "~/Library/CloudStorage/Dropbox/bibliographic-notes/"))
         (all-files (directory-files notes-dir t "\\.org$"))
         (total (length all-files))
         (processed 0))
    (message "Found %d .org files in %s" total notes-dir)

    ;; Pre-filter: only files containing :public:
    (message "Filtering for files with :public: tag...")
    (let ((public-files
           (seq-filter
            (lambda (f)
              (with-temp-buffer
                (insert-file-contents f)
                (goto-char (point-min))
                (re-search-forward ":public:" nil t)))
            all-files)))
      (message "Found %d files with :public: subheadings" (length public-files))

      (dolist (file public-files)
        (setq processed (1+ processed))
        (when (= (% processed 50) 0)
          (message "Progress: %d/%d files (%d%%)"
                   processed (length public-files)
                   (/ (* 100 processed) (length public-files))))
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
    (when export-errors
      (message "Errors: %d" (length export-errors))
      (dolist (err export-errors)
        (message "  %s: %s" (file-name-nondirectory (car err)) (cdr err))))))

;; Run the export
(export-quotes-batch)

;;; export-quotes.el ends here
