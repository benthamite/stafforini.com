;;; export-notes.el --- Batch export blog org files to Hugo markdown -*- lexical-binding: t; -*-

;; Run via: emacs --batch -l scripts/export-notes.el
;;
;; Exports all org files in the pablos-miscellany directory to Hugo
;; markdown in content/notes/, using ox-hugo per-subtree export.
;;
;; Prerequisites: run prepare-org-notes.py first to add ox-hugo
;; metadata (:EXPORT_FILE_NAME:, :EXPORT_HUGO_SECTION:, etc.)

;;; Code:

;; Add all elpaca build directories to load-path
(let ((builds-dir (expand-file-name "~/.config/emacs-profiles/7.1.29/elpaca/builds/")))
  (when (file-directory-p builds-dir)
    (dolist (dir (directory-files builds-dir t "^[^.]"))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))

(require 'ox-hugo)

;; Allow broken links (some old WordPress links may be dead)
(setq org-export-with-broken-links t)
;; Exclude :ARCHIVE: and :noexport: tagged headings from export
(setq org-export-exclude-tags '("noexport" "ARCHIVE"))
;; Don't render TODO keywords in exported headings
(setq org-export-with-todo-keywords nil)
;; Exclude entire headings that have any TODO keyword (TODO, WAITING, DONE, DELEGATED, etc.)
(setq org-export-with-tasks nil)
(setq org-hugo-suppress-lastmod-period 0)

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

;; Define dummy git-auto-commit-mode
(unless (fboundp 'git-auto-commit-mode)
  (defun git-auto-commit-mode (&rest _) nil))

;; Track stats
(defvar export-notes-total 0)
(defvar export-notes-errors '())

(defun export-notes-batch ()
  "Export all blog org files from pablos-miscellany to Hugo."
  (let* ((notes-dir (expand-file-name
                     "~/Library/CloudStorage/Dropbox/websites/pablos-miscellany/"))
         (all-files (directory-files notes-dir t "\\.org$"))
         ;; Skip the monolithic project file
         (files (seq-remove
                 (lambda (f) (string= (file-name-nondirectory f) "pablos-miscellany.org"))
                 all-files))
         ;; Only process files that have ox-hugo metadata
         (exportable
          (seq-filter
           (lambda (f)
             (with-temp-buffer
               (insert-file-contents f)
               (goto-char (point-min))
               (re-search-forward ":EXPORT_FILE_NAME:" nil t)))
           files))
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
                  (org-hugo-export-wim-to-md :all-subtrees)
                  (setq export-notes-total (1+ export-notes-total)))
              (kill-buffer buf)))
        (error
         (push (cons file (error-message-string err)) export-notes-errors)
         (message "ERROR in %s: %s"
                  (file-name-nondirectory file)
                  (error-message-string err)))))

    (message "\n========================================")
    (message "EXPORT COMPLETE")
    (message "========================================")
    (message "Files exported: %d" export-notes-total)
    (when export-notes-errors
      (message "Errors: %d" (length export-notes-errors))
      (dolist (err export-notes-errors)
        (message "  %s: %s" (file-name-nondirectory (car err)) (cdr err))))))

;; Run the export
(export-notes-batch)

;; Inject lastmod dates from org file modification times
(let ((default-directory (expand-file-name
                          "~/Library/CloudStorage/Dropbox/repos/stafforini.com/")))
  (message "\nInjecting lastmod dates...")
  (message "%s" (shell-command-to-string "python3 scripts/inject-lastmod.py")))

;;; export-notes.el ends here
