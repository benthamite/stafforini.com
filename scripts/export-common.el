;;; export-common.el --- Shared setup for batch org-to-Hugo exports -*- lexical-binding: t; -*-

;;; Commentary:

;; Loaded by export-notes.el and export-quotes.el to avoid duplicating
;; the elpaca load-path setup and dataless-file detection.

;;; Code:

;; Add all elpaca build directories to load-path.
;; Finds the latest emacs-profiles directory with an elpaca/builds/ subdirectory,
;; so the version number doesn't need to be hardcoded.
(let* ((profiles-dir (expand-file-name "~/.config/emacs-profiles/"))
       (candidates
        (seq-filter
         (lambda (d)
           (file-directory-p
            (expand-file-name "elpaca/builds/" d)))
         (directory-files profiles-dir t "^[0-9]")))
       ;; Sort by version number (strip non-numeric suffixes like "-target"
       ;; before comparing, since version< rejects them)
       (latest (car (last (sort candidates
                                (lambda (a b)
                                  (let ((va (replace-regexp-in-string
                                             "-.*" ""
                                             (file-name-nondirectory a)))
                                        (vb (replace-regexp-in-string
                                             "-.*" ""
                                             (file-name-nondirectory b))))
                                    (version< va vb)))))))
       (builds-dir (when latest
                     (expand-file-name "elpaca/builds/" latest))))
  (when builds-dir
    (message "Using elpaca builds from: %s" builds-dir)
    (dolist (dir (directory-files builds-dir t "^[^.]"))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))

(defun export--file-dataless-p (file)
  "Return non-nil if FILE has the macOS SF_DATALESS flag (Dropbox dehydrated).
Reading such a file blocks indefinitely, so we must skip it."
  (when (eq system-type 'darwin)
    (let ((output (shell-command-to-string
                   (format "ls -lO %s 2>/dev/null" (shell-quote-argument file)))))
      (string-match-p "dataless" output))))

;;;; Dotfiles file-link exporter

(defvar export--dotfiles-github-url
  "https://github.com/benthamite/dotfiles/blob/main/"
  "Base GitHub URL for dotfiles file links.")

(defun export--file-link-exporter (path desc backend _info)
  "Export file: links pointing to dotfiles as GitHub URLs.
For other file: links, return nil to fall through to the default handler.
PATH is the link path, DESC the description, BACKEND the export backend."
  (when (and (eq backend 'hugo)
             (string-match "dotfiles/" path))
    (let ((repo-path (replace-regexp-in-string ".*dotfiles/" "" path)))
      (format "[%s](%s%s)" (or desc (file-name-nondirectory path))
              export--dotfiles-github-url repo-path))))

(defun export--fix-raw-dotfiles-links (output _backend _info)
  "Convert leftover raw [[file:…dotfiles…]] links in OUTPUT to GitHub URLs.
Safety net for cases where `org-link-set-parameters' is not honored."
  (replace-regexp-in-string
   "\\[\\[file:[^]]*dotfiles/\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]"
   (lambda (match)
     (format "[%s](%s%s)"
             (match-string 2 match)
             export--dotfiles-github-url
             (match-string 1 match)))
   output))

;;;; Broken-relref fixer

(defvar export--relref-slug-alist
  '(;; modifications.org has EXPORT_FILE_NAME: my-keyboard-setup, so
    ;; ox-hugo emits relref \"modifications\" which doesn't resolve
    ("modifications" . "/notes/my-keyboard-setup/"))
  "Alist mapping broken relref targets to correct Hugo slugs.
ox-hugo converts file links to .org files into relref shortcodes using
just the filename stem, which may not match the published page slug.
This alist maps the broken stem to the correct URL.")

(defun export--fix-broken-relrefs (output _backend _info)
  "Replace broken relref shortcodes with correct URLs in OUTPUT."
  (let ((result output))
    (dolist (pair export--relref-slug-alist result)
      (setq result (replace-regexp-in-string
                    (format "{{< relref \"%s\" >}}" (regexp-quote (car pair)))
                    (cdr pair)
                    result t t)))))

;;;; Register export hooks

(with-eval-after-load 'ox-hugo
  (org-link-set-parameters "file" :export #'export--file-link-exporter)
  (add-to-list 'org-export-filter-body-functions #'export--fix-raw-dotfiles-links)
  (add-to-list 'org-export-filter-body-functions #'export--fix-broken-relrefs))

(provide 'export-common)

;;; export-common.el ends here
