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

;; Shared export settings
(setq org-export-with-broken-links t)
(setq org-export-exclude-tags '("noexport" "ARCHIVE"))
(setq org-hugo-suppress-lastmod-period 0)
(setq org-id-track-globally t)
(setq enable-local-variables nil)

;; Suppress minor-mode errors in batch mode
(unless (fboundp 'git-auto-commit-mode)
  (defun git-auto-commit-mode (&rest _)))
(unless (fboundp 'init-tangle-conditionally)
  (defun init-tangle-conditionally (&rest _)))

(defun export--file-dataless-p (file)
  "Return non-nil if FILE has the macOS SF_DATALESS flag (cloud-evicted).
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

;;;; Broken-italic fixer

(defun export--fix-broken-italic-close (output _backend _info)
  "Fix closing emphasis broken by file-link export inside /italic/.
When file: links inside org /emphasis/ are resolved, the slash
characters in the file path can confuse the emphasis parser.  The
opening / is converted to _ by ox-blackfriday, but the closing /
is left as a literal character.  This filter converts the trailing
./ to ._ on affected lines."
  (replace-regexp-in-string
   "^\\(_\\[.*\\)\\./$"
   "\\1._"
   output))

;;;; Frontmatter title fixer (ox-hugo #+INCLUDE workaround)

;; ox-hugo occasionally drops the title from TOML frontmatter for org
;; files that use #+INCLUDE directives.  These functions patch the
;; exported .md file immediately after export, so Hugo never sees a
;; page without a title.

(defun export--heading-export-pairs (org-file)
  "Return alist of (EXPORT_FILE_NAME . heading-title) from ORG-FILE.
For each subtree with an :EXPORT_FILE_NAME: property, pairs the
property value with the cleaned-up heading text."
  (let (result)
    (with-temp-buffer
      (insert-file-contents org-file)
      (goto-char (point-min))
      (while (re-search-forward "^\\*+\\s-+\\(.+\\)" nil t)
        (let* ((heading (string-trim (match-string 1)))
               (next-heading-pos
                (save-excursion
                  (if (re-search-forward "^\\*" nil t)
                      (line-beginning-position)
                    (point-max))))
               (export-name
                (save-excursion
                  (when (re-search-forward
                         ":EXPORT_FILE_NAME:\\s-+\\(\\S-+\\)"
                         next-heading-pos t)
                    (match-string 1)))))
          (when export-name
            ;; Strip org tags like :note: at end of heading
            (when (string-match "\\s-+:[[:alnum:]:]+:\\s-*$" heading)
              (setq heading (substring heading 0 (match-beginning 0))))
            ;; Convert org =verbatim= and ~code~ to markdown backticks
            (setq heading (replace-regexp-in-string
                           "=\\([^=]+\\)=" "`\\1`" heading))
            (setq heading (replace-regexp-in-string
                           "~\\([^~]+\\)~" "`\\1`" heading))
            ;; Escape double quotes for TOML
            (setq heading (replace-regexp-in-string
                           "\"" "\\\\\"" heading))
            (push (cons export-name heading) result)))))
    (nreverse result)))

(defun export--inject-title-if-missing (md-path title)
  "Ensure MD-PATH has TITLE in its TOML frontmatter.
Returns non-nil if the title was injected."
  (with-temp-buffer
    (insert-file-contents md-path)
    (goto-char (point-min))
    (when (looking-at "\\+\\+\\+\n")
      (let ((fm-start (match-end 0)))
        (goto-char fm-start)
        (when (re-search-forward "^\\+\\+\\+$" nil t)
          (let ((fm-end (line-beginning-position)))
            (goto-char fm-start)
            (unless (re-search-forward "^title = " fm-end t)
              ;; Title missing — inject it after the opening +++
              (goto-char fm-start)
              (insert (format "title = \"%s\"\n" title))
              (write-region (point-min) (point-max) md-path nil 'silent)
              (message "  [TITLE FIX] %s -> \"%s\""
                       (file-name-nondirectory md-path) title)
              t)))))))

(defun export--fix-missing-titles (org-file hugo-base-dir section)
  "Fix missing titles in markdown files exported from ORG-FILE.
HUGO-BASE-DIR is the Hugo project root, SECTION is the content section
\(e.g. \"notes\").  Reads the org source headings and injects the title
into any exported .md file that lacks one."
  (dolist (pair (export--heading-export-pairs org-file))
    (let* ((slug (car pair))
           (title (cdr pair))
           (md-path (expand-file-name
                     (format "content/%s/%s.md" section slug)
                     hugo-base-dir)))
      (when (and title (file-exists-p md-path))
        (export--inject-title-if-missing md-path title)))))

;;;; Register export hooks

(with-eval-after-load 'ox-hugo
  (org-link-set-parameters "file" :export #'export--file-link-exporter)
  (add-to-list 'org-export-filter-body-functions #'export--fix-raw-dotfiles-links)
  (add-to-list 'org-export-filter-body-functions #'export--fix-broken-relrefs)
  (add-to-list 'org-export-filter-body-functions #'export--fix-broken-italic-close))

;;;; Transclusion support

(require 'org-transclusion)

;; Match user's config: exclude structural elements from transcluded content
(setq org-transclusion-exclude-elements '(headline drawer property-drawer))

(defun export--expand-transclusions ()
  "Expand all #+transclude: directives in the current buffer.
Call this after opening a file and before exporting with ox-hugo,
so that transcluded content is included in the export."
  (with-demoted-errors "org-transclusion error: %S"
    (org-transclusion-add-all)
    ;; `org-transclusion-add' enables `org-transclusion-mode', which installs
    ;; hooks that strip transcluded content on save/kill.  Deactivating the
    ;; mode would call `org-transclusion-remove-all', undoing our work.
    ;; Instead, surgically remove the problematic hooks so the expanded text
    ;; survives through the ox-hugo export that follows.
    (remove-hook 'before-save-hook #'org-transclusion-before-save-buffer t)
    (remove-hook 'after-save-hook #'org-transclusion-after-save-buffer t)
    (remove-hook 'kill-buffer-hook #'org-transclusion-before-kill t)
    (remove-hook 'kill-emacs-hook #'org-transclusion-before-kill t)))

;;;; ID-slug map for cross-reference resolution

(defvar export-id-slug-map nil
  "Hash-table mapping org-id → Hugo slug for cross-references.")

(let ((map-file (expand-file-name "data/id-slug-map.json"
                                    (locate-dominating-file load-file-name ".git"))))
  (if (file-exists-p map-file)
      (setq export-id-slug-map
            (with-temp-buffer
              (insert-file-contents map-file)
              (json-parse-string (buffer-string) :object-type 'hash-table)))
    (setq export-id-slug-map (make-hash-table :test 'equal))
    (message "WARNING: %s not found — id: links will render as plain text" map-file)))

;;;; Pre-build org-id database for cross-reference resolution

(require 'org-id)
;; Use a temp file for the ID locations database (avoid polluting the user's real one)
(setq org-id-locations-file (expand-file-name "/tmp/org-id-locations-batch-export"))

(let ((notes-dir (expand-file-name "~/My Drive/notes/"))
      (bib-dir   (expand-file-name "~/My Drive/bibliographic-notes/")))
  (org-id-update-id-locations
   (append
    (directory-files-recursively notes-dir "\\.org$")
    (directory-files-recursively bib-dir "\\.org$"))))

(provide 'export-common)

;;; export-common.el ends here
