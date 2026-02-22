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

(provide 'export-common)

;;; export-common.el ends here
