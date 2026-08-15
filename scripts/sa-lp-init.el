;;; Minimal init + driver for batch evaluation of sa-lp babel blocks.
;;; Avoids the user's full init, whose autoload chain breaks under --batch.

(require 'org)
(require 'subr-x)
(org-babel-do-load-languages 'org-babel-load-languages '((python . t)))
(setq org-confirm-babel-evaluate nil)
(setq python-shell-interpreter "/Users/pablostafforini/.pyenv/shims/python3")
(setq shell-file-name "/bin/bash")
(setq shell-command-switch "-c")

(defconst sa-lp-daily-blocks
  '("sa-data" "sa-perf" "sa-chart" "sa-chart-ais" "sa-delay" "sa-calc")
  "Named src blocks to refresh every day.")

(defconst sa-lp-sensitivity-block "sa-sensitivity"
  "Named sensitivity block to refresh after filings or model changes.")

(defconst sa-lp-max-attempts 3
  "Maximum attempts per block on transient failure.")

(defun sa-lp-blocks (&optional include-sensitivity)
  "Return refresh blocks, including sensitivity when requested."
  (if include-sensitivity
      (append (butlast sa-lp-daily-blocks 2)
              (list sa-lp-sensitivity-block)
              (last sa-lp-daily-blocks 2))
    sa-lp-daily-blocks))

(defun sa-lp-refresh (org-file &optional include-sensitivity)
  "Evaluate the daily blocks in ORG-FILE with retry on failure.
When INCLUDE-SENSITIVITY is non-nil, also evaluate the sensitivity
block after the charts and before the delay model.
Raises an error if any block still fails after `sa-lp-max-attempts'
tries."
  (find-file org-file)
  (dolist (name (sa-lp-blocks include-sensitivity))
    (sa-lp-execute-with-retry name))
  (save-buffer))

(defun sa-lp-execute-with-retry (name)
  "Execute the named src block NAME, retrying on transient errors.
Between attempts, sleeps for `sa-lp-backoff-seconds'. Raises if the
block still fails after `sa-lp-max-attempts' tries."
  (let ((attempt 1) success)
    (while (and (not success) (<= attempt sa-lp-max-attempts))
      (message "sa-lp-refresh: %s (attempt %d/%d)"
               name attempt sa-lp-max-attempts)
      (sa-lp-reset-error-state)
      (condition-case err
          (progn
            (org-babel-goto-named-src-block name)
            (org-babel-execute-src-block)
            (when (sa-lp-had-babel-error-p)
              (sa-lp-log-babel-error-output)
              (error "Babel subprocess reported an error"))
            (when (and (string= name sa-lp-sensitivity-block)
                       (sa-lp-sensitivity-result-has-error-p))
              (error "Sensitivity result contains an err cell"))
            (setq success t))
        (error
         (message "sa-lp-refresh: %s failed on attempt %d: %s"
                  name attempt (error-message-string err))
         (when (< attempt sa-lp-max-attempts)
           (sleep-for (sa-lp-backoff-seconds attempt)))
         (setq attempt (1+ attempt)))))
    (unless success
      (error "Block %s failed after %d attempts" name sa-lp-max-attempts))))

(defun sa-lp-sensitivity-result-has-error-p ()
  "Return non-nil when the sensitivity result contains an `err' cell."
  (save-excursion
    (org-babel-goto-named-src-block sa-lp-sensitivity-block)
    (when-let ((result (org-babel-where-is-src-block-result)))
      (goto-char result)
      (let ((end (or (save-excursion
                       (and (re-search-forward "^#\\+end_" nil t)
                            (line-end-position)))
                     (point-max))))
        (re-search-forward "\\berr\\b" end t)))))

(defun sa-lp-backoff-seconds (attempt)
  "Return seconds to sleep before retrying after failed ATTEMPT.
Exponential: 5s, 15s, 45s, …, capped at 60s."
  (min 60 (* 5 (expt 3 (1- attempt)))))

(defun sa-lp-reset-error-state ()
  "Kill any *Org-Babel Error Output* buffer left from a prior block."
  (when-let ((buf (get-buffer "*Org-Babel Error Output*")))
    (kill-buffer buf)))

(defun sa-lp-had-babel-error-p ()
  "Return non-nil if the babel subprocess exited with non-zero status."
  (when-let ((buf (get-buffer "*Org-Babel Error Output*")))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (and (re-search-forward "exited with code \\([0-9]+\\)" nil t)
             (not (string= "0" (match-string 1))))))))

(defun sa-lp-log-babel-error-output ()
  "Write the current Org Babel error buffer to batch stderr/stdout."
  (when-let ((buf (get-buffer "*Org-Babel Error Output*")))
    (with-current-buffer buf
      (message "%s" (string-trim-right (buffer-string))))))
