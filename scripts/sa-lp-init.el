;;; Minimal init for batch evaluation of sa-lp babel blocks.
;;; Avoids the user's full init, whose autoload chain breaks under --batch.

(require 'org)
(org-babel-do-load-languages 'org-babel-load-languages '((python . t)))
(setq org-confirm-babel-evaluate nil)
(setq python-shell-interpreter "/Users/pablostafforini/.pyenv/shims/python3")
(setq shell-file-name "/bin/bash")
(setq shell-command-switch "-c")
