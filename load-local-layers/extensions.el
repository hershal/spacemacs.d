(setq load-local-layers-pre-extensions '(bootstrap))

(defun load-local-layers/init-bootstrap ()
  (org-babel-load-file "~/.spacemacs.d/custom.org")
  (hb/configure)
  (message "HB worked!"))
