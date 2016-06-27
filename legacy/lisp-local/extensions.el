(setq lisp-local-pre-extensions '())

(setq lisp-local-post-extensions '(scratch))

(defun lisp-local/init-scratch ()
  (with-current-buffer "*scratch*" (emacs-lisp-mode)))
