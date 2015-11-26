(setq lisp-local-packages '(company smartparens))

(defun lisp-local/lisp-style-mode-hook ()
  (sp-local-pair major-mode "'" nil :actions nil)
  (sp-local-pair major-mode "`" nil :actions nil))

(defun lisp-local/post-init-smartparens ()
  (mapc (lambda (hook)
          (add-hook hook 'lisp-local/lisp-style-mode-hook))
        lisp-local-lisp-style-mode-hooks)
  (mapc (lambda (hook)
          (add-hook hook 'turn-on-smartparens-strict-mode))
        lisp-local-strict-style-mode-hooks))

(defun lisp-local/post-init-company ()
  (with-current-buffer "*scratch*" (company-mode)))
