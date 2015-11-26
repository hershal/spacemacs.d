(setq smartparens-local-packages '(smartparens))

(defun smartparens-local/post-init-smartparens ()
  (setq sp-hybrid-kill-excessive-whitespace t)
  (bind-key "M-j" 'sp-join-sexp smartparens-mode-map)
  (bind-key "C-*" (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "(")))
  (bind-key "C-\"" (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "\"")))
  (smartparens-global-mode)
  (show-smartparens-global-mode))
