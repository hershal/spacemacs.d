(setq smartparens-local-packages '(smartparens))

(defun smartparens-local/post-init-smartparens ()
  (add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)

  (setq sp-hybrid-kill-excessive-whitespace t)

  (bind-key "M-j" 'sp-join-sexp smartparens-mode-map)

  (bind-key "C-M-f" 'sp-forward-sexp smartparens-mode-map)
  (bind-key "C-M-b" 'sp-backward-sexp smartparens-mode-map)

  (bind-key "C-M-d" 'sp-down-sexp smartparens-mode-map)
  (bind-key "C-M-a" 'sp-backward-down-sexp smartparens-mode-map)
  (bind-key "C-S-d" 'sp-beginning-of-sexp smartparens-mode-map)
  (bind-key "C-S-a" 'sp-end-of-sexp smartparens-mode-map)

  (bind-key "C-M-e" 'sp-up-sexp smartparens-mode-map)
  (bind-key "C-M-u" 'sp-backward-up-sexp smartparens-mode-map)
  (bind-key "C-M-t" 'sp-transpose-sexp smartparens-mode-map)

  (bind-key "C-M-n" 'sp-next-sexp smartparens-mode-map)
  (bind-key "C-M-p" 'sp-previous-sexp smartparens-mode-map)

  (bind-key "C-M-k" 'sp-kill-sexp smartparens-mode-map)
  (bind-key "C-M-w" 'sp-copy-sexp smartparens-mode-map)

  (bind-key "C-<backspace>" 'sp-splice-sexp smartparens-mode-map)
  (bind-key "C-)" 'sp-forward-slurp-sexp smartparens-mode-map)
  (bind-key "C-(" 'sp-forward-barf-sexp smartparens-mode-map)

  (bind-key "C-{" 'sp-backward-slurp-sexp smartparens-mode-map)
  (bind-key "C-}" 'sp-backward-barf-sexp smartparens-mode-map)

  (bind-key "C-M-<backspace>" 'sp-splice-sexp-killing-backward smartparens-mode-map)

  (bind-key "C-*" (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "(")) smartparens-mode-map)
  (smartparens-global-mode)
  (show-smartparens-global-mode))
