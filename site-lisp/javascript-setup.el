(defun js2-hooks/remove-broken-tab-complete ()
  (setq tab-always-indent t))

(add-hook 'js2-mode-hook 'js2-hooks/remove-broken-tab-complete)

(provide 'javascript-setup)
