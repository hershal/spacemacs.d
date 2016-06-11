(defun js2-hooks/remove-broken-tab-complete ()
  (setq tab-always-indent t))

(add-hook 'js2-mode-hook 'js2-hooks/remove-broken-tab-complete)

(define-skeleton js-skeleton "Javascript skeleton" nil "'use strict';" \n \n -)

(define-auto-insert '("\\.\\(js\\|ts\\)" . "Javascript skeleton")
  'js-skeleton)

(provide 'js-setup)
