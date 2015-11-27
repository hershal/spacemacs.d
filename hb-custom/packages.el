(setq hb-custom-packages '(transpose-frame))

(defun hb-custom/init-transpose-frame ()
  (use-package transpose-frame
    :config (bind-key "M-|" 'transpose-frame)))
