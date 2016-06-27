(setq hb-custom-packages '(transpose-frame))
(setq hb-custom-excluded-packages '(window-numbering))

(defun hb-custom/init-transpose-frame ()
  (use-package transpose-frame
    :config (bind-key "M-|" 'transpose-frame)))
