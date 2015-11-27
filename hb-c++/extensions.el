(setq hb-c++-pre-extensions '())

(setq hb-c++-post-extensions '(hooks))

(defun hb-c++/init-hooks ()
  (add-hook 'c-mode-common-hook 'hb-c++/common-hook))

(defun hb-c++/common-hook ()
  (setq comment-start "/*"
        comment-end "*/"
        c-basic-offset 4))
