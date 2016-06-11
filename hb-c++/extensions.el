(setq hb-c++-pre-extensions '())
(setq hb-c++-post-extensions '(hooks))

(defun hb-c++/init-hooks ()
  (add-hook 'c-mode-common-hook 'hb-c++/common-hook))

(defun hb-c++/common-hook ()
  (set (make-local-variable 'compile-command)
       (concat "g++ -std=c++11 -Wall " buffer-file-name " && ./a.out"))
  (setq company-clang-arguments '("-std=c++11"))
  (setq flycheck-clang-language-standard "c++11")
  (setq comment-start "/*"
        comment-end "*/"
        c-basic-offset 4)
  (c-set-offset 'arglist-cont-nonempty '+))
