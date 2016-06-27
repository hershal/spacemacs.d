
(defun hb/init ()
  (hb/introduction)
  (hb/os-specific)
  (hb/spacemacs)
  (hb/internal)
  (hb/external)
  (hb/conclusion))

(defun hb/introduction ()
  (hb/introduction/who-am-i))

(defun hb/introduction/who-am-i ()
  (setq user-full-name "Hershal Bhave"))

(defun hb/os-specific ())

(defun hb/spacemacs ())

(defun hb/internal ()
  (hb/internal/org))

(defun hb/internal/org ()
  (with-eval-after-load 'org
    (add-to-list 'org-structure-template-alist
                 '("st"
                   "#+BEGIN_SRC emacs-lisp :tangle yes\n?\n#+END_SRC"
                   "<src lang=\"?\">\n\n</src>"))))

(defun hb/external())

(defun hb/conclusion ()
  (find-file "~/.spacemacs.d/init-new.org"))

(provide 'init-new)
