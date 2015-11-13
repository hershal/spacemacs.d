(setq org-local-packages '(org))

(defun org-local/post-init-org ()
  (setq org-src-window-setup 'other-window
        org-startup-indented t
        org-hide-emphasis-markers t
        org-startup-folded 'content
        org-log-into-drawer "LOGBOOK"
        org-use-sub-superscripts '{}
        org-export-with-sub-superscripts nil
        org-src-fontify-natively nil
        org-refile-targets '((nil :maxlevel . 3)
                             (org-agenda-files :maxlevel . 3)))
  (after 'org-indent
    (diminish 'org-indent-mode ""))
  (bind-key "C-x C-s" 'org-save org-mode-map)
  (add-to-list 'org-structure-template-alist
               '("st"
                 "#+BEGIN_SRC emacs-lisp :tangle yes\n?\n#+END_SRC"
                 "<src lang=\"?\">\n\n</src>"))
  (bind-key "C-c c" 'org-capture)
  (setq org-capture-templates

        '(("n" "Notes" entry (file "~/notes.org") "* %?\n")

          ("s" "Stack" entry (file "~/stack.org") "\n* TODO %?\n")
          ("w" "Weektree" entry (file+function "~/.emacs.d/test.org" org-capture-datetree)
           "* %?\n")))
  (bind-key "C-c a" 'org-agenda)
  (add-hook 'org-mode-hook 'hb/configure-org-mode-hook)
  t)

(defun hb/configure-org-mode-hook ()
  (sp-local-pair 'org-mode "/" "/" :unless '(sp-point-after-word-p))
  (sp-local-pair 'org-mode "_" "_" :unless '(sp-point-after-word-p))
  (sp-local-pair 'org-mode "=" "=" :unless '(sp-point-after-word-p))
  (auto-fill-mode 1)
  (company-mode 0))

(defun org-save ()
  (interactive)
  (org-update-statistics-cookies t)
  (org-babel-tangle))

