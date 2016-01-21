(setq org-local-packages '(org writegood-mode))

(defun org-local/post-init-org ()
  (setq org-src-window-setup 'other-window
        org-startup-indented t
        org-startup-folded t
        org-hide-emphasis-markers t
        org-fontify-done-headline t
        org-startup-folded 'content
        org-log-into-drawer "LOGBOOK"
        org-use-sub-superscripts '{}
        org-export-with-sub-superscripts nil
        org-src-fontify-natively nil
        org-alphabetical-lists t
        org-use-fast-todo-selection t
        org-agenda-files '("~/repos/org/")
        org-directory "~/repos/org/"
        org-default-notes-file "~/repos/org/refile.org"
        org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))

  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (add-hook 'git-commit-mode-hook 'evil-insert-state)
  (add-hook 'org-clock-out-hook 'remove-empty-drawer-on-clock-out 'append)
  (setq org-structure-template-alist
        '(("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
          ("st" "#+BEGIN_SRC emacs-lisp :tangle yes\n?\n#+END_SRC" "<src lang=\"?\">\n\n</src>")
          ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
          ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
          ("v" "#+begin_verse\n?\n#+end_verse" "<verse>\n?\n</verse>")
          ("c" "#+begin_center\n?\n#+end_center" "<center>\n?\n</center>")
          ("l" "#+begin_latex\n?\n#+end_latex" "<literal style=\"latex\">\n?\n</literal>")
          ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
          ("h" "#+begin_html\n?\n#+end_html" "<literal style=\"html\">\n?\n</literal>")
          ("H" "#+html: " "<literal style=\"html\">?</literal>")
          ("a" "#+begin_ascii\n?\n#+end_ascii")
          ("A" "#+ascii: ")
          ("i" "#+index: ?" "#+index: ?")
          ("I" "#+include %file ?" "<include file=%file markup=\"?\">")))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "red" :weight bold)
          ("REVIEW" :foreground "yellow" :weight bold)
          ("NEXT" :foreground "blue" :weight bold)
          ("DONE" :foreground "forest green" :weight bold :strike-through t)
          ("HOLD" :foreground "magenta" :weight bold)
          ("CANCELLED" :foreground "forest green" :weight bold)
          ("MEETING" :foreground "forest green" :weight bold)))
  (setq org-todo-state-tags-triggers
        '(("CANCELLED" ("CANCELLED" . t))
          ("WAITING" ("WAITING" . t))
          ("HOLD" ("WAITING") ("HOLD" . t))
          (done ("WAITING") ("HOLD"))
          ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
          ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
          ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
  (setq org-capture-templates
        '(("t" "todo" entry (file "~/repos/org/refile.org")
           "* TODO %?\n%U\n%a\n")
          ("j" "Journal" entry (file+datetree "~/repos/org/diary.org")
           "* %?\n%U\n")
          ("w" "org-protocol" entry (file "~/repos/org/refile.org")
           "* REVIEW %c\n%U\n" :immediate-finish t)
          ("m" "Meeting" entry (file "~/repos/org/refile.org")
           "* MEETING with %? :meeting:\n%U")
          ))

  (evil-leader/set-key "oc" 'org-capture)
  (evil-leader/set-key "oa" 'org-agenda)
  (evil-leader/set-key "or" 'org-goto-refile-target)
  (evil-leader/set-key "os" 'org-save)
  (evil-leader/set-key "oi" 'org-insert-link)

  (add-hook 'org-mode-hook 'hb/configure-org-mode-hook)

  ;; org-agenda's turn

  ;; Do not dim blocked tasks
  (setq org-agenda-dim-blocked-tasks nil)

  ;; Compact the block agenda view
  (setq org-agenda-compact-blocks t)

  ;; Custom agenda command definitions
  (setq org-agenda-custom-commands
        '((" " "Agenda"
           ((agenda "" nil)
            (tags "REFILE"
                  ((org-agenda-overriding-header "Entries to Refile")
                   (org-tags-match-list-sublevels nil)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Up")))
            (todo "TODO"
                  ((org-agenda-overriding-header "Task List")
                   (org-agenda-sorting-strategy '(time-up priority-down category-keep))))
            ) nil)))
  (setq org-agenda-tags-column -100
        org-agenda-sorting-strategy
        '((agenda habit-down
                  time-up
                  priority-down
                  user-defined-up
                  effort-up
                  category-keep)
          (todo priority-down category-up effort-up)
          (tags priority-down category-up effort-up)
          (search priority-down category-up))
        ;; Enable display of the time grid so we can see the marker for the
        ;; current time
        org-agenda-time-grid
        '((daily today remove-match)
          #("----------------" 0 16 (org-heading t)) (0900 1100 1300 1500 1700))
        org-agenda-persistent-filter t
        org-agenda-repeating-timestamp-show-all t
        org-agenda-start-on-weekday nil
        org-agenda-sticky t
        org-agenda-span 4
        org-agenda-compact-blocks nil
        org-agenda-show-all-dates t)
  )

(defun org-local/init-writegood-mode ()
  (add-hook 'org-mode-hook 'writegood-mode))

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

(defun remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(defun org-goto-refile-target ()
  (interactive)
  (find-file org-default-notes-file))
