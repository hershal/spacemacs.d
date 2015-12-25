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
          ("NEXT" :foreground "sky blue" :weight bold)
          ("DONE" :foreground "forest green" :weight bold :strike-through t)
          ("WAITING" :foreground "orange" :weight bold)
          ("HOLD" :foreground "magenta" :weight bold)
          ("CANCELLED" :foreground "forest green" :weight bold)
          ("MEETING" :foreground "forest green" :weight bold)
          ("PHONE" :foreground "forest green" :weight bold)))
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
          ("r" "respond" entry (file "~/repos/org/refile.org")
           "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :immediate-finish t)
          ("n" "note" entry (file "~/repos/org/refile.org")
           "* %? :NOTE:\n%U\n%a\n")
          ("j" "Journal" entry (file+datetree "~/repos/org/diary.org")
           "* %?\n%U\n")
          ("w" "org-protocol" entry (file "~/repos/org/refile.org")
           "* TODO Review %c\n%U\n" :immediate-finish t)
          ("m" "Meeting" entry (file "~/repos/org/refile.org")
           "* MEETING with %? :MEETING:\n%U")
          ("p" "Phone call" entry (file "~/repos/org/refile.org")
           "* PHONE %? :PHONE:\n%U")
          ("h" "Habit" entry (file "~/repos/org/refile.org")
           "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))

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
        (quote (("N" "Notes" tags "NOTE"
                 ((org-agenda-overriding-header "Notes")
                  (org-tags-match-list-sublevels t)))
                ("h" "Habits" tags-todo "STYLE=\"habit\""
                 ((org-agenda-overriding-header "Habits")
                  (org-agenda-sorting-strategy
                   '(todo-state-down effort-up category-keep))))
                (" " "Agenda"
                 ((agenda "" nil)
                  (tags "REFILE"
                        ((org-agenda-overriding-header "Tasks to Refile")
                         (org-tags-match-list-sublevels nil)))
                  (tags-todo "-CANCELLED/!"
                             ((org-agenda-overriding-header "Stuck Projects")
                              (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  (tags-todo "-HOLD-CANCELLED/!"
                             ((org-agenda-overriding-header "Projects")
                              (org-agenda-skip-function 'bh/skip-non-projects)
                              (org-tags-match-list-sublevels 'indented)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  (tags-todo "-CANCELLED/!NEXT"
                             ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including WAITING and SCHEDULED tasks)")))
                              (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                              (org-tags-match-list-sublevels t)
                              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-sorting-strategy
                               '(todo-state-down effort-up category-keep))))
                  (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                             ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including WAITING and SCHEDULED tasks)")))
                              (org-agenda-skip-function 'bh/skip-non-project-tasks)
                              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                             ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including WAITING and SCHEDULED tasks)")))
                              (org-agenda-skip-function 'bh/skip-project-tasks)
                              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  (tags-todo "-CANCELLED+WAITING|HOLD/!"
                             ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including WAITING and SCHEDULED tasks)")))
                              (org-agenda-skip-function 'bh/skip-non-tasks)
                              (org-tags-match-list-sublevels nil)
                              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
                  (tags "-REFILE/"
                        ((org-agenda-overriding-header "Tasks to Archive")
                         (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                         (org-tags-match-list-sublevels nil))))
                 nil)))))

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
