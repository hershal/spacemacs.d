(setq org-projectile-packages '(org-projectile))
(setq org-projectile-excluded-packages '())

(defun org-projectile/init-org-projectile ()
  (use-package org-projectile
    :ensure t
    :config
    (setq org-projectile:projects-file "~/repos/org/projects.org"
          org-agenda-files (append org-agenda-files (org-projectile:todo-files)))
    (evil-leader/set-key "op" 'org-projectile:project-todo-completing-read)))
