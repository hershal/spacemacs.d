;;; packages.el --- org-projectile Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq org-projectile-packages
      '(org-projectile))

;; List of packages to exclude.
(setq org-projectile-excluded-packages '())

(defun org-projectile/init-org-projectile ()
  (use-package org-projectile
    :ensure t
    :config
    (setq org-projectile:projects-file "~/repos/org/projects.org"
          org-agenda-files (append org-agenda-files (org-projectile:todo-files)))
    (evil-leader/set-key "op" 'org-projectile:project-todo-completing-read)))

;; For each package, define a function org-projectile/init-<package-name>
;;
;; (defun org-projectile/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
