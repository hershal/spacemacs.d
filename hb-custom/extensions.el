(setq hb-custom-pre-extensions '())
(setq hb-custom-post-extensions '(general))

(defun hb-custom/init-general ()
  (setq vc-follow-symlinks t
        evil-move-beyond-eol t
        kill-whole-line t
        shell-command-switch "-ci"
        tramp-auto-save-directory "~/tmp/autosave-tramp/"
        split-height-threshold nil
        split-width-threshold 180
        search-whitespace-regexp "[ \t\r\n\-]+")

  (bind-key "C-=" 'er/expand-region)
  (bind-key "C-;" 'comment-line-dwim)
  (bind-key "C-x k" 'kill-this-buffer)
  (bind-key "C-x c" 'capitalize-line-dwim)
  (bind-key "C-x 2" 'vsplit-last-buffer)
  (bind-key "C-x 3" 'hsplit-last-buffer)
  (bind-key "C-x C-b" 'spacemacs/alternate-buffer)
  (bind-key "C-x C-K" 'kill-buffer-and-window)
  (bind-key "M-y" 'helm-show-kill-ring)
  (bind-key "C-c C-x C-e" 'fc-eval-and-replace)
  (bind-key "C-x b" 'helm-mini)
  (bind-key "C-x C-f" 'spacemacs/helm-find-files)
  (evil-leader/set-key "fp" 'ffap)
  (evil-leader/set-key "bD" 'kill-buffer-and-window)
  (evil-leader/set-key "fn" 'revert-buffer-noconfirm)
  (evil-leader/set-key "wf" 'follow-delete-other-windows-and-split)

  ;; helm setup
  (setq helm-ff-newfile-prompt-p nil
        helm-buffer-max-length nil)

  (add-hook 'before-save-hook 'delete-trailing-whitespace-untabify))

(defun fc-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun revert-buffer-noconfirm ()
  (interactive)
  (revert-buffer t t))

(defun hb/quotemeta (str-val)
  "Return STR-VAL with all non-word characters and / escaped with backslash.

  This is more vigorous than `shell-quote-argument'."
  (save-match-data
    (replace-regexp-in-string "\\([^A-Za-z_0-9 /]\\)" "\\\\\\1" str-val)))

(defun comment-line-dwim (&optional arg)
    "Replacement for the comment-dwim command.
    If no region is selected and current line is not blank and we
    are not at the end of the line, then comment current line.
    Replaces default behaviour of comment-dwim, when it inserts
    comment at the end of the line."
    (interactive "*P")
    (comment-normalize-vars)
    (if (or (and (not (region-active-p))
                 (not (looking-at "[ \t]*$")))
            (and (not (equal comment-end ""))
                 (looking-at (hb/quotemeta comment-end))))
        (if (looking-at (hb/quotemeta comment-end))
            (progn
              (comment-or-uncomment-region
               (if (comment-beginning)
                   (comment-beginning)
                 (line-beginning-position))
               (line-end-position))
              (delete-trailing-whitespace
               (line-beginning-position) (line-end-position)))
          (comment-or-uncomment-region
           (line-beginning-position) (line-end-position)))
      (comment-dwim arg)))

(defun capitalize-line-dwim ()
  (interactive)
  (if (region-active-p)
      (save-excursion
        (let ((beg (region-beginning))
              (end (region-end)))
          (capitalize-region beg end)))
    (save-excursion
      (let ((beg (line-beginning-position))
            (end (line-end-position)))
        (capitalize-region beg end)))))

(defun delete-trailing-whitespace-untabify ()
  (interactive)
  (if (not (member major-mode makefile-modes))
      (progn
        (delete-trailing-whitespace (point-min) (point-max))
        (untabify (point-min) (point-max)))))

(defun vsplit-last-buffer (prefix)
  "Split the window vertically and display the previous buffer."
  (interactive "p")
  (split-window-vertically)
  (other-window 1 nil)
  (if (= prefix 1) (switch-to-next-buffer)))

(defun hsplit-last-buffer (prefix)
  "Split the window horizontally and display the previous buffer."
  (interactive "p")
  (split-window-horizontally)
  (other-window 1 nil)
  (if (= prefix 1) (switch-to-next-buffer)))
