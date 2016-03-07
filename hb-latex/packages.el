(defconst hb-latex-packages '(auctex))

(defun hb-latex/post-init-auctex ()
  (spacemacs/set-leader-keys-for-major-mode 'latex-mode
    "l" 'hb-latex/insert-label))

(defun hb-latex/insert-label ()
  (interactive)
  (let* ((ref (completing-read
               "LaTeX-labels"
               (hb-latex/enumerate-labels) nil nil nil nil nil)))
    (insert ref)))

(defun hb-latex/enumerate-labels ()
  (with-current-buffer (current-buffer)
    (save-excursion
      (save-restriction
        (widen)
        (mapcar '(lambda (candidate) (list (cadr candidate)))
                (s-match-strings-all
                 (concat "\\\\label{\\(" hb-latex/label-regexp "\\)}")
                 (buffer-substring-no-properties (point-min) (point-max))))))))
