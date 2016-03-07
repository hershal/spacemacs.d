(defconst hb-latex-packages '(auctex))

(defun hb-latex/post-init-auctex ()
  (spacemacs/set-leader-keys-for-major-mode 'latex-mode
    "l" 'hb-latex/helm-label))

(defun hb-latex/helm-label ()
  (interactive)
  (helm :sources (helm-build-sync-source "latex-labels"
                   :candidates (latex-enumerate-labels)
                   :action '(lambda (candidate) (insert "\\label{" candidate "}"))
                   :fuzzy-match t)
        :buffer "*helm labels-complete*"))

(defun hb-latex/enumerate-labels ()
  (with-current-buffer (current-buffer)
    (save-excursion
      (save-restriction
        (widen)
        (mapcar 'cadr
                (s-match-strings-all
                 (concat "\\\\label{\\(" LaTeX-equation-label hb-latex/label-regexp "\\)}")
                 (buffer-substring-no-properties (point-min) (point-max))))))))
