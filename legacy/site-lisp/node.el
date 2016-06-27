(defconst node-eval-buffer "*node.js*")
(defun u/node-eval ()
  "Evaluate the current buffer (or region if mark-active),
   and return the result into another buffer,
   which is to be shown in a window."
  (interactive)
  (when (get-buffer node-eval-buffer)
    (with-current-buffer node-eval-buffer
      (delete-region (point-min) (point-max))
      ))
  (let ((debug-on-error t) (start 1) (end 1))
    (cond
     (mark-active
      (setq start (point))
      (setq end (mark)))
     (t
      (setq start (point-min))
      (setq end (point-max))))
    (call-process-region
     start end     ; seems the order does not matter
     "node"        ; node.js
     nil           ; don't delete region
     node-eval-buffer     ; output buffer
     nil)          ; no redisply during output
    (let ((contents ""))
      (save-window-excursion
        (switch-to-buffer node-eval-buffer)
        (goto-char (point-max))
        (forward-line -1)
        (message (buffer-substring (point-min) (point-at-eol)))
        ))
    (setq deactivate-mark t)))

(defun u/node-configure ()
  (bind-key "C-x C-n" 'u/node-eval js2-mode-map)
  (spacemacs/set-leader-keys-for-major-mode 'js2-mode "n" 'u/node-eval))

(add-hook 'js2-mode-hook 'u/node-configure)

(provide 'node)
