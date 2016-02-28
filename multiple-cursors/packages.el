(setq multiple-cursors-packages '(multiple-cursors))
(setq multiple-cursors-excluded-packages '())

(defun hb/configure-mc-isearch ()
  (defvar jc/mc-search--last-term nil)
  (defun jc/mc-search (search-command)
    ;; Read new search term when not repeated command or applying to fake cursors
    (when (and (not mc--executing-command-for-fake-cursor)
               (not (eq last-command 'jc/mc-search-forward))
               (not (eq last-command 'jc/mc-search-backward)))
      (setq jc/mc-search--last-term (read-from-minibuffer "Search: ")))
    (funcall search-command jc/mc-search--last-term))
  (defun jc/mc-search-forward ()
    "Simplified version of forward search that supports multiple cursors"
    (interactive)
    (jc/mc-search 'search-forward))
  (defun jc/mc-search-backward ()
    "Simplified version of backward search that supports multiple cursors"
    (interactive)
    (jc/mc-search 'search-backward)))

(defun hb/configure-mc-bindings ()
  (bind-key "C-S-c C-S-c" 'mc/edit-lines)
  (bind-key "C-S-SPC" 'set-rectangular-region-anchor)
  (bind-key "C->" 'mc/mark-next-like-this)
  (bind-key "C-<" 'mc/mark-previous-like-this)
  (bind-key "C-c C-<" 'mc/mark-all-like-this)
  (bind-key "C-c C->" 'mc/mark-all-like-this-dwim)
  (bind-key "C-c ~" 'mc/insert-numbers)
  (bind-key "M-~" 'mc/sort-regions)
  (bind-key "C-~" 'mc/reverse-regions)
  (bind-key "C-S-c C-e" 'mc/edit-ends-of-lines)
  (bind-key "C-S-c C-a" 'mc/edit-beginnings-of-lines))

(defun hb/configure-mc-fixes ()
  (bind-key "M-SPC" 'just-one-space mc/keymap))

(defun hb/configure-mc ()
  (hb/configure-mc-isearch)
  (hb/configure-mc-bindings)
  (hb/configure-mc-fixes))

(defun multiple-cursors/init-multiple-cursors ()
  (use-package multiple-cursors
    :config (hb/configure-mc))
  (bind-key "C-s" 'jc/mc-search-forward mc/keymap)
  (bind-key "C-r" 'jc/mc-search-backward mc/keymap))
