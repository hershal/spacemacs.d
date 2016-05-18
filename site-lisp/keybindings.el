(bind-key "C-h C-F" 'find-function)
(bind-key "M-x" 'execute-extended-command)

(require 'helm-files)
(bind-key "C-h" 'helm-find-files-up-one-level helm-find-files-map)
(bind-key "C-l" 'helm-execute-persistent-action helm-find-files-map)

(provide 'keybindings)
