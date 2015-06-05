;; shell set up
(setenv "PATH" (shell-command-to-string "/bin/bash -c 'echo -n $PATH'"))

;; zsh
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

(provide 'init-shell)
