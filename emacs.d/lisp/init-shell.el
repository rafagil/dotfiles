;; shell set up
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; zsh
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

(provide 'init-shell)
