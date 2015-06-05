;; Start with agenda
(setq inhibit-splash-screen t)
(add-hook 'after-init-hook (lambda () (org-agenda nil "n")))
(setq org-agenda-window-setup 'current-window)

(provide 'post-init)
