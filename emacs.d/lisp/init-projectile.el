;; projectile - https://github.com/bbatsov/projectile for keybindins
(projectile-global-mode)
(require 'projectile)
(helm-projectile-on)
(setq projectile-completion-system 'helm)
(setq projectile-enable-caching nil)

(defun projectile-helm-ag ()
  (interactive)
  (helm-ag (projectile-project-root)))

(provide 'init-projectile)
