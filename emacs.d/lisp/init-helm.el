;; Helm
(require 'helm-config)
(helm-mode 1)
(helm-autoresize-mode 1)

(require 'helm-projectile)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(provide 'init-helm)
