
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cl)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-packages)
(require 'init-personal)
(require 'init-shell)
(require 'init-paths)
(require 'init-file-management)
(require 'init-yas)
(require 'init-spelling)
(require 'init-display)
(require 'init-formatting)
(require 'init-smex)
(require 'init-git)
(require 'init-projectile)
(require 'init-org-mode)
(require 'init-speedbar)
(require 'init-scala)
(require 'init-helm)
(require 'init-cleanup)
(require 'init-javascript)
(require 'init-markdown)
(require 'init-theme)
;; needs to be last to have various functions available
(require 'init-keyboard)

(require 'post-init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (git-commit-mode yaml-mode writegood-mode web-mode tabbar-ruler sr-speedbar smex rainbow-mode paredit org-plus-contrib org marmalade markdown-mode magit ibuffer-vc htmlize helm-swoop helm-projectile helm-ag haskell-mode guide-key f expand-region exec-path-from-shell ergoemacs-mode ensime darcula-theme color-theme-sanityinc-tomorrow autopair))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
