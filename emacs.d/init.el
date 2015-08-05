(package-initialize)

(require 'cl)

(add-to-list 'load-path "~/dotfiles/emacs.d/lisp/")

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

