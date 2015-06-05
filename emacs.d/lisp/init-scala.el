;; Scala and Ensime
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(setq pretty-symbol-categories '(lambda relational logical))
(add-hook 'scala-mode 'pretty-symbols-mode)

;; see https://github.com/folone/emacs-scalaz-unicode-input-method
(add-to-list 'load-path "~/Code/dev/emacs-scalaz-unicode-input-method")
(require 'scalaz-unicode-input-method)
(add-hook 'scala-mode-hook
          (lambda () (set-input-method "scalaz-unicode")))

(add-hook 'scala-mode-hook (lambda () (setq truncate-lines t)))
(add-hook 'scala-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-hook 'scala-mode-hook (lambda () (setq show-trailing-whitespace t)))

(provide 'init-scala)
