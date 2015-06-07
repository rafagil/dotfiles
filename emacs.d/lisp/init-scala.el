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

(defun scala-mode-intellij-keys ()
  "Modify keymaps used by `scala-mode'."
  (local-set-key (kbd "s-n")    'ensime-search)
  (local-set-key (kbd "s-<f9>") 'ensime-typecheck-all)
  (local-set-key (kbd "M-s-ø")  'ensime-refactor-organize-imports)
  (local-set-key (kbd "M-s-¬")  'ensime-format-source)
  (local-set-key (kbd "S-<f6>") 'ensime-refactor-rename)
  (local-set-key (kbd "M-s-√")  'ensime-refactor-extract-local)
  (local-set-key (kbd "M-s-µ")  'ensime-refactor-extract-method)
  (local-set-key (kbd "M-s-~")  'ensime-refactor-inline-local)
  (local-set-key (kbd "s-b")    'ensime-edit-definition)
  )

(add-hook 'scala-mode-hook 'scala-mode-intellij-keys)

(provide 'init-scala)
