(evil-define-key `normal scala-mode-map (kbd "s-o")    'ensime-search)
(evil-define-key `normal scala-mode-map (kbd "s-<f9>") 'ensime-typecheck-all)
(evil-define-key `normal scala-mode-map (kbd "M-s-ø")  'ensime-refactor-organize-imports)
(evil-define-key `normal scala-mode-map (kbd "M-s-l")  'ensime-format-source)
(evil-define-key `normal scala-mode-map (kbd "S-<f6>") 'ensime-refactor-rename)
(evil-define-key `normal scala-mode-map (kbd "M-s-√")  'ensime-refactor-extract-local)
(evil-define-key `normal scala-mode-map (kbd "M-s-µ")  'ensime-refactor-extract-method)
(evil-define-key `normal scala-mode-map (kbd "M-s-n")  'ensime-refactor-inline-local)
(evil-define-key `normal scala-mode-map (kbd "s-b")    'ensime-edit-definition)
(evil-define-key `normal scala-mode-map (kbd "s-w")    'ensime-expand-selection-command)
(evil-define-key `normal scala-mode-map (kbd "M-<f7>") 'ensime-show-uses-of-symbol-at-point)
