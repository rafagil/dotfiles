(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; Indentation
(setq tab-width 2
      indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

(show-paren-mode t)

(provide 'init-formatting)
