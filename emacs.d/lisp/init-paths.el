;; Paths
(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))

(setq exec-path (append exec-path '("/usr/local/bin/")))
(setq exec-path (append exec-path '("/usr/texbin/")))
(setq exec-path (append exec-path '("~/bin/")))

(provide 'init-paths)
