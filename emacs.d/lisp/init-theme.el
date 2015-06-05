;; UI Theme
(add-to-list 'custom-theme-load-path (expand-file-name "lisp" user-emacs-directory))
;;(load-theme 'sanityinc-tomorrow-night t)

(load-theme 'modern-classic-dark t)

(set-face-attribute 'default nil :font  "CMU Typewriter Text Light" )
(set-face-attribute 'default nil :height 120)

(provide 'init-theme)
