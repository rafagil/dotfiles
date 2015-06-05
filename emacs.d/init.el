(require 'cl)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "lisp" user-emacs-directory))

(load "package")
(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(setq package-archive-enable-alist '(("melpa" deft magit)))

(defvar channing/packages '(auto-complete
                            autopair
                            company
                            dash
                            ensime
                            epl
                            exec-path-from-shell
                            expand-region
                            f
                            furl
                            git-commit-mode
                            git-rebase-mode
                            guide-key
                            haskell-mode
                            helm
                            helm-ag
                            helm-projectile
                            helm-swoop
                            htmlize
                            magit
                            markdown-mode
                            marmalade
                            org
                            org-plus-contrib
                            paredit
                            popup
                            projectile
                            rainbow-mode
                            s
                            color-theme-sanityinc-tomorrow
                            sbt-mode
                            scala-mode2
                            sr-speedbar
                            smex
                            web-mode
                            writegood-mode
                            yaml-mode
                            yasnippet)
  "Default packages")

(defun channing/packages-installed-p ()
  (loop for pkg in channing/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (channing/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg channing/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

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

;; needs to be last to have various functions available
(require 'init-keyboard)

;; Javascript
(defun js-custom ()
  "js-mode-hook"
  (setq js-indent-level 2))

(add-hook 'js-mode-hook 'js-custom)

;; Markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode t)
            (writegood-mode t)
            (flyspell-mode t)))
(setq markdown-command "pandoc --smart -f markdown -t html")
(setq markdown-css-path "http://thomasf.github.io/solarized-css/solarized-light.min.css" )

;; Magit
(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")

;; UI Theme
;;(load-theme 'sanityinc-tomorrow-night t)

(load-theme 'modern-classic-dark t)

(set-face-attribute 'default nil :font  "CMU Typewriter Text Light" )
(set-face-attribute 'default nil :height 120)

;; OSX Keybindings
(global-set-key (kbd "<s-right>") 'move-end-of-line)
(global-set-key (kbd "<s-left>") 'move-beginning-of-line)
(global-set-key (kbd "s-b") 'helm-buffers-list)
(global-set-key (kbd "<s-up>") 'beginning-of-buffer)
(global-set-key (kbd "<s-down>") 'end-of-buffer)

;; Other Keybindings
(global-set-key (kbd "TAB" ) 'smart-tab)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "s-/") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(global-set-key (kbd "M-p") 'helm-projectile-switch-project)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

(global-set-key (kbd "M-x")      'helm-M-x)
(global-set-key (kbd "M-s s")   #'helm-ag)
(global-set-key (kbd "C-x b")   #'helm-mini)
(global-set-key (kbd "C-x C-b") #'helm-buffers-list)
(global-set-key (kbd "C-x C-m") #'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-x C-r") #'helm-recentf)
(global-set-key (kbd "C-x r l") #'helm-filtered-bookmarks)
(global-set-key (kbd "M-y")     #'helm-show-kill-ring)
(global-set-key (kbd "M-s o")   #'helm-swoop)
(global-set-key (kbd "M-s /")   #'helm-multi-swoop)

(global-set-key (kbd "C-x M-t") 'cleanup-region)
(global-set-key (kbd "C-c f") 'cleanup-buffer)

;; Start with agenda
(setq inhibit-splash-screen t)
(add-hook 'after-init-hook (lambda () (org-agenda nil "n")))
(setq org-agenda-window-setup 'current-window)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("b82a6fdc1f0a77f72b2b27437399051fb95c22c50b75571841d21b5ce22535f9" "51472d455204ae998ea0be03d21aa6d3f260983364aa75ae54a614604eef2dd4" "0f9ce726036ad2e49642eb6f1f2c532eae4eee74a11558bb593e99e828cae167" "41e9e014f545d246d9974978f9e542904c40d6d71a57a45dbe244035ba57ea06" "b10feb29e0ba2bf06fbf05bc9dea40a56c31f3ba99ef77da33fc8b7fe4965387" "d812d7e91ce9a896183300c8faee32513a14dde95fda8c59571fe389294ff81c" "5d46b06933737e5d02efe8d6e25576b967cfcbeea64a0b21508cd0864780d911" "d7cc15abed0a5b3aa8dd02ef69466fdacd52a08c867168cc94f0624bf52a0217" "56c7920158466a6ad7b5ac8d10376d747714d5517f29f72e7c133e03396b5b11" "4d2c5dc1f06ca209155ea92d0192a9cb621fa0059987fc8d42ade58d6e8e5f01" "c77bb19f6a30329b5e1066f622f3d38b766bc12e3d2640f1ac5bb6b342477129" "0eeea14fa20b96b0a39bc0951d3a157186da47bbaf4e6a3d7b228bab619cfb6b" "6d5ad253382d680052a6293ef17929d85a840db275bbbaa79226cb0c8ae9df7f" "0c03dbde145ca1a81b43a2219a006cf51dbf524058bc89c4c648915bd4f95dc2" "523f99feb0a686ed46eea16d376228a085084732d1c10f785c6486128546e1b2" "147a506e5d668e6b058e6826bf65de0f8df1a2d081568a870d1b145b72eefc80" "603a9c7f3ca3253cb68584cb26c408afcf4e674d7db86badcfe649dd3c538656" "40bc0ac47a9bd5b8db7304f8ef628d71e2798135935eb450483db0dbbfff8b11" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "a041a61c0387c57bb65150f002862ebcfe41135a3e3425268de24200b82d6ec9" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "7bde52fdac7ac54d00f3d4c559f2f7aa899311655e7eb20ec5491f3b5c533fe8" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "e80932ca56b0f109f8545576531d3fc79487ca35a9a9693b62bf30d6d08c9aaf" default)))
 '(dired-listing-switches "-al")
 '(display-time-mode t)
 '(ensime-sem-high-faces
   (quote
    ((var :foreground "#eaeaea" :underline
          (:style wave :color "yellow"))
     (val :foreground "#eaeaea")
     (varField :slant italic)
     (valField :foreground "#eaeaea" :slant italic)
     (functionCall :foreground "#eaeaea")
     (operator :foreground "#a5a5a5")
     (param :foreground "#eaeaea")
     (class :foreground "#eaeaea")
     (trait :foreground "#eaeaea" :slant italic)
     (object :foreground "#eaeaea" :slant italic)
     (package :foreground "#eaeaea"))))
 '(frame-background-mode (quote dark))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:foreground "#de935f" :slant italic :weight bold :height 1.44))))
 '(org-done ((t (:foreground "light green"))))
 '(org-level-1 ((t (:inherit outline-1 :slant italic :height 1.4))))
 '(org-level-2 ((t (:inherit outline-2 :slant italic :height 1.2)))))
