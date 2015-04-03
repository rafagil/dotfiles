(require 'cl)
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

;; me
(setq user-full-name "Channing Walton")

;; keybindings
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Paths
(setq exec-path (append exec-path '("/usr/local/bin/")))

;; recent file size
(setq recentf-max-saved-items 100)

;; Backups
(setq backup-directory-alist '(("." . "~/Dropbox/emacs-backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(yas-reload-all)

;; Spelling
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))
(setq ispell-dictionary "british")
(setq ispell-personal-dictionary "~/dotfiles/dictionary.txt")

;; Marking text
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; Display Settings
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; Indentation
(setq tab-width 2
      indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

;; hide backup files etc
(setq dired-omit-mode t)

;; yes and no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Keybindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)

;; Misc
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

;; Smex
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Ido
;;(ido-mode t)
;;(setq ido-enable-flex-matching t
;;      ido-use-virtual-buffers t)

;; autopair
(require 'autopair)

;; zsh
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

;; conf mode
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))

;; projectile - https://github.com/bbatsov/projectile for keybindins
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(setq projectile-enable-caching t)
(global-set-key (kbd "M-p") 'helm-projectile)

(defun projectile-helm-ag ()
  (interactive)
  (helm-ag (projectile-project-root)))

;; org-mode
;; For exporting to latex and pdf do
;; $ brew install caskroom/cask/brew-cask
;; $ brew cask install mactex
(setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))
(require 'org)
(require 'org-install)
(require 'org-habit)
(add-to-list 'org-modules "org-habit")
(setq org-habit-preceding-days 7
      org-habit-following-days 1
      org-habit-graph-column 80
      org-habit-show-habits-only-for-today t
      org-habit-show-all-today t)

(setq org-directory "~/Dropbox/org")
(add-hook 'org-mode-hook (lambda () (writegood-mode)))

;; Clocking
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; capture
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat org-directory "/todo.org") "Tasks")
         "* TODO [#B] %U %?\n  %i\n  %a")

        ("T" "Todo (Important)" entry (file+headline (concat org-directory "/todo.org") "Tasks")
         "* TODO [#A] %U %?\n  %i\n  %a")

        ("h" "HowTo" entry (file+headline (concat org-directory "/howto.org") "How To")
         "* %^{What?} :%^{Tag}:\n %?\n")

        ("m" "Fixme" entry (file+headline (concat org-directory "/todo.org") "Tasks")
         "* FIXME [#C] %U %?\n  %i\n  %a")

        ("j" "Personal Journal" entry (file+datetree (concat org-directory "/journal.org"))
         "* %U\n\n%?\n")

        ("b" "Boost Journal" entry (file+datetree (concat org-directory "/projects/boost/journal.org"))
         "* %U\n\n%?\n")

        ("f" "Foggyball Journal" entry (file+datetree (concat org-directory "/projects/foggyball/journal.org"))
         "* %U\n\n%?\n")

        ("g" "Glossary" entry (file+headline (concat org-directory "/projects/boost/glossary.org") "Glossary")
         "* %^{Term} :%^{Tag}:\n %?\n")
        ))

(setq org-agenda-files (list org-directory
                             (concat org-directory "/projects/boost")
                             (concat org-directory "/projects/foggyball")))

(setq org-startup-indented t)
(setq org-ellipsis " \u25bc" )
(setq org-completion-use-ido t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d!)" "NO NEED(n@/!)")
        (sequence "BUG(b)" "FIXME(f)" "|" "FIXED(f!)" "WON'T FIX(w@/!)" "DELEGATED(l@/!)")
        (sequence "QUESTION(q)" "|" "ANSWERED(a!)")
        (sequence "|" "CANCELED(c!)")))

;; This from http://orgmode.org/worg/org-faq.html
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(add-hook 'org-mode-hook 'turn-on-flyspell)

;; Babel for diagrams etc
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (plantuml   . t)
   (ditaa      . t)
   (java       . t)
   (js         . t)
   (scala      . t)
   (sh         . t)
   (sql        . t)
   (dot        . t)))

;; don't prompt when evaluating various babel files
(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "plantuml")))  ; don't ask for ditaa
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; Setup path to ditaa.jar after brew install ditaa
(setq org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.9/libexec/ditaa0_9.jar")

(setq org-plantuml-jar-path
      (expand-file-name "~/.emacs.d/plantuml/plantuml.jar"))

(setq org-image-actual-width nil)

;; latex
(require 'ox-latex)

;; publishing
(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ("org-notes"
         :base-directory "~/Dropbox/org/"
         :base-extension "org"
         :publishing-directory "~/public_html/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4             ; Just the default for this project.
         :auto-preamble t
         )

        ("org-static"
         :base-directory "~/Dropbox/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf|pptx\\|ppt\\|doc\\|docx\\|"
         :publishing-directory "~/public_html/"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("org" :components ("org-notes" "org-static"))
        ))

(setq org-publish-use-timestamps-flag nil)

;; configure sr-speedbar
(setq speedbar-use-images nil)
(setq sr-speedbar-right-side nil)

;; Set up fonts
(setq pretty-symbol-categories '(lambda relational logical))

;; Scala and Ensime
;;(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode 'pretty-symbols-mode)

;; see https://github.com/folone/emacs-scalaz-unicode-input-method
(add-to-list 'load-path "~/Code/dev/emacs-scalaz-unicode-input-method")
(require 'scalaz-unicode-input-method)
(add-hook 'scala-mode-hook
          (lambda () (set-input-method "scalaz-unicode")))

(setq exec-path (append exec-path '("~/bin/")))

(add-hook 'scala-mode-hook (lambda () (setq truncate-lines t)))
(add-hook 'scala-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(electric-pair-mode 1)

(column-number-mode 1)

;; Allow hash to be entered
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

;; Show line numbers
(setq linum-format "%4d ")
(global-linum-mode 1)
(setq cursor-type (quote bar))
(modify-all-frames-parameters (list (cons 'cursor-type 'bar)))

;; scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; show the time
(display-time-mode 1)

;; make emacs split buffers horizontally not vertically
;; see http://stackoverflow.com/questions/2081577/setting-emacs-split-to-horizontal
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; Helm
(require 'helm-config)
(helm-mode 1)
(helm-autoresize-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)

(require 'helm-projectile)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; helm ag
;; brew install ag
(global-set-key (kbd "M-s s")   #'helm-ag)

;; helm keybindings
(global-set-key (kbd "C-x b")   #'helm-mini)
(global-set-key (kbd "C-x C-b") #'helm-buffers-list)
(global-set-key (kbd "C-x C-m") #'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-x C-r") #'helm-recentf)
(global-set-key (kbd "C-x r l") #'helm-filtered-bookmarks)
(global-set-key (kbd "M-y")     #'helm-show-kill-ring)
(global-set-key (kbd "M-s o")   #'helm-swoop)
(global-set-key (kbd "M-s /")   #'helm-multi-swoop)

;; Cleaning things up - http://www.aaronbedra.com/emacs.d/
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun cleanup-region (beg end)
  "Remove tmux artifacts from region."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
    (replace-regexp re "" nil beg end)))

(global-set-key (kbd "C-x M-t") 'cleanup-region)
(global-set-key (kbd "C-c f") 'cleanup-buffer)

(setq-default show-trailing-whitespace t)

;; guide-key
(require 'guide-key)
(setq guide-key/guide-key-sequence t)
(guide-key-mode 1)

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

;; UI Theme
(load-theme 'sanityinc-tomorrow-night t)

(set-face-attribute 'default nil :font  "Source Code Pro" )
(set-face-attribute 'default nil :height 110)

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
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "a041a61c0387c57bb65150f002862ebcfe41135a3e3425268de24200b82d6ec9" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "7bde52fdac7ac54d00f3d4c559f2f7aa899311655e7eb20ec5491f3b5c533fe8" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "e80932ca56b0f109f8545576531d3fc79487ca35a9a9693b62bf30d6d08c9aaf" default)))
 '(dired-listing-switches "-al")
 '(display-time-mode t)
 '(frame-background-mode (quote dark))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
