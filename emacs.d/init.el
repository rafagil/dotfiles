;;  This was shamelessly stolen from https://github.com/abedra/emacs.d/blob/master/abedra.org
;; packages
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
			    darcula-theme
			    dash
			    ensime
			    epl
			    expand-region
			    f
			    furl
			    git-commit-mode
			    git-rebase-mode
			    haskell-mode
			    htmlize
			    magit
			    markdown-mode
			    marmalade
			    org
			    org-journal
			    org-plus-contrib
			    paredit
			    popup
			    projectile
			    s
			    sbt-mode
			    scala-mode2
			    sr-speedbar
			    smex
			    tabbar
			    tabbar-ruler
			    web-mode
			    writegood-mode
			    yaml-mode)
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

;; Paths
(setq exec-path (append exec-path '("/usr/local/bin/")))

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
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

;; autopair
(require 'autopair)

;; zsh
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

;; conf mode
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))

;; projectile - https://github.com/bbatsov/projectile for keybindins
(projectile-global-mode)
(setq projectile-enable-caching t)

;; tabbar
(setq tabbar-ruler-global-tabbar t) ; If you want tabbar
(setq tabbar-ruler-global-ruler t) ; if you want a global ruler
(setq tabbar-ruler-popup-menu t) ; If you want a popup menu.
(setq tabbar-ruler-popup-toolbar t) ; If you want a popup toolbar
(setq tabbar-ruler-popup-scrollbar t) ; If you want to only show the
                                        ; scroll bar when your mouse is moving.
(require 'tabbar)
;; Enable tabbars globally:
(tabbar-mode 1)

;; org-mode and org-journal
;; For exporting to latex and pdf do
;; $ brew install caskroom/cask/brew-cask
;; $ brew cask install mactex
(setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))
(require 'org)
(setq user-full-name "Channing Walton")
(setq org-directory "~/Dropbox/org/")
(setq org-journal-dir "~/Dropbox/org/journal/")
(setq org-agenda-files (list "~/Dropbox/org"
                             "~/Dropbox/org/projects/boost" 
                             "~/Dropbox/org/journal"))

(setq org-journal-file-format "%Y%m%d.org")
(setq org-log-done 'time)
(setq org-startup-indented t)
(setq org-ellipsis " \u25bc" )
(setq org-completion-use-ido t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
	(sequence "BUG(b)" "|" "FIXED(f)" "DELEGATED(d)")
	(sequence "QUESTION(q)" "|" "ANSWERED(a)")
	(sequence "|" "CANCELED(c)")))

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

;; Setup path to ditaa.jar after brew install ditaa
(setq org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.9/libexec/ditaa0_9.jar")

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
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
	 :publishing-directory "~/public_html/"
	 :recursive t
	 :publishing-function org-publish-attachment
	 )
	
	("org" :components ("org-notes" "org-static"))
	))

;; Theme
(if window-system
    (require 'darcula-theme)
  (load-theme 'wombat t))

(set-background-color "black")

;; Set up fonts
 (defun my-variable-face-mode ()
   "Set font to a variable width (proportional) fonts in current buffer"
   (interactive)
   (setq buffer-face-mode-face '(:family "Ariel"))
   (buffer-face-mode))

 ;; Use monospaced font faces in current buffer
 (defun my-fixed-face-mode ()
   "Sets a fixed width (monospace) font in current buffer"
   (interactive)
   (setq buffer-face-mode-face '(:family "Input Mono Narrow"))
   (buffer-face-mode))

 ;; Set default font faces for Info and ERC modes
 (add-hook 'org-mode-hook 'my-variable-face-mode)
 (add-hook 'scala-mode-hook 'my-fixed-face-mode)

(setq pretty-symbol-categories '(lambda relational logical))

;; Scala and Ensime
;;(require 'ensime)
;;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode 'pretty-symbols-mode)

;; see https://github.com/folone/emacs-scalaz-unicode-input-method
(add-to-list 'load-path "~/Code/dev/emacs-scalaz-unicode-input-method")
(require 'scalaz-unicode-input-method)
(add-hook 'scala-mode-hook 
  (lambda () (set-input-method "scalaz-unicode")))

(setq exec-path (append exec-path '("~/bin/")))

(add-hook 'scala-mode-hook (lambda () (setq truncate-lines t)))

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(electric-pair-mode 1)

(column-number-mode 1)

;; Allow hash to be entered  
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

;; keybindings
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; make emacs split buffers horizontally not vertically
;; see http://stackoverflow.com/questions/2081577/setting-emacs-split-to-horizontal
(setq split-height-threshold nil)
(setq split-width-threshold 0)
