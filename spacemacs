;; -*- mode: dotspacemacs -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/dotfiles/spacemacs-config/")
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; --------------------------------------------------------
     ;; Example of useful layers you may want to use right away
     ;; Uncomment a layer name and press C-c C-c to install it
     ;; --------------------------------------------------------
     auto-completion
     ;; better-defaults
     (git :variables
           git-gutter-use-fringe t)
     markdown
     org
     osx
     syntax-checking
     scala
     themes-megapack
     channing
     )
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'emacs
   ;; If non nil output loading progess in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to a .PNG file.
   ;; If the value is nil then no banner is displayed.
   ;; dotspacemacs-startup-banner 'official
   dotspacemacs-startup-banner 'official
   ;; t if you always want to see the changelog at startup
   dotspacemacs-always-show-changelog t
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(solarized-light
                         solarized-dark
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("CMU Typewriter Text Light"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil the paste micro-state is enabled. While enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state t
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   )
  ;; User initialization goes here
  ;; me
  (setq user-full-name "Channing Walton")

  (global-auto-revert-mode t)

  ;; Paths
  (setq exec-path (append exec-path '("/usr/local/bin/")))
  (setq exec-path (append exec-path '("/usr/texbin/")))

  ;; yas
  ;;(add-to-list 'yas-snippet-dirs "~/dotfiles/snippets/")
  ;;(yas/initialize)

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

;; Spelling
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))
(setq ispell-dictionary "british")
(setq ispell-personal-dictionary "~/dotfiles/dictionary.txt")

;; hide backup files etc
(setq dired-omit-mode t)

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

;; Clocking
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; capture
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat org-directory "/todo.org") "To do")
         "* TODO [#B] %U %?\n  %i\n  %a")

        ("T" "Todo (Important)" entry (file+headline (concat org-directory "/todo.org") "To do")
         "* TODO [#A] %U %?\n  %i\n  %a")

        ("h" "HowTo" entry (file+headline (concat org-directory "/howto.org") "How To")
         "* %^{What?} :%^{Tag}:\n %?\n")

        ("m" "Fixme" entry (file+headline (concat org-directory "/todo.org") "To do")
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

(setq org-refile-targets '((org-agenda-files :maxlevel . 9)))
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-use-outline-path 'file)

;; From http://emacs.stackexchange.com/a/10762/5463
(defun org-refile-to-datetree (&optional file)
  "Refile a subtree to a datetree corresponding to it's timestamp.

The current time is used if the entry has no timestamp. If FILE
is nil, refile in the current file."
  (interactive "f")
  (let* ((datetree-date (or (org-entry-get nil "TIMESTAMP" t)
                            (org-read-date t nil "now")))
         (date (org-date-to-gregorian datetree-date))
         )
    (save-excursion
      (with-current-buffer (current-buffer)
        (org-cut-subtree)
        (if file (find-file file))
        (org-datetree-find-date-create date)
        (org-narrow-to-subtree)
        (show-subtree)
        (org-end-of-subtree t)
        (newline)
        (goto-char (point-max))
        (org-paste-subtree 4)
        (widen)
        ))
    )
  )

;; This from http://orgmode.org/worg/org-faq.html
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

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
      (expand-file-name "~/dotfiles/plantuml/plantuml.jar"))

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

;; see https://github.com/folone/emacs-scalaz-unicode-input-method
(add-to-list 'load-path "~/Code/dev/emacs-scalaz-unicode-input-method")
(require 'scalaz-unicode-input-method)
(add-hook 'scala-mode-hook
          (lambda () (set-input-method "scalaz-unicode")))

(setq exec-path (append exec-path '("~/bin/")))

(add-hook 'scala-mode-hook (lambda () (setq truncate-lines t)))
(add-hook 'scala-mode-hook (lambda () (setq indent-tabs-mode nil)))

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
            (flyspell-mode t)))
(setq markdown-command "pandoc --smart -f markdown -t html")
(setq markdown-css-path "http://thomasf.github.io/solarized-css/solarized-light.min.css" )

;; Magit
(setq magit-auto-revert-mode nil)

;; UI Theme
;;(load-theme 'sanityinc-tomorrow-night t)
)

(defun dotspacemacs/config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  )

;; Keybindings
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

(global-set-key (kbd "M-p") 'helm-projectile)

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

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.

