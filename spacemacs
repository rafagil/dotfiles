;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/dotfiles/spacemacs-layers/")
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     better-defaults
     autopair
     emacs-lisp
     git
     latex
     markdown
     org
     osx
     scala
     smex
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     syntax-checking
     themes-megapack
     version-control
     ;; mine
     my-org
     my-scala
     my-markdown
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '()
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
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed.
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '( solarized-light
                          solarized-dark
                          spacemacs-light
                          spacemacs-dark
                          leuven
                          monokai
                          zenburn)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("CMU Typewriter Text"
                               :size 12
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
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; Default value is `cache'.
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f) is replaced.
   dotspacemacs-use-ido nil
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state nil
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
   dotspacemacs-maximized-at-startup t
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
   ;; Select a scope to highlight delimiters. Possible value is `all',
   ;; `current' or `nil'. Default is `all'
   dotspacemacs-highlight-delimiters 'all
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

  ;; Start with agenda
  (setq inhibit-splash-screen t)
  (add-hook 'after-init-hook (lambda () (org-agenda nil "n")))
  (setq org-agenda-window-setup 'current-window)
  )

(defun dotspacemacs/config ()
  "Configuration function.
   This function is called at the very end of Spacemacs initialization after
   layers configuration."

  (setq user-full-name "Channing Walton")

  (setenv "PATH" (shell-command-to-string "/bin/bash -c 'echo -n $PATH'"))
  ;; zsh
  (add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

  (setq exec-path (append exec-path '("/usr/local/bin/")))
  (setq exec-path (append exec-path '("/usr/texbin/")))
  (setq exec-path (append exec-path '("~/bin/")))

  (setq recentf-max-saved-items 100)
  (global-auto-revert-mode t)

  ;; Backups
  (setq backup-directory-alist '(("." . "~/Dropbox/emacs-backup"))
        backup-by-copying t    ; Don't delink hardlinks
        version-control t      ; Use version numbers on backups
        delete-old-versions t  ; Automatically delete excess backups
        kept-new-versions 20   ; how many of the newest versions to keep
        kept-old-versions 5    ; and how many of the old
        )

  ;; hide backup files etc
  (setq dired-omit-mode t)
  (setq dired-listing-switches "-al")

  ;; spelling
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra"))
  (setq ispell-dictionary "british")
  (setq ispell-personal-dictionary "~/dotfiles/dictionary.txt")

  ;;formatting
  (setq-default indicate-empty-lines t)
  (when (not indicate-empty-lines)
    (toggle-indicate-empty-lines))

  ;; Indentation
  (setq tab-width 2
        indent-tabs-mode nil)
  (setq-default indent-tabs-mode nil)

  ;; git
  (add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))

  ;; Magit
  (setq magit-auto-revert-mode nil)
  (setq magit-last-seen-setup-instructions "1.4.0")

  ;; set proxy for work
  (if (string-equal system-name "LDNPWA000000054")
      (setq url-proxy-services
            '(("no_proxy" . "^\\(localhost\\|10.*\\)")
              ("http" . "primary-proxy.gslb.intranet.barcapint.com:8080")
              ("https" . "primary-proxy.gslb.intranet.barcapint.comp:8080")))
    )

  (with-eval-after-load 'yasnippet
    (setq yas-snippet-dirs (append yas-snippet-dirs '("~/dotfiles/snippets"))))

  (setq ns-right-alternate-modifier (quote none))
  (setq flycheck-scalastyle-jar "~/dotfiles/scalastyle/scalastyle_2.11-0.7.0.jar")

  ;; Keybindings

  ;; Avy
  (global-set-key (kbd "C-:") 'avy-goto-char)
  (global-set-key (kbd "C-'") 'avy-goto-char-2)
  (global-set-key (kbd "M-g f") 'avy-goto-line)
  (global-set-key (kbd "M-g w") 'avy-goto-word-1)
  (global-set-key (kbd "M-g e") 'avy-goto-word-0)

  ;; OSX Keybindings
  (global-set-key (kbd "<s-right>") 'move-end-of-line)
  (global-set-key (kbd "<s-left>") 'move-beginning-of-line)
  (global-set-key (kbd "s-b") 'helm-buffers-list)
  (global-set-key (kbd "<s-up>") 'beginning-of-buffer)
  (global-set-key (kbd "<s-down>") 'end-of-buffer)

  ;; Other Keybindings
  (global-set-key (kbd "TAB" ) 'smart-tab)
  (global-set-key (kbd "RET") 'newline-and-indent)
  (global-set-key (kbd "M-/") 'hippie-expand)
  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)

  (global-set-key (kbd "M-[") 'previous-buffer)
  (global-set-key (kbd "M-]") 'next-buffer)

  (global-set-key (kbd "M-x") 'smex)

  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)

  (global-set-key (kbd "M-x")      'helm-M-x)
  (global-set-key (kbd "M-s s")   #'helm-ag)
  (global-set-key (kbd "C-x b")   #'helm-mini)
  (global-set-key (kbd "C-x C-m") #'helm-M-x)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (global-set-key (kbd "C-x C-r") #'helm-recentf)
  (global-set-key (kbd "C-x r l") #'helm-filtered-bookmarks)
  (global-set-key (kbd "M-y")     #'helm-show-kill-ring)
  (global-set-key (kbd "M-s o")   #'helm-swoop)
  (global-set-key (kbd "M-s /")   #'helm-multi-swoop)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
