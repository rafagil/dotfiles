;; recent file size
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

(provide 'init-file-management)
