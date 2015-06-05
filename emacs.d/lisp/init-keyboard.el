;; Keyboard related things

;; expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Marking text
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)

(require 'autopair)
(electric-pair-mode 1)

;; guide-key
(require 'guide-key)
(setq guide-key/guide-key-sequence t)
(guide-key-mode 1)

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

(provide 'init-keyboard)
