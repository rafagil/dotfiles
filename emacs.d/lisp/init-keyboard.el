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

(provide 'init-keyboard)
