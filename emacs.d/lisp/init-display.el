;; Display Settings
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

;; yes and no
(defalias 'yes-or-no-p 'y-or-n-p)

;; is this the right place for this?
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(column-number-mode 1)

;; Show line numbers
(setq linum-format "%4d ")
(global-linum-mode 1)
(setq cursor-type (quote bar))
(modify-all-frames-parameters (list (cons 'cursor-type 'bar)))

;; show the time
(display-time-mode 1)

;; make emacs split buffers horizontally not vertically
;; see http://stackoverflow.com/questions/2081577/setting-emacs-split-to-horizontal
(setq split-height-threshold nil)
(setq split-width-threshold 0)

(provide 'init-display)
