;;; packages.el --- Writeroom Layer packages File for Spacemacs
;;
;; Copied from https://raw.githubusercontent.com/fmdkdd/dotfiles/master/spacemacs/.emacs.d/private/writeroom/packages.el
;;
;; Copyright (c) 2015 fmdkdd
;;
;; Author: fmdkdd
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq my-writeroom-packages
      '(writeroom-mode))

(defun my-writeroom/init-writeroom-mode ()
  (use-package writeroom-mode
    :commands (writeroom-mode)
    :init
    (evil-leader/set-key "wr" 'writeroom-mode)
    :config
    (setq writeroom-restore-window-config t)))
