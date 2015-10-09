;;; packages.el --- my-scala Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq my-scala-packages
      '(
        scala-mode2
      ))

;; List of packages to exclude.
(setq my-scala-excluded-packages '())

;; For each package, define a function my-scala/init-<package-name>
;;

;;(when (configuration-layer/layer-usedp 'scala)
  (defun my-scala/post-init-scala-mode2 ()
    "Initialize my scala"
    (use-package scala
      :init
      (progn
        (message "Initialising Scala")
        (load-file "~/Code/dev/emacs-scalaz-unicode-input-method/scalaz-unicode-input-method.el")
        (add-hook 'scala-mode-hook
                  (lambda () (set-input-method "scalaz-unicode")))

        (setq input-method-highlight-flag nil)

        (add-hook 'scala-mode-hook (lambda () (setq truncate-lines t)))
        (add-hook 'scala-mode-hook (lambda () (setq indent-tabs-mode nil)))
        (add-hook 'scala-mode-hook (lambda () (setq show-trailing-whitespace t)))

        ;; Only enable unicode mode for insert and emacs states in evil-mode
        (add-hook 'evil-insert-state-entry-hook
                  (lambda () (set-input-method "scalaz-unicode")))
        (add-hook 'evil-insert-state-exit-hook
                  (lambda () (set-input-method nil)))
        (add-hook 'evil-emacs-state-entry-hook
                  (lambda () (set-input-method "scalaz-unicode")))
        (add-hook 'evil-emacs-state-exit-hook
                  (lambda () (set-input-method nil)))

        )))

;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
