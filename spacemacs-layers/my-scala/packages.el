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

  (defun my-scala/post-init-scala-mode2 ()
    "Initialize my scala"
    (use-package scala
      :init
      (progn
        (add-hook 'scala-mode-hook (lambda () (setq truncate-lines t)))
        (add-hook 'scala-mode-hook (lambda () (setq indent-tabs-mode nil)))
        (add-hook 'scala-mode-hook (lambda () (setq show-trailing-whitespace t)))
        (add-hook 'scala-mode-hook #'global-flycheck-mode)

        (setq flycheck-scalastyle-jar (concat (file-name-as-directory user-home-directory) "dotfiles/spacemacs-layers/my-scala/scalastyle_2.11-0.7.0-batch.jar"))
        (setq flycheck-scalastylerc (concat (file-name-as-directory user-home-directory) "dotfiles/spacemacs-layers/my-scala/scalastyle_config.xml"))
        )))

;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
