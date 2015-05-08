;;; packages.el --- channing Layer packages File for Spacemacs
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

(defvar channing-packages
  '(
    expand-region
    color-theme-sanityinc-tomorrow
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar channing-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function channing/init-<package-channing>

(defun channing/init-expand-region ()
  (use-package expand-region
    :init (global-set-key (kbd "C-=") 'er/expand-region))
  )

(defun channing/init-color-theme-sanityinc-tomorrow ()
  (use-package color-theme-sanityinc-tomorrow
    :init (load-theme 'sanityinc-tomorrow-night t))
  )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
