;;; packages.el --- my-markdown Layer packages File for Spacemacs
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
(setq my-markdown-packages
    '(
      markdown
      ))

;; List of packages to exclude.
(setq my-markdown-excluded-packages '())

;; For each package, define a function my-markdown/init-<package-name>
;;
 (defun my-markdown/post-init-my-package ()
   "Initialize my markdown"
   (use-package markdown
     :defer t
     :init
     (progn
       (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
       (add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
       (add-hook 'markdown-mode-hook
                 (lambda ()
                   (visual-line-mode t)
                   (flyspell-mode t)))
       (setq markdown-command "pandoc --smart -f markdown -t html")
       (setq markdown-css-paths "http://thomasf.github.io/solarized-css/solarized-light.min.css" ))))

;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
