;;; extensions.el --- my-markdown Layer extensions File for Spacemacs
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

(setq my-markdown-pre-extensions
      '(
        ;; pre extension names go here
        ))

(setq my-markdown-post-extensions
      '(
        ;; post extension names go here
        ))

;; For each extension, define a function my-markdown/init-<extension-name>
;;
;; (defun my-markdown/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package