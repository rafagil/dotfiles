(load "package")
(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(setq package-archive-enable-alist '(("melpa" deft magit)))

(defvar channing/packages '(auto-complete
                            autopair
                            avy
                            company
                            dash
                            ensime
                            epl
                            exec-path-from-shell
                            expand-region
                            f
                            furl
                            guide-key
                            haskell-mode
                            helm
                            helm-ag
                            helm-projectile
                            helm-swoop
                            htmlize
                            ibuffer-vc
                            magit
                            markdown-mode
                            marmalade
                            org
                            org-plus-contrib
                            paredit
                            popup
                            projectile
                            rainbow-mode
                            s
                            color-theme-sanityinc-tomorrow
                            sbt-mode
                            scala-mode2
                            sr-speedbar
                            smex
                            web-mode
                            writegood-mode
                            yaml-mode
                            yasnippet)
  "Default packages")

(defun channing/packages-installed-p ()
  (loop for pkg in channing/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (channing/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg channing/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(provide 'init-packages)
