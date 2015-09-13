;;; packages.el --- my-org Layer packages File for Spacemacs

(setq my-org-packages
      '(
        org
        ox-latex
        ox-publish
        ))

;; List of packages to exclude.
(setq my-org-excluded-packages '())

(defun my-org/init-org ()
  "Initialize org"
  (use-package org
    :defer t
    :init
    (progn

      ;; Fonts
      (add-hook 'org-mode-hook 'variable-pitch-mode)

      ;; got to be a better way to do this!
      (add-hook 'org-mode-hook
                (lambda () (set-face-attribute 'org-table nil :inherit 'fixed-pitch)))
      (add-hook 'org-mode-hook
                (lambda () (set-face-attribute 'org-code nil :inherit 'fixed-pitch)))
      (add-hook 'org-mode-hook
                (lambda () (set-face-attribute 'org-block nil :inherit 'fixed-pitch)))
      (custom-set-faces
       '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
       '(org-level-2 ((t (:inherit outline-1 :height 1.3))))
       '(org-level-3 ((t (:inherit outline-1 :height 1.1))))
       )

      ;; For exporting to latex and pdf do
      ;; $ brew install caskroom/cask/brew-cask
      ;; $ brew cask install mactex
      (setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))

      (setq org-directory "~/Dropbox/org")
      (setq project-directory (concat org-directory "/projects"))

      ;; agenda
      (setq org-agenda-start-with-log-mode t)
      (setq org-agenda-include-inactive-timestamps t)
      (setq org-agenda-files (list org-directory
                                   (concat project-directory "/boost")
                                   (concat project-directory "/foggyball")
                                   (concat org-directory "/blogs")))

      ;; Clocking
      (setq org-clock-persist 'history)
      (org-clock-persistence-insinuate)

      ;; capture
      (setq org-default-notes-file (concat org-directory "/notes.org"))

      (setq org-log-into-drawer t)

      (setq org-capture-templates
            '(("t" "Todo" entry (file+headline (concat org-directory "/todo.org") "To do")
               "* TODO [#B] %U %?\n  %i\n  %a")

              ("T" "Todo (Important)" entry (file+headline (concat org-directory "/todo.org") "To do")
               "* TODO [#A] %U %?\n  %i\n  %a")

              ("d" "Done" entry (file+headline (concat org-directory "/todo.org") "To do")
               "* DONE %U %?\n  %i\n")

              ("h" "HowTo" entry (file+headline (concat org-directory "/howto.org") "How To")
               "* %^{What?} :%^{Tag}:\n %?\n")

              ("m" "Fixme" entry (file+headline (concat org-directory "/todo.org") "To do")
               "* FIXME [#C] %U %?\n  %i\n  %a")

              ("j" "Personal Journal" entry (file+datetree (concat org-directory "/journal.org"))
               "* %U\n\n%?\n")

              ("b" "Boost Journal" entry (file+datetree (concat project-directory "/boost/journal.org"))
               "* %U\n\n%?\n")

              ("f" "Foggyball Journal" entry (file+datetree (concat project-directory "/foggyball/journal.org"))
               "* %U\n\n%?\n")

              ("g" "Glossary" entry (file+headline (concat project-directory "/boost/glossary.org") "Glossary")
               "* %^{Term} :%^{Tag}:\n %?\n")

              ("n" "Add note to the clocked task" item (clock) "+ %T %?")
              ))

      (setq org-refile-targets '((org-agenda-files :maxlevel . 9)))

      (setq org-todo-keywords
            '((sequence "TODO(t!)" "STARTED(s!)" "WAITING(w@/!)" "|" "DONE(d!)" "NO NEED(n@/!)")
              (sequence "BUG(b!)" "FIXME(f!)" "|" "FIXED(f!)" "WON'T FIX(o@/!)" "DELEGATED(l@/!)")
              (sequence "QUESTION(q!)" "|" "ANSWERED(a!)")
              (sequence "|" "CANCELED(c!)")))

      (defun channing/clock-in-when-starting ()
        (when (equal (org-get-todo-state) "STARTED") (org-clock-in) ))

      (add-hook 'org-after-todo-state-change-hook
                'channing/clock-in-when-starting)

      (defun channing/clock-out-when-waiting ()
        (when (equal (org-get-todo-state) "WAITING") (org-clock-out) ))

      (add-hook 'org-after-todo-state-change-hook
                'channing/clock-out-when-waiting)

      (setq org-refile-allow-creating-parent-nodes 'confirm)
      (setq org-refile-use-outline-path 'file)

      ;; From http://emacs.stackexchange.com/a/10762/5463
      (defun org-refile-to-datetree (&optional file)
        "Refile a subtree to a datetree corresponding to it's timestamp.
         The current time is used if the entry has no timestamp. If FILE
         is nil, refile in the current file."
        (interactive "f")
        (let* ((datetree-date (or (org-entry-get nil "TIMESTAMP" t)
                                  (org-read-date t nil "now")))
               (date (org-date-to-gregorian datetree-date))
               )
          (save-excursion
            (with-current-buffer (current-buffer)
              (org-cut-subtree)
              (if file (find-file file))
              (org-datetree-find-date-create date)
              (org-narrow-to-subtree)
              (show-subtree)
              (org-end-of-subtree t)
              (newline)
              (goto-char (point-max))
              (org-paste-subtree 4)
              (widen)
              ))
          )
        )

      ;; This from http://orgmode.org/worg/org-faq.html
      (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

      ;; Babel for diagrams etc
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((emacs-lisp . t)
         (plantuml   . t)
         (ditaa      . t)
         (java       . t)
         (js         . t)
         (scala      . t)
         (sh         . t)
         (sql        . t)
         (dot        . t)))

      ;; don't prompt when evaluating various babel files
      (defun my-org-confirm-babel-evaluate (lang body)
        (not (string= lang "plantuml")))  ; don't ask for ditaa
      (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

      ;; Setup path to ditaa.jar after brew install ditaa
      (setq org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.9/libexec/ditaa0_9.jar")

      (setq org-plantuml-jar-path
            (expand-file-name "~/dotfiles/plantuml/plantuml.jar"))

      (setq org-image-actual-width nil)

      ;; pandoc
      ;; special settings for beamer-pdf and latex-pdf exporters
      (setq org-pandoc-options-for-beamer-pdf '((latex-engine . "xelatex")))
      (setq org-pandoc-options-for-latex-pdf '((latex-engine . "xelatex")))

      ;; publishing
      (setq org-publish-project-alist
            '(
              ("org-notes"
               :base-directory "~/Dropbox/org/"
               :base-extension "org"
               :publishing-directory "~/public_html/"
               :recursive t
               :publishing-function org-html-publish-to-html
               :headline-levels 4             ; Just the default for this project.
               :auto-preamble t
               )

              ("org-static"
               :base-directory "~/Dropbox/org/"
               :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf|pptx\\|ppt\\|doc\\|docx\\|"
               :publishing-directory "~/public_html/"
               :recursive t
               :publishing-function org-publish-attachment
               )

              ("org" :components ("org-notes" "org-static"))
              ))

      (setq org-publish-use-timestamps-flag nil)

      )
    )
  )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
