;;;$Id: org-prepare.el 6727 2011-01-14 23:22:20Z tv.raman.tv $  -*- lexical-binding: t; -*-

(load-library "org")

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cb" 'org-switchb)
(define-key global-map "\C-cc" 'org-capture)
(setq org-directory "~/.org/")
(setq org-default-notes-file (expand-file-name "notes.org"  org-directory))
(define-prefix-command 'org-todo-state-map)
(define-key org-mode-map "\C-cx" 'org-todo-state-map)
(define-key org-todo-state-map "x"
  #'(lambda nil (interactive) (org-todo "CANCELLED")))
(define-key org-todo-state-map "d"
  #'(lambda nil (interactive) (org-todo "DONE")))
(define-key org-todo-state-map "f"
  #'(lambda nil (interactive) (org-todo "DEFERRED")))
(define-key org-todo-state-map "l"
  #'(lambda nil (interactive) (org-todo "DELEGATED")))
(define-key org-todo-state-map "s"
  #'(lambda nil (interactive) (org-todo "STARTED")))
(define-key org-todo-state-map "w"
  #'(lambda nil (interactive) (org-todo "WAITING")))
