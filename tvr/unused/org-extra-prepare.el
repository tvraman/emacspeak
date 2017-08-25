;;;$Id: org-prepare.el 6727 2011-01-14 23:22:20Z tv.raman.tv $  -*- lexical-binding: nil; -*-

(load-library "org-autoloads")
(eval-after-load "org"
    `(progn
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
       (require 'emacspeak-muggles-autoloads)
       (define-key org-mode-map (kbd "C-c C-SPC") 'emacspeak-muggles-org-nav/body)
       (define-key org-mode-map (kbd "C-c t") 'emacspeak-muggles-org-table/body)
       (define-key org-mode-map (kbd "C-c DEL") 'hydra-ox/body)
       ))
