;;;$Id$
;;; See http://johnwiegley.com/org.mode.day.planner.html

(load-library "org")
(load-library "remember")
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-directory "~/.org/")
(setq org-default-notes-file (expand-file-name "notes.org"  org-directory))
