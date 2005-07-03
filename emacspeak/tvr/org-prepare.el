;;;$Id$
;;; load org mode
(augment-load-path "org" "org")
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
;;; need to fix foldout 
(define-key outline-mode-map "\C-c\C-x" nil)

(load-library "remember-prepare")
;(autoload 'org-remember-annotation "org")
;(autoload 'org-remember-handler "org")
(setq org-directory "~/.org/")
(setq org-default-notes-file 
(expand-file-name "notes.org"  org-directory))
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))

