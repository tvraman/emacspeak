;;;$Id: org-prepare.el 6727 2011-01-14 23:22:20Z tv.raman.tv $  -*- lexical-binding: nil; -*-


(eval-after-load "org"
  `(progn
;;;  List creation commands:

     (defun tvr-org-itemize ()
       "Start a numbered  list."
       (interactive)
       (forward-line 0)
       (insert "  -  ")
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'item))

     (defun tvr-org-enumerate ()
       "Start a numbered  list."
       (interactive)
       (forward-line 0)
       (insert "  1.  ")
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'item))

;;; add these to outline-minor-mode-map
     ;;; will work in both org and orgalist when loaded 
     
     
     (define-key outline-minor-mode-map (kbd "C-o i") 'tvr-org-itemize)
     (define-key outline-minor-mode-map (kbd "C-o e") 'tvr-org-enumerate)
     
     (define-key global-map "\C-cl" 'org-store-link)
     (define-key global-map "\C-cb" 'org-switchb)
     (define-key global-map "\C-cc" 'org-capture)
     (require 'emacspeak-muggles-autoloads)
     (define-key org-mode-map (kbd "C-c C-SPC") 'emacspeak-muggles-org-nav/body)
     (define-key org-mode-map (kbd "C-c t") 'emacspeak-muggles-org-table/body)
     (define-key org-mode-map (kbd "C-c DEL") 'hydra-ox/body)
     ))
