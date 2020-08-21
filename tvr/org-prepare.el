;;;$Id: org-prepare.el 6727 2011-01-14 23:22:20Z tv.raman.tv $  -*- lexical-binding: nil; -*-


(eval-after-load "org"
  `(progn
     (define-key global-map "\C-cl" 'org-store-link)
     (define-key global-map "\C-cb" 'org-switchb)
     (define-key global-map "\C-cc" 'org-capture)
     (require 'emacspeak-muggles-autoloads)
     (define-key org-mode-map (kbd "C-c C-SPC") 'emacspeak-muggles-org-nav/body)
     (define-key org-mode-map (kbd "C-c t") 'emacspeak-muggles-org-table/body)
     (define-key org-mode-map (kbd "C-c DEL") 'hydra-ox/body)
     ))
