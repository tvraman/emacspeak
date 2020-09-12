;;;$Id: org-prepare.el 6727 2011-01-14 23:22:20Z tv.raman.tv $  -*- lexical-binding: nil; -*-

(with-eval-after-load "org"
  (require 'org-tempo)
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-cb" 'org-switchb)
  (define-key global-map "\C-cc" 'org-capture)
  (define-key org-mode-map (kbd "C-c C-SPC") 'emacspeak-muggles-org-nav/body)
  (define-key org-mode-map (kbd "C-c t") 'emacspeak-muggles-org-table/body))
