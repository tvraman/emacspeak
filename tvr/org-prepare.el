;;;$Id: org-prepare.el 6727 2011-01-14 23:22:20Z tv.raman.tv $  -*- lexical-binding: nil; -*-

(with-eval-after-load "org"
  (require 'org-tempo)
  (require 'ol-eww)
  (require 'ox-md)
  (define-key org-mode-map (ems-kbd "C-,") 'emacspeak-alt-keymap)
  (define-key org-mode-map (ems-kbd "C-c m")
              'org-md-export-as-markdown)
  (define-key global-map "\C-ci" 'org-insert-link)
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-cb" 'org-switchb)
  (define-key global-map  "\C-cc" 'org-capture)
  )
