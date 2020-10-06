;;;$Id: org-prepare.el 6727 2011-01-14 23:22:20Z tv.raman.tv $  -*- lexical-binding: nil; -*-

(with-eval-after-load "org"
  (require 'org-tempo)
  (define-key global-map "\C-c l" 'org-store-link)
  (define-key global-map "\C-c b" 'org-switchb)
  (define-key global-map  "\C-c c" 'org-capture)
)
