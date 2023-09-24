;;;$Id: org-prepare.el 6727 2011-01-14 23:22:20Z tv.raman.tv $  -*- lexical-binding: nil; -*-

(with-eval-after-load "org"

  (require 'org-tempo)
  (add-hook 'org-mode-hook #'turn-on-org-cdlatex)
  (require 'ol-eww)
  (require 'ox-md)
  )
(with-eval-after-load "orgalist"
  (diminish 'orgalist-mode ""))
