
(with-eval-after-load "org"

  (require 'org-tempo)
  (add-hook 'org-mode-hook #'turn-on-org-cdlatex)
  (require 'ol-eww)
  (require 'ox-md)
  )
(with-eval-after-load "orgalist"
  (diminish 'orgalist-mode ""))
