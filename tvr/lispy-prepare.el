(eval-after-load
    "lispy"
  `(progn
     (define-key lispy-mode-map (kbd "M-m") nil)
     (define-key lispy-mode-map (kbd "C-,") nil)
     (define-key lispy-mode-map-lispy (kbd "C-,") nil)
     ))
