(eval-after-load
    "lispy"
  `(progn
     (define-key lispy-mode-map (kbd "M-m") nil)
     (define-key lispy-mode-map (kbd "C-,") nil)
     (define-key lispy-mode-map-lispy (kbd "C-,") nil)
     (add-hook
      'lispy-mode-hook
      #'(lambda nil
          (setq
           header-line-format
           '((:eval
              (format  "%s %s"
                       (buffer-name)
                       "Emacs Lispy"))))))
     ))
