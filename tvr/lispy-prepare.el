(eval-after-load
    "lispy"
  `(progn
     (define-key lispy-mode-map (kbd "M-m") nil)
     (define-key lispy-mode-map (kbd "C-,") nil)
     (define-key lispy-mode-map [(control left)] 'lispy-barf)
     (define-key lispy-mode-map [(control right)] 'lispy-snarf)
     (define-key lispy-mode-map-lispy (kbd "C-,") nil)
     (add-hook
      'lispy-mode-hook
      #'(lambda nil
          (setq
           header-line-format
           '((:eval
              (format  "%s %s %s"
                       (buffer-name)
                       " Lispy"
                       (if vc-mode
                           (concat
                            vc-mode
                            (format "%s" (vc-state (buffer-file-name))))
                         "")))))))
     ))
