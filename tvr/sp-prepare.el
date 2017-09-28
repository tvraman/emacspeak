
(eval-after-load "smartparens"
  `(progn
     (require 'smartparens-config)
     (sp-use-smartparens-bindings)
     (define-key  smartparens-mode-map (kbd "C-M-a") 'beginning-of-defun)
     (define-key  smartparens-mode-map (kbd "C-M-e") 'end-of-defun)
     (define-key  smartparens-mode-map (kbd "C-M-k") 'kill-sexp)
     ))
