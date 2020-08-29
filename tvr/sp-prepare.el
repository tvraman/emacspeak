
(with-eval-after-load "smartparens"
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)
  (define-key  smartparens-mode-map (kbd "C-M-a") 'beginning-of-defun)
  (define-key  smartparens-mode-map (kbd "C-M-e") 'end-of-defun)
  (define-key  smartparens-mode-map (kbd "C-M-k") 'kill-sexp)
  (define-key  smartparens-mode-map (kbd "C-M-SPC") 'mark-sexp)
  (define-key smartparens-mode-map (kbd "M-a") 'sp-backward-down-sexp)
  (define-key smartparens-mode-map (kbd "M-e") 'sp-up-sexp)
  (define-key smartparens-mode-map (kbd "M-k") 'sp-kill-sexp)
  (define-key smartparens-mode-map (kbd "C-M-f") 'forward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-b") 'backward-sexp))
