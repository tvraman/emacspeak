;;; Set up racer for completion etc in rust buffers.
;;; See https://github.com/racer-rust/emacs-racer
(require 'rust-mode)
(add-hook 'rust-mode-hook #'eglot)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'company-mode)
(define-key rust-mode-map (kbd "TAB")
  #'company-indent-or-complete-common)
