;;; Set up racer for completion etc in rust buffers.
;;; See https://github.com/racer-rust/emacs-racer
(require 'rust-mode)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'racer-mode-hook #'eglot-ensure)
(define-key rust-mode-map (ems-kbd "TAB")
  #'company-indent-or-complete-common)
