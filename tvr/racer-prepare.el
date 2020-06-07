;;; Set up racer for completion etc in rust buffers.
;;; See https://github.com/racer-rust/emacs-racer
(require 'rust-mode)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'company-mode)
(define-key rust-mode-map (kbd "TAB")
  #'company-indent-or-complete-common)


;;; xref and racer:
(defun racer--xref-backend () 'racer)

(add-hook 'xref-backend-functions #'racer--xref-backend)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql racer)))
  (racer-find-definition))


(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql racer)))
  (completion-table-dynamic #'racer-complete))
