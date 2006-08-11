;;; Loading  and using auctex 
(augment-load-path "auctex" "latex")
;;; auc-tex:
(load-library-if-available "tex-site")
(setq TeX-lisp-directory
      (expand-file-name "auctex/"
                        emacs-personal-library-directory))

(load-library "reftex-prepare")
(add-hook 'LaTeX-mode-hook 'reftex-mode)
(add-hook 'LaTeX-mode-hook
          #'(lambda nil
              (setq fill-prefix "")
              (setq adaptive-fill-mode nil)))
