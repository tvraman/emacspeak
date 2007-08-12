;;; Loading  and using auctex 
(augment-load-path "auctex" "tex-site")
;;; auc-tex:
(load-library-if-available "tex-site")

(load-library "reftex-prepare")
(add-hook 'LaTeX-mode-hook 'reftex-mode)
(add-hook 'LaTeX-mode-hook
          #'(lambda nil
              (setq fill-prefix "")
              (setq adaptive-fill-mode nil)))


(declare (special TeX-auto-private
                      TeX-macro-private
                      TeX-auto-local))
    
    ;; Personal defaults for AUC-TeX mode
;;; variable settings:
    (declare (special 
              tex-mode-hook))
                                        ; find overful underful boxes in debugger. 

    (setq tex-mode-hook
          #'(lambda()
              (local-set-key "\M-s" 'save-buffer)
              (local-set-key "\C-c," 'comma-at-end-of-word)
              (local-set-key "\C-c~" 'tex-tie-current-word)
              (auto-fill-mode 1)))
