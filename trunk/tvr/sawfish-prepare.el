(load-library "sawfish")
(define-key sawfish-mode-map "\M-\C-i" 'sawfish-complete-symbol)


(augment-auto-mode-alist "\\.sawfishrc$" 'sawfish-mode)
(augment-auto-mode-alist "\\.jl$" 'sawfish-mode)
