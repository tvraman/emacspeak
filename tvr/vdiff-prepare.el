(require 'vdiff)
(define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)
(csetq vdiff-only-highlight-refinements t)
