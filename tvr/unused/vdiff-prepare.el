
(eval-after-load "vdiff"
`(progn
(define-key vdiff-mode-map (ems-kbd "C-c") vdiff-mode-prefix-map)
(setq vdiff-only-highlight-refinements t)
(setq vdiff-auto-refine t)))
