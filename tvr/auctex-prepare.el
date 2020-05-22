;; -*- lexical-binding: nil; -*-


(eval-after-load "auctex"
`(progn
   (load-library "reftex-prepare")
(add-hook 'LaTeX-mode-hook 'reftex-mode)
))
