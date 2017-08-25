;; -*- lexical-binding: nil; -*-
(load-library "tex-site")
(load-library "auctex-autoloads")
(eval-after-load "auctex"
`(progn
   (load-library "reftex-prepare")
(add-hook 'LaTeX-mode-hook 'reftex-mode)
))
