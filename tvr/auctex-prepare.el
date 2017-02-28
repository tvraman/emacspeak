;; -*- lexical-binding: t; -*-
(load-library "tex-site")
(load-library "auctex-autoloads")
(load-library "reftex-prepare")
(add-hook 'LaTeX-mode-hook 'reftex-mode)
