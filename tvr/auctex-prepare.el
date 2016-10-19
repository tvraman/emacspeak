(load-library "tex-site")  -*- lexical-binding: t; -*-
(load-library "auctex-autoloads")
(load-library "reftex-prepare")
(add-hook 'LaTeX-mode-hook 'reftex-mode)
