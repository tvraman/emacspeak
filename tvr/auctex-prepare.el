;(load-library "tex-site")  -*- lexical-binding: t; -*-
(load-library "reftex-prepare")
(add-hook 'LaTeX-mode-hook 'reftex-mode)
(add-hook 'LaTeX-mode-hook
          #'(lambda nil
              (setq fill-prefix "")
              (setq adaptive-fill-mode nil)))
