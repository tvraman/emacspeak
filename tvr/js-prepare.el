;; -*- lexical-binding: t; -*-
(when (locate-library "js2-mode")
  (load "js2-mode-autoloads")
  (augment-auto-mode-alist "\\.js$" 'js2-mode))
(when (locate-library "tern")
  (load-library "tern-autoloads")
  (add-hook 'js-mode-hook #'tern-mode)
  (add-hook 'js2-mode-hook #'tern-mode))
