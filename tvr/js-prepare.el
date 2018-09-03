;; -*- lexical-binding: nil; -*-
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(eval-after-load "js2-mode"
  `(progn  
     (when (locate-library "tern")
       (load-library "tern-autoloads")
       (add-hook 'js2-mode-hook #'tern-mode))
     (when (locate-library "xref-js2")
       (load-library "xref-js2-autoloads"))
     (when (locate-library "js2-refactor")
       (load-library "js2-refactor-autoloads"))
     ))
