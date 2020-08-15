;; -*- lexical-binding: nil; -*-

(eval-after-load "python-mode"
  `(progn
     (add-hook 'python-mode-hook #'elpy-enable)))
