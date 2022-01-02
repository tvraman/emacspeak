;; -*- lexical-binding: nil; -*-
;;; setup package python-mode (melpa)
;;; http://www.python.org 
;;; not to be confused with
;;;built-in package python.
(eval-after-load "python-mode"
  `(progn
     (add-hook 'python-mode-hook #'elpy-enable)))
