;; -*- lexical-binding: nil; -*-

;(load-library "python-mode-autoloads")
(eval-after-load "python-mode"
  `(progn
     (add-hook
      'python-mode-hook
      #'(lambda ()
           (company-mode  1)
           ;(elpy-enable)
           ))))
