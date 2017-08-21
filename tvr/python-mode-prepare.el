;; -*- lexical-binding: t; -*-
(when (locate-library "python-mode-autoloads")
  (load-library "python-mode-autoloads"))

(add-hook
 'python-mode-hook
 #'(lambda ()
     (when (locate-library "company") (company-mode  1))
     ;(when (locate-library "elpy") (elpy-enable))
     ))
