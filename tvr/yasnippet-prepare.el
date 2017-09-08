;; -*- lexical-binding: nil; -*-

;(load-library "yasnippet-autoloads")
(eval-after-load 'yasnippet
  `(progn 
(yas-reload-all)
(setq yas-verbosity 0)))
(add-hook
 'prog-mode-hook
 #'(lambda nil
     (yas-minor-mode 1)))
