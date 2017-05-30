;; -*- lexical-binding: t; -*-

(load-library "yasnippet")

(yas-reload-all)
(add-hook
 'prog-mode-hook
 #'(lambda nil
     (yas-minor-mode 1)))
;;; Silence yasnippet:
(setq yas-verbosity 0)
