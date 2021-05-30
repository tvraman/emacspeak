;; -*- lexical-binding: nil; -*-
(defvar tvr-yas-snippets-loaded nil)
(eval-after-load
    'yasnippet
  `(progn
     (unless tvr-yas-snippets-loaded
       (run-with-idle-timer 1 nil #'yas-reload-all)
       (setq tvr-yas-snippets-loaded t))
     (setq yas-verbosity 0)))

