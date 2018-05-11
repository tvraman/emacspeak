;; -*- lexical-binding: nil; -*-

(eval-after-load
    'yasnippet
  `(progn
     (run-with-idle-timer 1 nil #'yas-reload-all)
     (setq yas-verbosity 0)))

