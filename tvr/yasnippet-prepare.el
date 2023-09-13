;; -*- lexical-binding: nil; -*-
(defvar tvr-yas-snippets-loaded nil)
(with-eval-after-load "yasnippet"
  (unless tvr-yas-snippets-loaded
    (run-with-idle-timer 1 nil #'yas-reload-all)
    (setq tvr-yas-snippets-loaded t))
  (setq yas-verbosity 0)
  (diminish 'yas-minor-mode ""))
