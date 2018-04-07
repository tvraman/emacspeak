;; -*- lexical-binding: nil; -*-

(eval-after-load
    'yasnippet
  `(progn 
     (yas-reload-all)
     (setq yas-verbosity 0)
     (add-hook 'prog-mode-hook #'yas-minor-mode)
     (run-with-idle-timer
      5 nil
      #'yas-global-mode 1)))

