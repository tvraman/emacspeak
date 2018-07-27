;; -*- lexical-binding: nil; -*-
;(load-library "iplayer-autoloads")
(eval-after-load "iplayer"
  `(progn
(push '("w" "BBC World Service") iplayer-presets)
(push '("a" "BBC Asian Network") iplayer-presets)
(define-key iplayer-mode-map "a" 'iplayer-preset)
(define-key iplayer-mode-map "w" 'iplayer-preset)
(defadvice iplayer-channel (after header-line pre act comp)
  (setq header-line-format (format "%s" channel)))
))
