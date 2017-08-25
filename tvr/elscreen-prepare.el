;; -*- lexical-binding: t; -*-
(when (locate-library "elscreen")
  (elscreen-start)
  (let ((copy-map (copy-keymap elscreen-map)))
    (define-key copy-map  [67108911] 'elscreen-toggle)
    (global-set-key   [67108911] copy-map)
    (global-set-key [C-tab] 'elscreen-toggle)))
(elscreen-persist-mode)
(setq elscreen-display-tab nil)
(condition-case nil
    (elscreen-persist-restore)
  (error "Failed to restore elscreen."))
