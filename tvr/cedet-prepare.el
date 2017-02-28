;; -*- lexical-binding: t; -*-
(augment-load-path "cedet")
(setq semantic-load-turn-useful-things-on t)
(load-library "cedet")
(global-semantic-idle-summary-mode -0)
(define-key global-map "\C-x@as"  senator-prefix-map)
(define-key senator-prefix-map "@" 'senator-mark-defun)
(define-key senator-mode-map "\M-n" 'senator-next-tag)
(define-key senator-mode-map "\M-p" 'senator-previous-tag)
