;; -*- lexical-binding: t; -*-
(load-library "outline")
;restore what we are about to steal 
(define-key outline-mode-prefix-map "o" 'open-line)
(global-set-key "\C-o"outline-mode-prefix-map)
