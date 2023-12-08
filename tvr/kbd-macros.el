;;; Jump to Emacs Git Logs At HEAD:
(defalias 'tvr-km-emacs-log
  (kmacro
   "C-c 3 g i t SPC p <return> C-; d l l C-a "))
(define-key emacspeak-y-keymap "3" 'tvr-km-emacs-log)
(defalias 'tvr-km-morning
  (kmacro "C-<tab> C-e g b h <tab> <return> n n e c"))
(define-key emacspeak-y-keymap "0" 'tvr-km-morning)
