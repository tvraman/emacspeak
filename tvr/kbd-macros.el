;;; Jump to Emacs Git Logs At HEAD:
(defalias 'tvr-km-emacs-log
   (kmacro "C-c 3 C-; d f U C-; d l l C-e d q M-< C-e C-q C-e d q n"))
(global-set-key [24 11 48] 'tvr-km-emacs-log)
(defalias 'tvr-km-morning
   (kmacro "C-<tab> C-e g b h <tab> <return> n n e c"))
(global-set-key [24 11 49] 'tvr-km-morning)
