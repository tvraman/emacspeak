;;; Jump to Emacs Git Logs At HEAD:
(defalias 'tvr-km-emacs-log
   (kmacro "C-c 3 C-; d F u C-; d l l M-< C-e C-q n"))
(global-set-key [24 11 48] 'tvr-km-emacs-log)
(defalias 'tvr-km-morning
   (kmacro "C-<tab> C-e g b h <tab> <return> n n e c"))
(global-set-key [24 11 49] 'tvr-km-morning)
