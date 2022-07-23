;;; Jump to Emacs Git Logs At HEAD:
(defalias 'tvr-emacs-log
   (kmacro "C-c 3 C-; d F u C-; d l l"))
(global-set-key [24 11 48] 'tvr-emacs-log)
(defalias 'tvr-morning
   (kmacro "C-<tab> C-e g b h <tab> <return> n n e c"))
(global-set-key [24 11 49] 'tvr-morning)
