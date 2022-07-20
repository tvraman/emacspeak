;;; Jump to Emacs Git Logs At HEAD:
(defalias 'emacs-github-log
   (kmacro "C-c 3 C-; d F u C-; d l l"))
(global-set-key [24 11 48] 'emacs-github-log)
