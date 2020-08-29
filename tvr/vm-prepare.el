;;; vm-prepare.l :  -*- lexical-binding: nil; -*-

(with-eval-after-load "vm"
     (global-set-key (kbd "C-x m") 'vm-mail)
     (when (require 'bbdb) (bbdb-insinuate-vm))
     (load-library "mspools"))

