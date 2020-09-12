;;; vm-prepare.l :  -*- lexical-binding: nil; -*-

(autoload 'vm "vm" "vm mail reader" t nil)
(autoload 'vm-visit-folder "vm" "Open VM folder" t nil)
(with-eval-after-load "vm"
  (global-set-key (kbd "C-x m") 'vm-mail)
  (when (require 'bbdb) (bbdb-insinuate-vm)))
