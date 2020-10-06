;;; vm-prepare.l :  -*- lexical-binding: nil; -*-

(autoload 'vm "vm" "vm mail reader" t nil)
(autoload 'vm-visit-folder "vm" "Open VM folder" t nil)
(with-eval-after-load "vm"
  (global-set-key "\C-xm" 'vm-mail)
  (when (require 'bbdb) (bbdb-insinuate-vm)))

(defun make-local-hook (hook)
  "compatibility"
  (if (local-variable-p hook)
      nil
    (or (boundp hook) (set hook nil))
    (make-local-variable hook)
    (set hook (list t)))
  hook)
