;;; vm-prepare.l :  -*- lexical-binding: nil; -*-

(global-set-key "\M-\C-v" 'vm-visit-folder)
(eval-after-load "vm"
  `(progn
     (global-set-key "\C-xm" 'vm-mail)
     (define-key vm-mode-map "o" 'mspools-show)
     (when (require 'bbdb) (bbdb-insinuate-vm))
     (load-library "mspools")))
(eval-after-load
    "mspools"
  `(progn
     (defun mspools-compute-size (file)
       (let ((message-log-max nil)
             (inhibit-message t)
             (emacspeak-speak-messages nil)))
       (read (shell-command-to-string (format "grep '^From ' %s | wc -l" file))))
     (defun mspools-size-folder (spool)
       "Return (SPOOL . SIZE ) iff SIZE of spool file is non-zero."
       (let ((size (mspools-compute-size (expand-file-name  spool mspools-folder-directory))))
         (unless (zerop size)
           (cons spool size))))))
