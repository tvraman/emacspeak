;;; vm-prepare.l :  -*- lexical-binding: nil; -*-
(push (expand-file-name "vm/lisp/" emacs-personal-library) load-path)
(autoload 'vm "vm" "vm" t)
(autoload 'vm-visit-folder "vm" "vm" t)
(global-set-key "\M-\C-v" 'vm-visit-folder)

(eval-after-load "vm"
  `(progn
     (load "vm-autoloads")
     (when (require 'bbdb) (bbdb-insinuate-vm))

     (global-set-key "\C-xm" 'vm-mail)
     
     (define-key vm-mode-map "o" 'mspools-show)
     (load-library "mspools")
     ))

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
