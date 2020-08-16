;; -*- lexical-binding: nil; -*-


(eval-after-load "mspools"
  `(progn
     (and (featurep 'vm) (define-key vm-mode-map "o" 'mspools-show))
     (defun mspools-compute-size (file)
       (with-temp-buffer
         (shell-command (format "grep '^From ' %s" file) (current-buffer))
         (count-lines (point-min) (point-max))))

     (defun mspools-size-folder (spool)
       "Return (SPOOL . SIZE ) iff SIZE of spool file is non-zero."
       (cons
        spool
        (mspools-compute-size (expand-file-name  spool mspools-folder-directory))))))
