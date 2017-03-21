;; -*- lexical-binding: t; -*-

    (load-library"mspools")
    (and (featurep 'vm)
         (declare (special vm-mode-map))
         (define-key vm-mode-map "o" 'mspools-show))
(defun mspools-compute-size (file)
    (with-temp-buffer
      (shell-command (format "grep '^From ' %s" file) (current-buffer))
          (count-lines (point-min) (point-max))))

(defun mspools-size-folder (spool)
  "Return (SPOOL . SIZE ) iff SIZE of spool file is non-zero."
  (let* ((file (concat mspools-folder-directory spool))
         (size (mspools-compute-size file )))
    (setq file (or (file-symlink-p file) file))
                                        ;size could be nil if the sym-link points to a non-existent file
                                        ;so check this first.
    (if (and size  (> size 0))
 	(cons spool  size)
                                        ;else SPOOL is empty
      nil)))
