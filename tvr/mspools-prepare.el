(defsubst mspools-compute-size (file)
  "Count number of messages."
  (cond
   ((= 0 (nth  7 (file-attributes file))) 0)
   (t (let ((buffer (get-buffer-create " *mspools-scratch*")))
        (save-excursion
          (set-buffer buffer)
          (erase-buffer)
          (shell-command (format "grep '^From ' %s | wc -l" file) buffer)
          (read (buffer-string)))))))

(defsubst mspools-size-folder (spool)
  "Return (SPOOL . SIZE ) iff SIZE of spool file is non-zero."
  (let* ((file (expand-file-name spool mspools-folder-directory))
         (size (mspools-compute-size file )))
    (setq file (or (file-symlink-p file) file))
                                        ;size could be nil if the sym-link points to a non-existent file
                                        ;so check this first.
    (if (and size  (> size 0))
	(cons spool  size)
                                        ;else SPOOL is empty
      nil)))
