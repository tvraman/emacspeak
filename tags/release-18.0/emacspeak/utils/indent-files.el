
;;;$Id$

(require 'cl)

(defun batch-indent-files ()
  "Batch indent  elisp files in directory.."
  (let ((file-list (directory-files default-directory
                                    nil
                                    "\\.el\\'")))
    (loop for f in file-list
          do
          (find-file f)
          (lisp-indent-region (point-min)
                              (point-max))
          (shell-command-on-region (point-min)
                                   (point-max)
                                   "cat -s"
                                   (current-buffer)
                                   'replace)
          (save-buffer))))

  
(batch-indent-files)
