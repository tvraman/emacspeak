
;;;$Id$

(require 'cl)

(defun batch-indent-files (file-list)
  "Batch indent specified list of elisp files."
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
        (save-buffer)))

(defun batch-indent-files-in-directory ()
  "Batch indent all files in working directory."
  (declare (special default-directory))
  (batch-indent-files
   (directory-files default-directory
		    nil
		    ".el$")))
