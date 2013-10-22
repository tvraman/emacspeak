;;;$Id: elint-files.el 7425 2011-11-22 01:55:17Z tv.raman.tv $

(require 'cl)
(require 'elint)
(defun batch-elint-files ()
  "Batch elint  elisp files in directory."
  (let ((file-list (directory-files default-directory nil "\\.el\\'")))
    (push default-directory load-path)
    (push (expand-file-name "g-utils" default-directory) load-path)
    (loop for f in file-list
          do
          (elint-file f))))
            
(batch-elint-files)
