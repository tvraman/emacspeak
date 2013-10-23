;;;$Id: elint-files.el 7425 2011-11-22 01:55:17Z tv.raman.tv $

(require 'cl)
(require 'advice)
(push default-directory load-path)
(push (expand-file-name "g-client" default-directory) load-path)
(require 'emacspeak-load-path)
(require 'elint)
(defun batch-elint-files ()
  "Batch elint  elisp files in directory."
  (let ((file-list (directory-files default-directory nil "\\.el\\'")))
    (loop for f in file-list
          do
          (unless
              (or (eq file "emacspeak-loaddefs.el")
                  (eq file "emacspeak-autoloads.el"))
            (elint-file f)))))
            
(batch-elint-files)
