;;;$Id: elint-files.el 7425 2011-11-22 01:55:17Z tv.raman.tv $

(require 'cl)
(require 'advice)
(require 'derived)
(push default-directory load-path)
(push (expand-file-name "g-client" default-directory) load-path)
(require 'emacspeak-load-path)
(load-library "g-loaddefs")
(require 'elint)
(defun batch-elint-files ()
  "Batch elint  elisp files in directory."
  (let ((file-list (directory-files default-directory nil "\\.el\\'")))
    (loop for f in file-list
          do
          (unless
              (or (string-match  "emacspeak-loaddefs.el" file)
                  (string-match "emacspeak-autoload.el" file))
            (elint-file f)))))
            
(batch-elint-files)
