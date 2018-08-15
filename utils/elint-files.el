;;;$Id: elint-files.el 7425 2011-11-22 01:55:17Z tv.raman.tv $  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'advice)
(require 'derived)
(push default-directory load-path)
(push (expand-file-name "g-client" default-directory) load-path)
(require 'emacspeak-load-path)
(load-file (expand-file-name "emacspeak-loaddefs.el" emacspeak-lisp-directory))
(require 'emacspeak-sounds)
(load-library "g-loaddefs")
(require 'elint)
(require 'emacspeak-preamble)
(defun batch-elint-files ()
  "Batch elint  elisp files in directory."
  (let ((file-list (directory-files default-directory nil "\\.el\\'")))
    (cl-loop
     for f in file-list do
     (unless
         (or (string-match  "emacspeak-loaddefs.el" file)
             (string-match "emacspeak-autoload.el" file)
             (string-match ".skeleton.el" file))
       (elint-file f)))))

(batch-elint-files)
