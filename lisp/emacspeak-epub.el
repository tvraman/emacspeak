;;; emacspeak-epub.el --- epubs Front-end for emacspeak desktop
;;; $Id: emacspeak-epub.el 5798 2008-08-22 17:35:01Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak front-end for EPUBS Talking Books
;;; Keywords: Emacspeak, epubs Digital Talking Books
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2008-06-21 10:50:41 -0700 (Sat, 21 Jun 2008) $ |
;;;  $Revision: 4541 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1999, 2011 T. V. Raman <raman@cs.cornell.edu>
;;; All Rights Reserved.
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  Introduction:

;;; Commentary:
;;; In celebration of a million books and more to read from
;;; Google Books
;;; The EPubs format is slightly simpler than full Daisy ---
;;; (see) emacspeak-daisy.el
;;; Since it only needs one level of indirection (no audio,
;;; therefore no smil). This module is consequently simpler than
;;; emacspeak-daisy.el.
;;; This module will eventually  implement the Google Books GData API
;;; --- probably by invoking the yet-to-be-written gbooks.el in emacs-g-client
;;; As we move to epub-3, this module will bring back audio layers etc., perhaps via a simplified smil implementation.
;;; Code:

;;}}}
;;{{{ Required Modules:

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'emacspeak-xslt)
(require 'derived)
(require 'find-lisp)

;;}}}
;;{{{  Customization variables

(defgroup emacspeak-epub nil
  "Epubs Digital  Books  for the Emacspeak desktop."
  :group 'emacspeak)

(defcustom emacspeak-epub-library-directory
  (expand-file-name "~/epubs/")
  "Directory under which we store Epubs."
  :type 'directory
  :group 'emacspeak-epub)

(defcustom emacspeak-epub-zip-extract
  (cond ((executable-find "unzip") "unzip")
        (t (error "unzip not found.")))
  "Program to extract a zip file member."
  :type 'string
  :group 'emacspeak-epub)

;;}}}
;;{{{ Epub Mode:

(define-derived-mode emacspeak-epub-mode special-mode
  "EPub Interaction On The Emacspeak Audio Desktop"
  "An EPub Front-end."
  (let ((inhibit-read-only t)
        (start (point)))
    (goto-char (point-min))
    (insert "Browse And Read EPub Materials\n\n")
    (put-text-property start (point)
                       'face font-lock-doc-face)
    (setq header-line-format "EPub Library")
    (cd-absolute emacspeak-epub-library-directory)))

;;}}}
;;{{{ EPub Implementation:
(defvar emacspeak-epub-toc-path-pattern
  ".ncx$"
  "Pattern match for path component  to table of contents in an Epub.")

(defvar emacspeak-epub-toc-command
  (format "zipinfo -1 %%s | grep %s" emacspeak-epub-toc-path-pattern)
  "Command that returns location of .ncx file in an epub archive.")

(defsubst emacspeak-epub-get-toc (file)
  "Return location of .ncx file within epub archive."
  (declare (special emacspeak-epub-toc-command))
  (substring
   (shell-command-to-string (format
                             emacspeak-epub-toc-command
                             file )) 0 -1))

(defvar emacspeak-epub-ls-command
  (format "zipinfo -1 %%s ")
  "Shell command that returns list of files in an epub archive.")

(defsubst emacspeak-epub-get-ls (file)
  "Return list of files in an epub archive."
  (declare (special emacspeak-epub-ls-command))
  (split-string
   (shell-command-to-string (format emacspeak-epub-ls-command file ))))

(defstruct emacspeak-epub
  path ; path to .epub file
  toc ; path to .ncx file in archive
  ls ; list of files in archive
  )

(defun emacspeak-epub-make-epub  (epub-file)
  "Construct an epub object given an epub filename."
  (let ((ls (emacspeak-epub-get-ls epub-file))
        (toc (emacspeak-epub-get-toc epub-file)))
    (unless (> (length toc) 0)
      (error "No TOC --- Not a valid EPub?"))
    (make-emacspeak-epub
     :path (expand-file-name epub-file)
     :toc toc
     :ls ls)))

(defun emacspeak-epub-get-contents (epub element)
  "Return buffer containing contents of element from epub."
  (unless (or  (emacspeak-epub-p epub)
               (member element (emacspeak-epub-ls epub)))
    (error "Invalid epub/element"))
  (let ((buffer
         (get-buffer-create
          (format " *epub-%s-%s*" (emacspeak-epub-path epub) element))))
    (with-current-buffer buffer
      (setq buffer-undo-list t)
      (erase-buffer)
      (call-process emacspeak-epub-zip-extract
                    nil t nil
                    "-c"
                    "-qq"
                    (emacspeak-epub-path epub)
                    element))
    buffer))
(defvar emacspeak-epub-this-epub nil
  "EPub associated with current buffer.")
(make-variable-buffer-local 'emacspeak-epub-this-epub)

(defun emacspeak-epub-browse-content (epub element &optional style )
  "Browse content in specified element of EPub."
  (unless   (emacspeak-epub-p epub) (error "Invalid epub"))
  (let ((content (emacspeak-epub-get-contents epub element))
        (base
         (format "\"'file:///%s'\"" (file-name-directory (emacspeak-epub-toc epub)))))
    (with-current-buffer content
      (add-hook 'emacspeak-web-post-process-hook
            #'(lambda nil
                (declare (special emacspeak-we-url-executor
                                  emacspeak-epub-this-epub))
                (setq emacspeak-epub-this-epub epub
                      emacspeak-we-url-executor 'emacspeak-epub-url-executor))
            'at-end)
      (emacspeak-webutils-with-xsl-environment
       style
       (list
        (cons "base"  base))            ;params
       nil                              ; options
       (browse-url-of-buffer)))))

(defun emacspeak-epub-browse-toc (epub)
  "Browse table of contents from an EPub."
  (unless   (emacspeak-epub-p epub)
    (error "Invalid epub"))
  (let ((toc (emacspeak-epub-toc epub)))
    (emacspeak-epub-browse-content epub toc
                                   (expand-file-name "epub-toc.xsl" emacspeak-xslt-directory))))

(defun emacspeak-epub-url-executor (url)
  "Custom URL executor for use in EPub Mode."
  (interactive "sURL: ")
  (declare (special emacspeak-epub-this-epub))
  (unless emacspeak-epub-this-epub
    (error "No EPub associated with this buffer."))
  (cond
   ((string-match "^file:" url)
    (emacspeak-epub-browse-content emacspeak-epub-this-epub (substring url  6)))
   (t (browse-url url))))
;;}}}
;;{{{ Interactive Commands:

(defvar emacspeak-epub-interaction-buffer "*EPub*"
  "Buffer for EPub interaction.")

;;;###autoload
(defun emacspeak-epub ()
  "EPub  Interaction."
  (interactive)
  (declare (special emacspeak-epub-interaction-buffer))
  (let ((buffer (get-buffer emacspeak-epub-interaction-buffer)))
    (cond
     ((buffer-live-p buffer) (switch-to-buffer buffer))
     (t
      (with-current-buffer (get-buffer-create emacspeak-epub-interaction-buffer)
        (erase-buffer)
        (setq buffer-undo-list t)
        (emacspeak-epub-mode)
        (setq buffer-read-only t))
      (switch-to-buffer emacspeak-epub-interaction-buffer)))
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(declaim (special emacspeak-epub-mode-map))
(loop for k in
      '(
        ("o" emacspeak-epub-open)
        ("g" emacspeak-epub-google)
        )
      do
      (emacspeak-keymap-update emacspeak-epub-mode-map k))

(defvar emacspeak-epub-toc-transform
  (expand-file-name "epub-toc.xsl" emacspeak-xslt-directory)
  "XSLT  Transform that maps epub-toc to HTML.")

;;;###autoload
(defun emacspeak-epub-open (toc)
  "Open specified Epub.
`toc' is the pathname to an EPubs table of contents."
  (interactive
   (list
    (emacspeak-epub-get-toc)))
  (declare (special emacspeak-epub-toc-transform))
  (emacspeak-webutils-autospeak)
  (emacspeak-xslt-view-file emacspeak-epub-toc-transform toc))

(defvar emacspeak-epub-google-search-template
  "http://books.google.com/books/feeds/volumes?min-viewability=full&epub=epub&q=%s"
  "REST  end-point for performing Google Books Search to find Epubs  having full viewability.")

;;;###autoload
(defun emacspeak-epub-google (query)
  "Search for Epubs from Gooble Books."
  (interactive "sGoogle Books Query: ")
  (declare (special emacspeak-epub-google-search-template))
  (emacspeak-webutils-atom-display
   (format emacspeak-epub-google-search-template
           (emacspeak-url-encode query))))

;;}}}

(provide 'emacspeak-epub)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
