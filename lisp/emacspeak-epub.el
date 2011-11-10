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
;;; emacspeak-daisy.el
;;; Since it only needs one level of indirection (no audio,
;;; therefore no smil). This module is consequently simpler than
;;; emacspeak-daisy.el.
;;; This module will also implement the Google Books GData API
;;; --- probably by invoking the yet-to-be-written gbooks.el in emacs-g-client

;;; Code:

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

(defcustom emacspeak-epub-toc-path-pattern
  ".ncx$"
  "Pattern match for path component  to table of contents in an Epub."
  :type 'string
  :group 'emacspeak-epub)

;;}}}
;;{{{ Epub Mode:

(define-derived-mode emacspeak-epub-mode text-mode
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
     ((buffer-live-p buffer)
      (switch-to-buffer buffer))
     (t
      (with-current-buffer (get-buffer-create emacspeak-epub-interaction-buffer)
        (erase-buffer)
        (setq buffer-undo-list t)
        (setq buffer-read-only t)
        (emacspeak-epub-mode))
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

(defsubst emacspeak-epub-get-toc-path ()
  "Read book location and return path to table of contents."
  (declare (special emacspeak-epub-toc-path-pattern
                    emacspeak-epub-library-directory))
  (first
   (find-lisp-find-files 
    (read-directory-name "Epub:" emacspeak-epub-library-directory)
    emacspeak-epub-toc-path-pattern)))

(defvar emacspeak-epub-toc-transform
  (expand-file-name "epub-toc.xsl" emacspeak-xslt-directory)
  "XSLT  Transform that maps epub-toc to HTML.")

;;;###autoload
(defun emacspeak-epub-open (toc)
  "Open specified Epub.
`toc' is the pathname to an EPubs table of contents."
  (interactive
   (list
    (emacspeak-epub-get-toc-path)))
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
