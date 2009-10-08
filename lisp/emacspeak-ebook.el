;;; emacspeak-ebook.el --- epubs Front-end for emacspeak desktop
;;; $Id: emacspeak-ebook.el 5798 2008-08-22 17:35:01Z tv.raman.tv $
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

;;; Copyright (C) 1999 T. V. Raman <raman@cs.cornell.edu>
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

(require 'emacspeak-preamble)
(require 'emacspeak-xslt)
(require 'easy-mmode)
;;}}}
;;{{{  Customization variables

(defgroup emacspeak-ebook nil
  "Epubs Digital  Books  for the Emacspeak desktop."
  :group 'emacspeak)

(defcustom emacspeak-ebook-library-root
  (expand-file-name "~/ebooks/")
  "Directory under which we store EBooks."
  :type 'directory
  :group 'emacspeak-ebook)


(defcustom emacspeak-ebook-toc-path
  "OEBPS/toc.ncx"
  "Path component  to table of contents in an EBook."
  :type 'string
  :group 'emacspeak-ebook)

(defvar emacspeak-ebook-toc-transform
  (expand-file-name "epub-toc.xsl"
                    emacspeak-xslt-directory)
  "Transformation that takes epub table of contents to XHTML.")

;;}}}
;;{{{ Define EBook Minor Mode
;;;###autoload
(define-minor-mode emacspeak-ebook-mode
  "EBooks minor mode."
  :keymap  emacspeak-ebook-keymap
  :lighter " EBooks")
    

;;}}}
;;{{{ Interactive Commands:
;;;###autoload
(defvar emacspeak-ebook-keymap nil
  "Keymap used for EBooks.")

;;;###autoload
(define-prefix-command  'emacspeak-ebook-command
  'emacspeak-ebook-keymap) 

(loop for k in
      '(
        ("o" emacspeak-ebook-open)
        ("g" emacspeak-ebook-google)
        )
      do
      (emacspeak-keymap-update emacspeak-ebook-keymap k))

(defsubst emacspeak-ebook-get-toc-path ()
  "Read book location and return path to table of contents."
  (declare (special emacspeak-ebook-toc-path
                    emacspeak-ebook-library-root))
  (expand-file-name
   (concat 
   (read-directory-name
    "EBook:"
    emacspeak-ebook-library-root)
   emacspeak-ebook-toc-path))
)

;;;###autoload
(defun emacspeak-ebook-open (toc)
  "Open specified EBook.
`toc' is the pathname to an EPubs table of contents."
  (interactive
   (list
    (emacspeak-ebook-get-toc-path)))
  (declare (special emacspeak-ebook-toc-transform))
  (emacspeak-webutils-autospeak)
  (emacspeak-xslt-view-file emacspeak-ebook-toc-transform toc))

(defvar emacspeak-ebook-google-search-template
  "http://books.google.com/books/feeds/volumes?min-viewability=full&ebook=epub&q=%s"
  "REST  end-point for performing Google Books Search to find EBooks  having full viewability.")

;;;###autoload
(defun emacspeak-ebook-google (query)
  "Search for EBooks from Gooble Books."
  (interactive "sGoogle Books Query: ")
  (declare (special emacspeak-ebook-google-search-template))
  (emacspeak-webutils-atom-display
   (format emacspeak-ebook-google-search-template
           (emacspeak-url-encode query))))

;;}}}

(provide 'emacspeak-ebook)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
