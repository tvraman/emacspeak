;;; xml-sexp.el --- Convert XML to S-Expressions
;;; $Id$
;;; $Author$
;;; Description:  Convert XML to S-Expressions
;;; Keywords: Emacspeak, XML 
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision$ |
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

;;; Commentary:
;;{{{  Introduction:

;;;  xml-parse.el provides an XML parser in Emacs Lisp.
;;; xml.el --- part of Emacs ---- also implements an XML parser.
;;; Unfortunately both parsers are relatively slow --- and also
;;; raise errors when parsing streams of XML

;;; xsltproc ---  part of the libxslt2 Gnome package ---
;;; implements a fast XSLT processor.

;;; There is a straight-forward equivalence between XML and Lisp
;;; S-Expressions 
;;; that  Lisp XML parsers emulate in the structure they return
;;; e.g. xml-parse.el;
;;; however it's inefficient to to construct this structure with
;;; a pure Emacs Lisp parser.

;;; This is still too slow --- possibly rewrite as a native
;;; libxml2 app.

;;; This module implements an alternative approach --- it uses a
;;; simple XSLT transform to convert XML into an S-Expression
;;; that can be read in  by Emacs Lisp.

;;; Pros: xsltproc is extremely fast, and by the time Emacs gets
;;; its  hands on the stream, it's in a form that Lisp can read
;;; in very efficiently.

;;; Cons: Requires an external process to launch xsltproc 
;;; Cons: At present the stylesheet quitely drops double-quote
;;; '"' characters in element content (to be fixed)

;;}}}
;;{{{ required modules

;;; Code:
(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'custom)

;;}}}
;;{{{ customizations

(defgroup xml-sexp nil
  "Convert XML structures to Lisp S-Expressions.")

(defcustom xml-sexp-xslt-program "xsltproc"
  "Program that runs XSLT processor."
  :type 'string
  :group 'xml-sexp)
(declaim (special emacspeak-xslt-directory))
(defcustom xml-sexp-transform
  (expand-file-name "xml2sexp.xsl"
                    emacspeak-xslt-directory)
  "XSL transform that converts XML structures to Lisp
S-Expressions."
  :type 'file
  :group 'xml-sexp)

;;}}}
;;{{{ Commands
;;;###autoload
(defun xml-sexp-read-file (filename)
  "Return S-Expression  from parsing XML file."
  (declare (special xml-sexp-xslt-program xml-sexp-transform))
  (let ((buffer (get-buffer-create " xml-sexp")))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (shell-command
       (format "%s %s %s"
               xml-sexp-xslt-program
               xml-sexp-transform
               filename)
       (current-buffer))
      (goto-char (point-min))
      (read (current-buffer)))))

;;;###autoload
(defun xml-sexp-read-region (start end)
  "Return S-Expression from parsing region of XML."
  (declare (special xml-sexp-xslt-program xml-sexp-transform))
  (let ((buffer (get-buffer-create " xml-sexp")))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer))
    (shell-command-on-region start end 
			     (format "%s %s -"
				     xml-sexp-xslt-program
				     xml-sexp-transform)
			     buffer)
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (read (current-buffer)))))

;;}}}

(provide 'xml-sexp)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
