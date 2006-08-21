;;; emacspeak-xslt.el --- Implements Emacspeak  xslt transform engine
;;; $Id$
;;; $Author$
;;; Description:  xslt transformation routines 
;;; Keywords: Emacspeak,  Audio Desktop XSLT
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
;;;Copyright (C) 1995 -- 2003, T. V. Raman 
;;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
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

;;{{{  introduction

;;; libxml and libxsl are XML libraries for GNOME.
;;; xsltproc is a  xslt processor using libxsl
;;; this module defines routines for applying xsl transformations
;;; using xsltproc

;;}}}
;;{{{  Required modules

(require 'emacspeak-preamble)

;;}}}
;;{{{  xslt 

(defgroup emacspeak-xslt nil
  "XSL transformation group.")
;;;###autoload 
(defvar emacspeak-xslt-directory
  (expand-file-name "xsl/" emacspeak-directory)
  "Directory holding XSL transformations.")

(defcustom emacspeak-xslt-program "xsltproc"
  "Name of XSLT transformation engine."
  :type 'string
  :group 'emacspeak-xslt)

(defcustom emacspeak-xslt-keep-errors  nil
  "If non-nil, xslt errors will be preserved in an errors buffer."
  :type 'boolean
  :group 'emacspeak-wizards)
;;;###autoload
(defun emacspeak-xslt-region (xsl start end &optional params )
  "Apply XSLT transformation to region and replace it with
the result.  This uses XSLT processor xsltproc available as
part of the libxslt package."
  (declare (special emacspeak-xslt-program
                    emacspeak-xslt-keep-errors
                    modification-flag ))
  (let ((parameters (when params 
                      (mapconcat 
                       #'(lambda (pair)
                           (format "--param %s %s "
                                   (car pair)
                                   (cdr pair)))
                       params
                       " "))))
    (shell-command-on-region start end
                             (format
                              "%s %s  --html --nonet --novalid %s - %s"
			      emacspeak-xslt-program
			      (or parameters "")
			      xsl
			      (if emacspeak-xslt-keep-errors
				  ""
				" 2>/dev/null "))
                             (current-buffer)
                             'replace
                             (when emacspeak-xslt-keep-errors
			       "*xslt errors*"))
    (when (get-buffer  "*xslt errors*")
      (bury-buffer "*xslt errors*"))
    (setq modification-flag nil)))
;;;###autoload
(defun emacspeak-xslt-url (xsl url &optional params)
  "Apply XSLT transformation to url
and return the results in a newly created buffer.
  This uses XSLT processor xsltproc available as
part of the libxslt package."
  (declare (special emacspeak-xslt-program
                    modification-flag
                    emacspeak-xslt-keep-errors))
  (let ((result (get-buffer-create " *xslt result*"))
        (parameters (when params 
                      (mapconcat 
                       #'(lambda (pair)
                           (format "--param %s %s "
                                   (car pair)
                                   (cdr pair)))
                       params
                       " "))))
    (save-excursion
      (set-buffer result)
      (erase-buffer)
      (shell-command
       (format
        "%s %s    --html --novalid %s '%s' %s"
	emacspeak-xslt-program
	(or parameters "")
	xsl url
	(if emacspeak-xslt-keep-errors
	    ""
	  " 2>/dev/null "))
       (current-buffer)
       (when emacspeak-xslt-keep-errors
         "*xslt errors*"))
      (when (get-buffer  "*xslt errors*")
        (bury-buffer "*xslt errors*"))
      (setq modification-flag nil)
      (goto-char (point-min))
      result)))
;;;###autoload
(defun emacspeak-xslt-xml-url (xsl url &optional params)
  "Apply XSLT transformation to XML url
and return the results in a newly created buffer.
  This uses XSLT processor xsltproc available as
part of the libxslt package."
  (declare (special emacspeak-xslt-program
                    modification-flag
                    emacspeak-xslt-keep-errors))
  (let ((result (get-buffer-create " *xslt result*"))
        (parameters (when params 
                      (mapconcat 
                       #'(lambda (pair)
                           (format "--param %s %s "
                                   (car pair)
                                   (cdr pair)))
                       params
                       " "))))
    (save-excursion
      (set-buffer result)
      (erase-buffer)
      (shell-command
       (format
        "%s %s    --novalid %s '%s' %s"
	emacspeak-xslt-program
	(or parameters "")
	xsl url
	(if emacspeak-xslt-keep-errors
	    ""
	  " 2>/dev/null "))
       (current-buffer)
       (when emacspeak-xslt-keep-errors
         "*xslt errors*"))
      (when (get-buffer  "*xslt errors*")
        (bury-buffer "*xslt errors*"))
      (setq modification-flag nil)
      (goto-char (point-min))
      result)))

;;}}}
(provide 'emacspeak-xslt)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
