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
;;;  $Revision: 24.0 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2004, T. V. Raman 
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
  "XSL transformation group."
  :group 'emacspeak)

;;;###autoload 
(defvar emacspeak-xslt-directory
  (expand-file-name "xsl/" emacspeak-directory)
  "Directory holding XSL transformations.")

(defcustom emacspeak-xslt-program "xsltproc"
  "Name of XSLT transformation engine."
  :type 'string
  :group 'emacspeak-xslt)
;;;###autoload
(defcustom emacspeak-xslt-options
  "--html --nonet --novalid"
  "Options passed to xsltproc."
  :type 'string
  :group 'emacspeak-xslt)

(defcustom emacspeak-xslt-keep-errors  nil
  "If non-nil, xslt errors will be preserved in an errors buffer."
  :type 'boolean
  :group 'emacspeak-xslt)

(defcustom emacspeak-xslt-nuke-null-char t
  "If T null chars in the region will be nuked.
This is useful when handling bad HTML."
  :type 'boolean
  :group 'emacspeak-xslt)

;;;###autoload
(defun emacspeak-xslt-region (xsl start end &optional params )
  "Apply XSLT transformation to region and replace it with
the result.  This uses XSLT processor xsltproc available as
part of the libxslt package."
  (declare (special emacspeak-xslt-program emacspeak-xslt-options
                    emacspeak-xslt-nuke-null-char
                    emacspeak-xslt-keep-errors
                    modification-flag ))
  (let ((command nil)
        (parameters (when params 
                      (mapconcat 
                       #'(lambda (pair)
                           (format "--param %s %s "
                                   (car pair)
                                   (cdr pair)))
                       params
                       " "))))
    (setq command (format
                   "%s %s  %s  %s - %s"
                   emacspeak-xslt-program
                   emacspeak-xslt-options
                   (or parameters "")
                   xsl
                   (if emacspeak-xslt-keep-errors
                       ""
                     " 2>/dev/null ")))
    (when emacspeak-xslt-nuke-null-char
      (goto-char start)
      (while (search-forward
              ( format "%c" 0)
              end t)
        (replace-match " ")))
    (shell-command-on-region start end
                             command 
                             (current-buffer)
                             'replace
                             (when emacspeak-xslt-keep-errors
                               "*xslt errors*"))
    (when (get-buffer  "*xslt errors*")
      (bury-buffer "*xslt errors*"))
    (goto-char (point-max))
    (insert
     (format "<!--\n %s \n-->\n"
             command))
    (setq modification-flag nil)))
;;; uses wget in a pipeline to avoid libxml2 bug:
;;;###autoload
(defcustom  emacspeak-xslt-use-wget-to-download nil
  "Set to T if you want to avoid URL downloader bugs in libxml2.
There is a bug that bites when using Yahoo Maps that wget can
work around."
  :group 'emacspeak-xslt
  :type 'boolean)

;;;###autoload
(defun emacspeak-xslt-url (xsl url &optional params dont-add-command-as-comment)
  "Apply XSLT transformation to url
and return the results in a newly created buffer.
  This uses XSLT processor xsltproc available as
part of the libxslt package."
  (declare (special emacspeak-xslt-program
                    emacspeak-xslt-use-wget-to-download
                    modification-flag
                    emacspeak-xslt-keep-errors))
  (let ((result (get-buffer-create " *xslt result*"))
        (command nil)
        (parameters (when params 
                      (mapconcat 
                       #'(lambda (pair)
                           (format "--param %s %s "
                                   (car pair)
                                   (cdr pair)))
                       params
                       " "))))
    (if emacspeak-xslt-use-wget-to-download
        (setq command (format
                       "wget -q -O - '%s' | %s %s    --html --novalid %s '%s' %s"
                       url
                       emacspeak-xslt-program
                       (or parameters "")
                       xsl "-"
                       (if emacspeak-xslt-keep-errors
                           ""
                         " 2>/dev/null ")))
      (setq command (format
                     "%s %s    --html --novalid %s '%s' %s"
                     emacspeak-xslt-program
                     (or parameters "")
                     xsl url
                     (if emacspeak-xslt-keep-errors
                         ""
                       " 2>/dev/null "))))
    (save-excursion
      (set-buffer result)
      (erase-buffer)
      (shell-command command (current-buffer)
                     (when emacspeak-xslt-keep-errors
                       "*xslt errors*"))
      (when emacspeak-xslt-nuke-null-char
        (goto-char (point-min))
        (while (search-forward
                ( format "%c" 0)
                nil  t)
          (replace-match " ")))
      (when (get-buffer  "*xslt errors*")
        (bury-buffer "*xslt errors*"))
      (unless  dont-add-command-as-comment
        (goto-char (point-max))
        (insert
         (format "<!--\n %s \n-->\n"
                 command)))
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
        (command nil)
        (parameters (when params 
                      (mapconcat 
                       #'(lambda (pair)
                           (format "--param %s %s "
                                   (car pair)
                                   (cdr pair)))
                       params
                       " "))))
    (setq command (format
                   "%s %s    --novalid %s '%s' %s"
                   emacspeak-xslt-program
                   (or parameters "")
                   xsl url
                   (if emacspeak-xslt-keep-errors
                       ""
                     " 2>/dev/null ")))
    (save-excursion
      (set-buffer result)
      (erase-buffer)
      (shell-command command
                     (current-buffer)
                     (when emacspeak-xslt-keep-errors
                       "*xslt errors*"))
      (when (get-buffer  "*xslt errors*")
        (bury-buffer "*xslt errors*"))
      (goto-char (point-max))
      (insert
       (format "<!--\n %s \n-->\n"
               command))
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
