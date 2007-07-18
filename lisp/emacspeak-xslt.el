;;; emacspeak-xslt.el --- Implements Emacspeak  xslt transform engine
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  xslt transformation routines
;;; Keywords: Emacspeak,  Audio Desktop XSLT
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-05-16 14:53:09 -0700 (Wed, 16 May 2007) $ |
;;;  $Revision: 4562 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2007, T. V. Raman
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
(require 'emacspeak-webutils)

;;}}}
;;{{{  xslt Environment:

(defgroup emacspeak-xslt nil
  "XSL transformation group."
  :group 'emacspeak)

(defsubst emacspeak-xslt-params-from-xpath (path base)
  "Return params suitable for passing to  emacspeak-xslt-region"
  (list
   (cons "path"
         (format "\"'%s'\""
                 (shell-quote-argument path)))
   (cons "locator"
         (format "'%s'"
                 path))
   (cons "base"
         (format "\"'%s'\""
                 base))))

(defsubst emacspeak-xslt-get (style)
  "Return fully qualified stylesheet path."
  (declare (special emacspeak-xslt-directory))
  (expand-file-name style emacspeak-xslt-directory))

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

;;}}}
;;{{{ Functions:

;;;###autoload
(defun emacspeak-xslt-region (xsl start end &optional params)
  "Apply XSLT transformation to region and replace it with
the result.  This uses XSLT processor xsltproc available as
part of the libxslt package."
  (declare (special emacspeak-xslt-program emacspeak-xslt-options
                    emacspeak-xslt-keep-errors modification-flag ))
  (let ((command nil)
        (default-process-coding-system (cons 'utf-8 'utf-8))
        (parameters (when params
                      (mapconcat
                       #'(lambda (pair)
                           (format "--param %s %s "
                                   (car pair)
                                   (cdr pair)))
                       params
                       " "))))
    (setq command
          (format
           "%s %s  %s  %s - %s"
           emacspeak-xslt-program
           (or emacspeak-xslt-options "")
           (or parameters "")
           xsl
           (if emacspeak-xslt-keep-errors
               ""
             " 2>/dev/null ")))
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
    (setq modification-flag nil)
    (current-buffer)))

;;;###autoload
(defun emacspeak-xslt-view-region (style start end)
  "View region after transforming via XSLT."
  (interactive
   (list
    (expand-file-name
     (read-file-name "XSL: "
                     emacspeak-xslt-directory))
    (point)
    (mark)))
  (let* ((emacspeak-xslt-options nil)
    (buffer
         (ems-modify-buffer-safely
         (emacspeak-xslt-region style start end ))))
    (browse-url-of-buffer buffer)))

;;; uses wget in a pipeline to avoid libxml2 bug:
;;;###autoload
(defcustom  emacspeak-xslt-use-wget-to-download nil
  "Set to T if you want to avoid URL downloader bugs in libxml2.
There is a bug that bites when using Yahoo Maps that wget can
work around."
  :group 'emacspeak-xslt
  :type 'boolean)

;;;###autoload
(defun emacspeak-xslt-url (xsl url &optional params)
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
        (default-process-coding-system (cons 'utf-8 'utf-8))
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
                       "wget -U mozilla -q -O - '%s' | %s %s    --html --novalid %s '%s' %s"
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
      (kill-all-local-variables)
      (erase-buffer)
      (setq buffer-undo-list t)
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
      (goto-char (point-max))
        (insert
         (format "<!--\n %s \n-->\n"
                 command))
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
                    emacspeak-xslt-use-wget-to-download
                    modification-flag
                    emacspeak-xslt-keep-errors))
  (let ((result (get-buffer-create " *xslt result*"))
        (command nil)
        (default-process-coding-system (cons 'utf-8 'utf-8))
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
                       "wget -q -O - '%s' | %s %s --novalid %s %s %s"
                       url
                       emacspeak-xslt-program
                       (or parameters "")
                       xsl "-"
                       (if emacspeak-xslt-keep-errors
                           ""
                         " 2>/dev/null ")))
      (setq command
            (format
             "%s %s --novalid %s '%s' %s"
             emacspeak-xslt-program
             (or parameters "")
             xsl url
             (if emacspeak-xslt-keep-errors
                 ""
               " 2>/dev/null "))))
    (save-excursion
      (set-buffer result)
      (kill-all-local-variables)
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
;;{{{ interactive commands:
;;;###autoload
(defun emacspeak-xslt-view (style url)
  "Browse URL with specified XSL style."
  (interactive
   (list
    (expand-file-name
     (read-file-name "XSL Transformation: "
                     emacspeak-xslt-directory))
    (read-string "URL: " (browse-url-url-at-point))))
  (emacspeak-webutils-with-xsl style url))

;;;###autoload
(defun emacspeak-xslt-view-xml (style url &optional unescape-charent)
  "Browse XML URL with specified XSL style."
  (interactive
   (list
    (expand-file-name
     (read-file-name "XSL Transformation: "
                     emacspeak-xslt-directory))
    (read-string "URL: " (browse-url-url-at-point))
    current-prefix-arg))
  (let ((src-buffer
         (emacspeak-xslt-xml-url
          style
          url
          (list
           (cons "base"
                 (format "\"'%s'\""
                         url))))))
    (add-hook 'emacspeak-w3-post-process-hook
              #'(lambda nil
                  (emacspeak-speak-mode-line)
                  (emacspeak-auditory-icon 'open-object)))
    (save-excursion
      (set-buffer src-buffer)
      (when unescape-charent
        (emacspeak-webutils-unescape-charent (point-min) (point-max)))
      (emacspeak-webutils-without-xsl
       (browse-url-of-buffer)))
    (kill-buffer src-buffer)))

;;;###autoload
(defun emacspeak-xslt-view-region (style start end &optional unescape-charent)
  "Browse XML region with specified XSL style."
  (interactive
   (list
    (expand-file-name
     (read-file-name "XSL Transformation: "
                     emacspeak-xslt-directory))
    (point)
    (mark)
    current-prefix-arg))
  (let ((src-buffer
         (ems-modify-buffer-safely
          (emacspeak-xslt-region style start end))))
    (save-excursion
      (set-buffer src-buffer)
      (when unescape-charent
        (emacspeak-webutils-unescape-charent (point-min) (point-max)))
      (emacspeak-webutils-without-xsl
       (browse-url-of-buffer)))
    (kill-buffer src-buffer)))

;;}}}
(provide 'emacspeak-xslt)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
