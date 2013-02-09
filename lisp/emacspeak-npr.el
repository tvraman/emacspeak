;;; emacspeak-npr.el --- Speech-enabled  NPR client
;;; $Id: emacspeak-npr.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable NPR An Emacs Interface to npr
;;; Keywords: Emacspeak,  Audio Desktop npr
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;;Copyright (C) 1995 -- 2011, T. V. Raman
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
;;; MERCHANTABILITY or FITNNPR FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; NPR == http://wwwnpr.org National Public Radio in the US
;;; It provides a simple Web  API http://www.npr.org/api/
;;; This module implements an Emacspeak Npr client.

;;; For now, users will need to get their own API key

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'emacspeak-webutils)
(require 'xml-parse)
(require 'xml)

;;}}}
;;{{{ Customizations

(defgroup emacspeak-npr nil
  "Npr Access on the Complete Audio Desktop."
  :group 'emacspeak)

(defcustom emacspeak-npr-api-key nil
  "Web API  key for this application."
  :type
  '(choice :tag "Key: "
           (const :tag "Unspecified" nil)
           (string :tag "API Key: "))
  :group 'emacspeak-npr)

(defcustom emacspeak-npr-user-id nil
  "Npr user Id."
  :type '(choice :tag "Npr User id"
                 (const :tag "None" nil)
                 (string :tag "Email"))
  :group 'emacspeak-npr)

;;}}}
;;{{{ Variables:

(defvar emacspeak-npr-curl-program (executable-find "curl")
  "Curl executable.")

(defvar emacspeak-npr-curl-common-options
  " --silent "
  "Common Curl options for Npr. ")

(defvar emacspeak-npr-api-base
  "http://api.npr.org"
  "Base REST end-point for Npr API  access.")

;;}}}
;;{{{ Helpers:

;;; beware: when using curl, npr.org wants apiKey first (WHY?)
(defsubst emacspeak-npr-rest-endpoint (operation operand )
  "Return  URL  end point for specified operation."
  (declare (special emacspeak-npr-api-base
                    emacspeak-npr-api-key))
  (format "%s/%s?apiKey=%s&%s"
          emacspeak-npr-api-base operation emacspeak-npr-api-key operand))

(defvar emacspeak-npr-scratch-buffer " *Npr Scratch* "
  "Scratch buffer for Npr operations.")

(defmacro emacspeak-npr-using-scratch(&rest body)
  "Evaluate forms in a  ready to use temporary buffer."
  `(let ((buffer (get-buffer-create emacspeak-npr-scratch-buffer))
         (default-process-coding-system (cons 'utf-8 'utf-8))
         (coding-system-for-read 'binary)
         (coding-system-for-write 'binary)
         (buffer-undo-list t))
     (save-excursion
       (set-buffer buffer)
       (kill-all-local-variables)
       (erase-buffer)
       (progn ,@body))))

(defsubst emacspeak-npr-get-xml (command)
  "Run command and return its output."
  (declare (special shell-file-name shell-command-switch))
  (emacspeak-npr-using-scratch
   (call-process shell-file-name nil t
                 nil shell-command-switch
                 command)
   (buffer-string)))

(defsubst emacspeak-npr-get-result (command)
  "Run command and return its parsed XML output."
  (declare (special shell-file-name shell-command-switch))
  (emacspeak-npr-using-scratch
   (call-process shell-file-name nil t
                 nil shell-command-switch
                 command)
   (goto-char (point-min))
   (read-xml)))

(defvar emacspeak-npr-last-action-uri nil
  "Cache last API call URI.")

(defun emacspeak-npr-api-call (operation operand)
  "Make a Npr API  call and get the result."
  (declare (special emacspeak-npr-last-action-uri))
  (setq emacspeak-npr-last-action-uri
        (emacspeak-npr-rest-endpoint operation operand))
  (emacspeak-npr-get-result
   (format
    "%s %s '%s'  2>/dev/null"
    emacspeak-npr-curl-program
    emacspeak-npr-curl-common-options
    emacspeak-npr-last-action-uri)))
;;;###autoload
(defun emacspeak-npr-view (operation operand)
  "View results as Atom."
  (interactive "sOperation:\nsOperands")
  (let* ((url
          (emacspeak-npr-rest-endpoint
           operation
           (format "%s&output=atom" operand))))
    (emacspeak-webutils-autospeak)
    (emacspeak-webutils-atom-display url)))

;;}}}
;;{{{ program index

;;; Found using documentation at 
;;; http://www.npr.org/api/inputReference.php
;;; All Programs : http://api.npr.org/list?id=3004

(defvar emacspeak-npr-listing-table
  '(("Programs"   . 3004)
    ("Topics" . 3002)
    ("Topics And Music Genres" .  3218)
    ("Music Genres" . 3018)
    ("Music Artists" .  3008)
    ("Columns" . 3003)
    ("Series" . 3006)
    ("Blogs" . 3013)
    ("Bios"   . 3007))
  "Association table of listing keys.
Generated from http://www.npr.org/api/inputReference.php")

(defsubst emacspeak-npr-get-listing-key ()
  "Prompt for and return listing key."
  (let* ((completion-ignore-case t)
         (label(completing-read "Listing: " emacspeak-npr-listing-table)))
    (cdr (assoc label emacspeak-npr-listing-table))))

(defun emacspeak-npr-listing-url-executor (url)
  "Special executor for use in NPR  listings."
  (interactive "sURL: ")
  (emacspeak-webutils-atom-display
   (emacspeak-npr-rest-endpoint "query"
                                (format "id=%s&output=atom"
                                        (file-name-nondirectory url)))))

;;;###autoload    
(defun emacspeak-npr-listing ()
  "Display specified listing."
  (interactive)
  (let ((key (emacspeak-npr-get-listing-key)))
    (add-hook
     'emacspeak-web-post-process-hook
     #'(lambda ()
         (declare (special emacspeak-we-url-executor))
         (setq emacspeak-we-url-executor
               'emacspeak-npr-listing-url-executor)
         (emacspeak-speak-buffer)))
    (emacspeak-xslt-view-xml
     (expand-file-name "npr-list.xsl" emacspeak-xslt-directory)
     (emacspeak-npr-rest-endpoint "list"
                                  (format "id=%s&output=atom" key)))))

;;}}}
(provide 'emacspeak-npr)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
