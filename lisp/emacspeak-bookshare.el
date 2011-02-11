;;; emacspeak-bookshare.el --- Speech-enabled  BOOKSHARE client
;;; $Id: emacspeak-bookshare.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable BOOKSHARE An Emacs Interface to bookshare
;;; Keywords: Emacspeak,  Audio Desktop bookshare
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
;;; MERCHANTABILITY or FITNBOOKSHARE FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; BOOKSHARE == http://www.bookshare.org provides book access to print-disabled users.
;;; It provides a simple Web  API http://developer.bookshare.org
;;; This module implements an Emacspeak Bookshare client.
;;; For now, users will need to get their own API key
;;; This might change once I get approval from Bookshare to embed the Emacspeak API  key in the Source code.

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'xml-parse)
(require 'xml)
(require 'derived)
;;}}}
;;{{{ Customizations

(defgroup emacspeak-bookshare nil
  "Bookshare Access on the Complete Audio Desktop."
  :group 'emacspeak)

(defcustom emacspeak-bookshare-api-key nil
  "Web API  key for this application."
  :type
  '(choice :tag "Key: "
           (const :tag "Unspecified" nil)
           (string :tag "API Key: "))
  :group 'emacspeak-bookshare)

(defcustom emacspeak-bookshare-user-id nil
  "Bookshare user Id."
  :type '(choice :tag "Bookshare User id"
                 (const :tag "None" nil)
                 (string :tag "Email"))
  :group 'emacspeak-bookshare)
(defcustom emacspeak-bookshare-directory (expand-file-name "~/")
  "Customize this to the root of where books are organized."
  :type 'directory
  :group 'emacspeak-bookshare)

(defcustom emacspeak-bookshare-downloads-directory
  (expand-file-name "~/" emacspeak-bookshare-directory)
  "Customize this to the root of where books are organized."
  :type 'directory
  :group 'emacspeak-bookshare)

;;}}}
;;{{{ Variables:

(defvar emacspeak-bookshare-curl-program (executable-find "curl")
  "Curl executable.")

(defvar emacspeak-bookshare-curl-common-options
  " --insecure "
  "Common Curl options for Bookshare. Includes --insecure  as per Bookshare docs.")

(defvar emacspeak-bookshare-api-base
  "https://api.bookshare.org"
  "Base end-point for Bookshare API  access.")

;;}}}
;;{{{ Helpers:
(defsubst emacspeak-bookshare-assert ()
  "Error out if not in bookshre mode."
  (unless (eq major-mode 'emacspeak-bookshare-mode)
    (error "Not in Bookshare Interaction.")))
(defvar emacspeak-bookshare-md5-cached-token nil
  "Cache MD5 token for future use.")
(defvar emacspeak-bookshare-password-cache nil
  "Cache user password for this session.")

(defsubst emacspeak-bookshare-user-password ()
  "User password.
Memoize token, and return token encoded using md5, and packaged
with X-password HTTP header for use with Curl."
  (declare (special emacspeak-bookshare-md5-cached-token emacspeak-bookshare-password-cache))
  (setq emacspeak-bookshare-password-cache
        (or  emacspeak-bookshare-password-cache
             (read-passwd
              (format "Bookshare password for %s: "
                      emacspeak-bookshare-user-id))))
  (setq emacspeak-bookshare-md5-cached-token (md5 emacspeak-bookshare-password-cache))
  (format "-H 'X-password: %s'" emacspeak-bookshare-md5-cached-token))

(defsubst emacspeak-bookshare-rest-endpoint (operation operand &optional no-auth)
  "Return  URL  end point for specified operation.
Optional argument `no-auth' says no user auth needed."
  (declare (special emacspeak-bookshare-api-base emacspeak-bookshare-user-id))
  (format "%s/%s/%s/%s?api_key=%s"
          emacspeak-bookshare-api-base
          operation
          operand
          (if no-auth
              ""
            (format "for/%s" emacspeak-bookshare-user-id))
          emacspeak-bookshare-api-key))

(defsubst emacspeak-bookshare-page-rest-endpoint ()
  "Generate REST endpoint for the next page of results."
  (declare (special emacspeak-bookshare-last-action-uri))
  (unless emacspeak-bookshare-last-action-uri
    (error "No query to  page!"))
  (let ((root
         (first (split-string emacspeak-bookshare-last-action-uri "/for")))
        (page nil))
    (setq page (string-match "/page/" root))
    (cond
     (page
      (setq page (split-string root "/page/"))Already paged once
      (format "%s/page/%s/for/%s?api_key=%s"
               (first page)
               (1+ (read (second page)))
               emacspeak-bookshare-user-id
               emacspeak-bookshare-api-key))
      (t
       (format "%s/page/2/for/%s?api_key=%s"
               root
               emacspeak-bookshare-user-id
               emacspeak-bookshare-api-key)))))
    

(defsubst emacspeak-bookshare-destruct-rest-url (url)
  "Return operator and operand used to construct this REST end-point."
  (declare (special emacspeak-bookshare-api-base))
  (let* ((start (length emacspeak-bookshare-api-base))
         (end (string-match "for/" url)))
    (nthcdr 2
            (split-string
             (substring url start end) "/" 'no-null))))

(defsubst emacspeak-bookshare-download-url (id fmt )
  "Return  URL  end point for content download.
Argument id specifies content. Argument fmt = 0 for Braille, 1
  for Daisy."
  (declare (special emacspeak-bookshare-api-base emacspeak-bookshare-user-id))
  (format "%s/%s/%s?api_key=%s"
          emacspeak-bookshare-api-base
          (format "download/content/%s/version/%s" id fmt)
          (format "for/%s" emacspeak-bookshare-user-id)
          emacspeak-bookshare-api-key))

(defvar emacspeak-bookshare-scratch-buffer " *Bookshare Scratch* "
  "Scratch buffer for Bookshare operations.")

(defmacro emacspeak-bookshare-using-scratch(&rest body)
  "Evaluate forms in a  ready to use temporary buffer."
  `(let ((buffer (get-buffer-create emacspeak-bookshare-scratch-buffer))
         (default-process-coding-system (cons 'utf-8 'utf-8))
         (coding-system-for-read 'binary)
         (coding-system-for-write 'binary)
         (buffer-undo-list t))
     (save-excursion
       (set-buffer buffer)
       (kill-all-local-variables)
       (erase-buffer)
       (progn ,@body))))

(defsubst emacspeak-bookshare-get-result (command)
  "Run command and return its output."
  (declare (special shell-file-name shell-command-switch))
  (emacspeak-bookshare-using-scratch
   (call-process shell-file-name nil t
                 nil shell-command-switch
                 command)
   (goto-char (point-min))
   (read-xml)))
(defvar emacspeak-bookshare-last-action-uri nil
  "Cache last API call URI.")



(defun emacspeak-bookshare-api-call (operation operand &optional no-auth)
  "Make a Bookshare API  call and get the result.
Optional argument 'no-auth says we dont need a user auth."
  (declare (special emacspeak-bookshare-last-action-uri))
  (setq emacspeak-bookshare-last-action-uri
        (emacspeak-bookshare-rest-endpoint operation operand no-auth))
  (emacspeak-bookshare-get-result
   (format
    "%s %s %s  %s 2>/dev/null"
    emacspeak-bookshare-curl-program
    emacspeak-bookshare-curl-common-options
    (if no-auth "" (emacspeak-bookshare-user-password))
    emacspeak-bookshare-last-action-uri)))

(defun emacspeak-bookshare-get-more-results ()
  "Get next page of results for last query."
  (interactive)
  (declare (special emacspeak-bookshare-last-action-uri))
  (setq emacspeak-bookshare-last-action-uri
        (emacspeak-bookshare-page-rest-endpoint))
  (emacspeak-bookshare-get-result
   (format "%s %s %s  %s 2>/dev/null"
           emacspeak-bookshare-curl-program
           emacspeak-bookshare-curl-common-options
           (emacspeak-bookshare-user-password)
           emacspeak-bookshare-last-action-uri)))


(defsubst emacspeak-bookshare-generate-target (author title)
  "Generate a suitable filename target."
  (declare (special emacspeak-bookshare-downloads-directory))
  (expand-file-name
   (replace-regexp-in-string
    " " "-"
    (format "%s-%s.zip" author title))
   emacspeak-bookshare-downloads-directory))

(defsubst emacspeak-bookshare-destruct-target (target)
  "Destruct  a  filename target into components."
   (split-string 
    (substring target  0 -4)
"-" 'no-null))
   emacspeak-bookshare-downloads-directory))


(defsubst emacspeak-bookshare-generate-directory (author title)
  "Generate name of unpack directory."
  (declare (special emacspeak-bookshare-directory))
  (expand-file-name
   (replace-regexp-in-string
    " " "-"
    (format "%s/%s" author title))
   emacspeak-bookshare-directory))

)
    (add-text-properties
     (line-beginning-position) (line-end-position)
     (list'face 'highlight
                'auditory-icon 'item))
    (message "Unpacked content.")))
(defvar emacspeak-bookshare-xslt "daisyTransform.xsl"
  "Name of bookshare supplied XSL transform.")

(defun emacspeak-bookshare-view-at-point ()
  "View book at point.
Make sure it's downloaded and unpacked first."
  (interactive)
  (declare (special emacspeak-bookshare-xslt))
  (let* ((target (emacspeak-bookshare-get-target))
         (directory (emacspeak-bookshare-get-directory))
         (title (emacspeak-bookshare-get-title))
         (xsl (expand-file-name emacspeak-bookshare-xslt directory)))
    (unless (file-exists-p target)
      (error "First download this content."))
    (unless (file-exists-p directory)
      (error "First unpack this content."))
    (unless (file-exists-p xsl)
      (error "No suitable XSL  transformation found."))
    (emacspeak-xslt-view-file
     xsl
     (first
      (directory-files directory
                       'full
                       ".xml")))))

(defun emacspeak-bookshare-view (directory)
  "View book in specified directory."
  (interactive
   (list
    (expand-file-name
    (read-directory-name "Book Directory: "
                         emacspeak-bookshare-directory))))
  (declare (special emacspeak-bookshare-xslt
                    emacspeak-bookshare-directory))
  (let* ((xsl (expand-file-name emacspeak-bookshare-xslt directory)))
    (unless (file-exists-p xsl)
      (error "No suitable XSL  transformation found."))
    (emacspeak-xslt-view-file
     xsl
     (first
      (directory-files directory 'full ".xml")))))

(defun emacspeak-bookshare-sign-out ()
  "Sign out, clearing password."
  (interactive)
  (declare (special emacspeak-bookshare-md5-cached-token
                    emacspeak-bookshare-password-cache))
  (setq emacspeak-bookshare-password-cache nil
        emacspeak-bookshare-md5-cached-token nil)
  (emacspeak-auditory-icon 'close-object)
  (message "Signed out from Bookshare."))

;;}}}
;;{{{ Navigation in  Bookshare Interaction

(defun emacspeak-bookshare-next-result ()
  "Move to next result."
  (interactive)
  (goto-char (line-end-position))
  (goto-char (next-single-property-change (point) 'id))
  (emacspeak-auditory-icon 'large-movement)
  (forward-char 1)
  (emacspeak-speak-line))

(defun emacspeak-bookshare-previous-result()
  "Move to previous result."
  (interactive)
  (goto-char (previous-single-property-change (point) 'id))
  (beginning-of-line)
  (emacspeak-auditory-icon 'large-movement)
  (emacspeak-speak-line))

(defun emacspeak-bookshare-flush-lines(regexp)
  "Flush lines matching regexp in Bookshare buffer."
  (interactive "sRegexp: ")
  (save-excursion
    (let ((inhibit-read-only t))
      (flush-lines regexp (point-min) (point-max)))))


;;}}}
(provide 'emacspeak-bookshare)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
