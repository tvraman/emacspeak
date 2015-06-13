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

;;;Copyright (C) 1995 -- 2015, T. V. Raman
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
;;; NPR == http://www.npr.org National Public Radio in the US
;;; It provides a simple Web  API http://www.npr.org/api/index
;;; This module implements an Emacspeak Npr client.

;;; For now, users will need to get their own API key

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'g-utils)
(require 'emacspeak-webutils)
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

(defvar emacspeak-npr-user-id nil
  "Npr user Id. Not used at present")

;;}}}
;;{{{ Variables:

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

(defsubst emacspeak-npr-get-xml (command)
  "Run command and return its output."
  (declare (special shell-file-name shell-command-switch))
  (g-using-scratch
   (call-process shell-file-name nil t
                 nil shell-command-switch
                 command)
   (buffer-string)))

(defvar emacspeak-npr-last-action-uri nil
  "Cache last API call URI.")

;;;###autoload
(defun emacspeak-npr-view (operation operand)
  "View results as Atom."
  (let* ((url
          (emacspeak-npr-rest-endpoint
           operation
           (format "%s&output=atom" operand))))
    (emacspeak-webutils-autospeak)
    (emacspeak-feeds-atom-display url)))

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
         (label(completing-read "Listing: " emacspeak-npr-listing-table nil t)))
    (cdr (assoc label emacspeak-npr-listing-table))))

(defun emacspeak-npr-listing-url-executor (url &optional get-date)
  "Special executor for use in NPR  listings.
Optional prefix arg prompts for date."
  (interactive "sURL: \nP")
  (emacspeak-webutils-autospeak)
  (emacspeak-feeds-atom-display
   (emacspeak-npr-rest-endpoint
    "query"
    (format
     "id=%s&output=atom%s"
     (file-name-nondirectory url)
     (if get-date
         (concat "&date=" (emacspeak-speak-year-month-date))
       "")))))

(defun emacspeak-npr-search (query)
  "Search NPR"
  (interactive "sTerm: ")
  (emacspeak-feeds-atom-display
   (emacspeak-npr-rest-endpoint "query"
                                (format"searchTerm=%s&output=Atom" query))))

;;;###autoload
(defun emacspeak-npr-listing (&optional search)
  "Display specified listing.
Interactive prefix arg prompts for search."
  (interactive "P")
  (cond
   (search (call-interactively #'emacspeak-npr-search))
   (t
    (let ((key (emacspeak-npr-get-listing-key)))
      (add-hook
       'emacspeak-web-post-process-hook
       #'(lambda ()
           (declare (special emacspeak-we-url-executor))
           (setq emacspeak-we-url-executor
                 'emacspeak-npr-listing-url-executor)
           (emacspeak-speak-buffer)))
      (emacspeak-xslt-view-xml
       (emacspeak-xslt-get  "npr-list.xsl")
       (emacspeak-npr-rest-endpoint "list"
                                    (format "id=%s&output=atom" key)))))))

;;}}}
;;{{{ Play Programs Directly:

(defvar emacspeak-npr-program-table nil
  "Cache mapping NPR program names to program ids.")

(defun emacspeak-npr-refresh-program-table (&optional force)
  "Refresh program table cache if needed."
  (interactive "P")
  (declare (special emacspeak-npr-program-table))
  (when (or (null emacspeak-npr-program-table) force)
    (let* ((url
            (emacspeak-npr-rest-endpoint "list" (format "id=3004&output=json")))
           (json
            (g-json-get 'item
                        (g-json-get-result
                         (format  "%s %s '%s'"
                                  g-curl-program g-curl-common-options url)))))

      (loop
       for p  across json do
       (push
        (list
         (g-json-lookup "title.$text"  p)
         (g-json-get 'id p))
        emacspeak-npr-program-table)))))

(defsubst emacspeak-npr-read-program-id ()
  "Interactively read program id with completion."
  (declare (special emacspeak-npr-program-table))
  (or emacspeak-npr-program-table (emacspeak-npr-refresh-program-table))
  (let ((completion-ignore-case t))
    (cadr
     (assoc
      (completing-read
       "NPR Program: "
       emacspeak-npr-program-table nil t)
      emacspeak-npr-program-table))))

(defun emacspeak-npr-play-program (pid &optional get-date)
  "Play specified NPR program.
Optional interactive prefix arg prompts for a date."
  (interactive
   (list
    (emacspeak-npr-read-program-id)
    current-prefix-arg))
  (let* ((emacspeak-speak-messages nil)
         (date (and get-date (emacspeak-speak-read-date-year/month/date)))
         (url
          (emacspeak-npr-rest-endpoint
           "query"
           (format "id=%s&output=json%s" pid
                   (if get-date (concat "&date=" date) ""))))
         (listing
          (g-json-get-result
           (format "%s %s '%s'"
                   g-curl-program g-curl-common-options url)))
         (stories (g-json-lookup "list.story" listing))
         (playlist (make-temp-file "npr" nil ".m3u"))
         (target nil))
    (loop
     for s across stories do
     (setq
      target
      (g-json-lookup
       "$text"
       (aref
        (g-json-lookup "format.mp3"
                       (aref (g-json-get 'audio s) 0))
        0)))
     (message target)
     (shell-command
      (format "%s --verbose '%s' >> %s"
              g-curl-program target playlist))
     (shell-command
      (format "echo ' ' >> %s" playlist)))
    (emacspeak-m-player playlist 'playlist)))

;;}}}
(provide 'emacspeak-npr)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
