;;; emacspeak-librivox.el --- Speech-enabled  LIBRIVOX API client
;;; $Id: emacspeak-librivox.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable LIBRIVOX An Emacs Interface to Free Audio Books 
;;; Keywords: Emacspeak,  Audio Desktop librivox
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
;;; MERCHANTABILITY or fitness FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; LIBRIVOX == http://wwwlibrivox.org Free Audio Books
;;; It provides a simple Web  API http://wiki.librivox.org/index.php/LibriVoxAPI
;;; This module implements an Emacspeak Librivox client.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'emacspeak-table)
(require 'derived)

;;}}}
;;{{{ Customizations

(defgroup emacspeak-librivox nil
  "Librivox Access on the Complete Audio Desktop."
  :group 'emacspeak)




(defcustom emacspeak-librivox-catalog-location
  (expand-file-name "librivox/catalog.csv"
  emacspeak-resource-directory)
  "Location where we cache the librivox catalog.")

;;}}}
;;{{{ Variables:

(defvar emacspeak-librivox-curl-program (executable-find "curl")
  "Curl executable.")

(defvar emacspeak-librivox-curl-common-options
  " --silent "
  "Common Curl options for Librivox. ")

(defvar emacspeak-librivox-api-base
  "http://librivox.org/newcatalog/"
  "Base REST end-point for Librivox API  access.")
(defvar emacspeak-librivox-buffer-name
  "*Librivox Interaction*"
  "Name of Librivox interaction buffer.")

;;}}}
;;{{{ Helpers:

(defun emacspeak-librivox-fetch-catalog ()
  "Fetch catalog to our cache location."
  (declare (special emacspeak-librivox-api-base
                    emacspeak-librivox-catalog-location))
  (let ((dir  (file-name-directory emacspeak-librivox-catalog-location)))
    (unless (file-exists-p dir)
      (make-directory dir 'parents)))
  (shell-command
   (format "%s %s %s > %s 2>/dev/null"
           emacspeak-librivox-curl-program
           emacspeak-librivox-curl-common-options
           (format "%s%s"
                   emacspeak-librivox-api-base
           "csv.php")
           emacspeak-librivox-catalog-location)))

;;}}}
;;{{{ program index

;;; Found using documentation at 
;;; http://www.librivox.org/api/inputReference.php
;;; All Programs : http://api.librivox.org/list?id=3004

(defvar emacspeak-librivox-listing-table
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
Generated from http://www.librivox.org/api/inputReference.php")

(defsubst emacspeak-librivox-get-listing-key ()
  "Prompt for and return listing key."
  (let* ((completion-ignore-case t)
         (label(completing-read "Listing: " emacspeak-librivox-listing-table)))
    (cdr (assoc label emacspeak-librivox-listing-table))))

(defun emacspeak-librivox-listing-url-executor (url)
  "Special executor for use in LIBRIVOX  listings."
  (interactive "sURL: ")
  (emacspeak-webutils-atom-display
   (emacspeak-librivox-rest-endpoint "query"
                                (format "id=%s&output=atom"
                                        (file-name-nondirectory url)))))

;;;###autoload    
(defun emacspeak-librivox-listing ()
  "Display specified listing."
  (interactive)
  (let ((key (emacspeak-librivox-get-listing-key)))
    (add-hook
     'emacspeak-web-post-process-hook
     #'(lambda ()
         (declare (special emacspeak-we-url-executor))
         (setq emacspeak-we-url-executor 'emacspeak-librivox-listing-url-executor)))
    (emacspeak-xslt-view-xml
     (expand-file-name "librivox-list.xsl" emacspeak-xslt-directory)
     (emacspeak-librivox-rest-endpoint "list"
                                  (format "id=%s&output=atom" key)))))

;;}}}
(provide 'emacspeak-librivox)
;;{{{ Librivox Mode:

(define-derived-mode emacspeak-librivox-mode emacspeak-table-mode
  "Librivox Library Of Free Audio Books"
  "A Librivox front-end for the Emacspeak Audio Desktop."
  )
  
;;;###autoload
(defun emacspeak-librivox ()
  "Librivox Library Of Free Audio Books."
  (interactive)
  (declare (special emacspeak-librivox-buffer-name
                    emacspeak-librivox-catalog-location))
  (let ((inhibit-read-only t))
  (unless (file-exists-p emacspeak-librivox-catalog-location)
    (message "Retrieving Librivox catalog, might take a minute.")
    (emacspeak-librivox-fetch-catalog))
  (unless (file-exists-p emacspeak-librivox-catalog-location)
    (error "Cannot find Librivox Catalog."))
  (cond
   ((null (get-buffer emacspeak-librivox-buffer-name))
    (emacspeak-table-find-csv-file
     emacspeak-librivox-catalog-location)
    (rename-buffer emacspeak-librivox-buffer-name)
    (emacspeak-librivox-mode))
   (t (switch-to-buffer emacspeak-librivox-buffer-name)))
  (emacspeak-auditory-icon 'open-object)
  (message "Librivox Interaction")))
      
      
      
      
      
  
  
    
    
    (put-text-property start (point)
                       'face font-lock-doc-face)
    (setq header-line-format "Bookshare Library")
    (cd-absolute emacspeak-bookshare-directory)))

;;}}}
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
