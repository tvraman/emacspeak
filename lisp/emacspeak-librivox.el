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
(require 'emacspeak-table-ui)
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
(provide 'emacspeak-librivox)
;;{{{ Librivox Mode:

(define-derived-mode emacspeak-librivox-mode emacspeak-table-mode
  "Librivox Library Of Free Audio Books"
  "A Librivox front-end for the Emacspeak Audio Desktop."
  (progn
    (declare (special emacspeak-table-speak-row-filter))
    (setq tab-width 12)
    (setq emacspeak-table-speak-row-filter
          '(0 " by " 5))))

(define-prefix-command 'emacspeak-librivox-searcher)

(defun emacspeak-librivox-setup-keys ()
  "Set up Librivox keys."
  (declare (special emacspeak-librivox-mode-map))
  (loop for binding in
        '(
          ("\C-m" emacspeak-librivox-open-rss)
          ("S" emacspeak-librivox-searcher)
          )
        do
        (emacspeak-keymap-update emacspeak-librivox-mode-map  binding))
  (loop for key in
        '(
          ("a" emacspeak-librivox-search-author)
          ("t" emacspeak-librivox-search-title)
          ("g" emacspeak-librivox-search-genre))
        do
        (emacspeak-keymap-update emacspeak-librivox-searcher key)))

(emacspeak-librivox-setup-keys)
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

;;}}}
;;{{{ User Actions:
(defvar  emacspeak-librivox-fields
  '(
    ("ProjectName" . 0)
    ("LibrivoxURL" . 1)
    ("RssURL" . 2)
    ("Category" . 3)
    ("Genre" . 4)
    ("Author1" . 5)
    ("Author2" . 6)
    ("Author3" . 7)
    ("Author4" . 8)
    ("Translator" . 9)
    ("Language" . 10)
    ("Type" . 11))
  "Association list of Librivox Catalog fields.")

(defsubst emacspeak-librivox-field-position (name)
  "Return column for specified field."
  (declare (special emacspeak-librivox-fields))
  (cdr (assoc name  emacspeak-librivox-fields)))

;;;###autoload
(defun emacspeak-librivox-open-rss ()
  "Open RSS  link for current Librivox book."
  (interactive)
  (declare (special emacspeak-table))
  (unless
      (and (eq major-mode 'emacspeak-librivox-mode)
           (boundp 'emacspeak-table)
           emacspeak-table)
    (error "Not in a valid Emacspeak table."))
  (let ((rss (emacspeak-table-this-element
              emacspeak-table
              (emacspeak-table-current-row emacspeak-table)
              (emacspeak-librivox-field-position "RssURL"))))
    (emacspeak-webutils-rss-display rss)))

;;;###autoload

(defun emacspeak-librivox-search-author (pattern)
  "Search in catalog for Author 1."
  (interactive "sAuthor 1")
  (declare (special emacspeak-table))
  (let*((column (emacspeak-librivox-field-position "Author1"))
        (row
         (emacspeak-table-find-match-in-column
          emacspeak-table column pattern 'string-match)))
    (emacspeak-table-goto row column)
    (call-interactively  emacspeak-table-speak-element)))
;;;###autoload
(defun emacspeak-librivox-search-title (pattern)
  "Search in catalog for title."
  (interactive "sTitle")
  (declare (special emacspeak-table))
  (let*((column (emacspeak-librivox-field-position "ProjectName"))
        (row
         (emacspeak-table-find-match-in-column
          emacspeak-table column pattern 'string-match)))
    (emacspeak-table-goto row column)
    (call-interactively  emacspeak-table-speak-element)))

;;;###autoload
(defun emacspeak-librivox-search-genre (pattern)
  "Search in catalog for genre."
  (interactive "sGenre")
  (declare (special emacspeak-table))
  (let*((column (emacspeak-librivox-field-position "ProjectName"))
        (row
         (emacspeak-table-find-match-in-column
          emacspeak-table column pattern 'string-match)))
    (emacspeak-table-goto row column)
    (call-interactively  emacspeak-table-speak-element)))


;;}}}
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
