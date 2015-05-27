
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
;;; LIBRIVOX == http://www.librivox.org Free Audio Books
;;; API Info: https://librivox.org/api/info
;;; It provides a simple Web  API
;;; This module implements an Emacspeak Librivox client.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'emacspeak-webutils)
(require 'emacspeak-m-player)
(require 'g-utils)

;;}}}
;;{{{ Variables:

(defvar emacspeak-librivox-buffer-name
  "*Librivox Interaction*"
  "Name of Librivox interaction buffer.")

;;}}}
;;{{{ API:

(defvar emacspeak-librivox-api-base
  "https://librivox.org/api/feed/"
  "Base REST end-point for Librivox API  access.")

;;; audiobooks:
;;; Params from API Documentation:

;; * id - fetches a single record
;; * since - takes a UNIX timestamp; returns all projects cataloged since that time
;; * author - all records by that author last name
;; * title - all matching titles
;; * genre - all projects of the matching genre
;; * extended - =1 will return the full set of data about the project
;; * limit (default is 50)
;;   * offset

(defsubst emacspeak-librivox-audiobooks-uri (pattern)
  "Search URI for audiobooks."
  (declare (special emacspeak-librivox-api-base))
  (concat emacspeak-librivox-api-base "audiobooks?format=json&" pattern))

;;; Audio Tracks API:
;;; Params:
;; * id - of track itself
;; * project_id - all tracks for project

(defsubst emacspeak-librivox-audiotracks-base (pattern)
  "Base URI for audiotracks."
  (declare (special emacspeak-librivox-api-base))
  (concat emacspeak-librivox-api-base "audiotracks?format=json&" pattern))

;;; Simple Authors API:
;;; Params:
;; * id - of author
;; * last_name - exact match

(defsubst emacspeak-librivox-authors-base ()
  "Base URI for authors."
  (declare (special emacspeak-librivox-api-base))
  (concat emacspeak-librivox-api-base "authors"))

;;}}}
;;{{{ Search Commands:

(defsubst emacspeak-librivox-display-author (author)
  "Display single author."
  (insert
   (format "%s, " (g-json-get 'last_name author))
   (format "%s " (g-json-get 'first_name author))
   (format "(%s -- %s \n" (g-json-get 'dob author) (g-json-get 'dod author))
   "<br/>"))

(defun emacspeak-librivox-display-authors (authors)
  "Display authors."
  (insert "\n\n<p>\n")
  (cond
   ((= (length authors)1)
    (emacspeak-librivox-display-author (aref authors 0)))
   (t
    (loop
     for a across authors
     do
     (emacspeak-librivox-display-author a))))
  (insert "</p>\n\n"))

(defun emacspeak-librivox-display-book (book position)
  "Render book results."
  (let ((title (g-json-get 'title book))
        (rss (g-json-get 'url_rss book))
        (zip (g-json-get 'url_zip_file book))
        (text (g-json-get 'url_text_source book))
        (time (g-json-get 'totaltime book))
        (desc (g-json-get 'description book)))
    (insert  (format "<h2>%s. %s</h2>\n\n" position title))
    (insert "<table><tr>\n")
    (when rss (insert (format "<td><a href='%s'>Listen (RSS)</a></td>\n" rss)))
    (when zip (insert (format "<td><a href='%s'>Download</a></td>\n" zip)))
    (when text (insert (format "<td><a href='%s'>Full Text</a></td>\n" text)))
    (when time (insert (format "<td>Time: %s</td>\n" time)))
    (insert "</tr></table>\n\n")
    (emacspeak-librivox-display-authors (g-json-get 'authors book))
    (when desc (insert "<p>" desc "</p>\n\n"))))

(defun emacspeak-librivox-search (pattern &optional page-title)
  "Search for books.
Argument `pattern' is of the form:
`author=pattern' Search by author.
`title=pattern' Search by title.
^all Browse books.
Optional arg `page-title' specifies page title."
  (or page-title (setq page-title pattern))
  (let* ((url
          (emacspeak-librivox-audiobooks-uri
           pattern))
         (result (g-json-get-result
                  (format
                   "%s  %s '%s'"
                   g-curl-program g-curl-common-options url)))
         (books (g-json-get 'books result)))
    (unless books (message "No results."))
    (emacspeak-auditory-icon 'task-done)
    (when books
      (with-temp-buffer
        (insert "<title>" page-title "</title>\n")
        (insert "<h1>" page-title "</h1>\n")
        (loop
         for b across books
         and i from 1
         do
         (emacspeak-librivox-display-book b i))
        (browse-url-of-buffer)))))
;;;###autoload
(defun emacspeak-librivox-search-by-genre (genre)
  "Search by genre.
Both exact and partial matches for `genre'."
  (interactive "sGenre: ")
  (emacspeak-librivox-search
   (format "genre=%s"
           (emacspeak-url-encode genre))
   (format "Search For Genre: %s" genre)))

;;;###autoload
(defun emacspeak-librivox-search-by-author (author)
  "Search by author.
Both exact and partial matches for `author'."
  (interactive "sAuthor: ")
  (emacspeak-librivox-search
   (format "author=%s"
           (emacspeak-url-encode author))
   (format "Search For Author: %s" author)))

;;;###autoload
(defun emacspeak-librivox-search-by-title (title)
  "Search by title.
Both exact and partial matches for `title'."
  (interactive "sTitle: ")
  (emacspeak-librivox-search
   (format "title=%s"
           (emacspeak-url-encode title))
   (format "Search For Title: %s" title)))

;;}}}
;;{{{ Top-Level Dispatch:

;;;###autoload
(defun emacspeak-librivox (search-type)
  "Launch a Librivox Search."
  (interactive
   (list
    (read-char "a: Author, t: Title,  p:Play, g:Genre")))
  (ecase search-type
    (?a (call-interactively 'emacspeak-librivox-search-by-author))
    (?p (call-interactively 'emacspeak-librivox-play))
    (?t (call-interactively 'emacspeak-librivox-search-by-title))
    (?g (call-interactively 'emacspeak-librivox-search-by-genre))))

  
  

;;}}}
;;{{{ Play Librivox Streams:
;;;###autoload
(defun emacspeak-librivox-play (rss-url)
  "Play book stream"
  (interactive
   (list
    (emacspeak-webutils-read-this-url)))
  (declare (special g-curl-program g-curl-common-options))
  (let ((file  (make-temp-file "librivox" nil ".rss")))
    (shell-command
     (format "%s %s %s > %s"
             g-curl-program g-curl-common-options rss-url file))
    (emacspeak-m-player-play-rss (format "file://%s" file))))
                         
                         

;;}}}
(provide 'emacspeak-librivox)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
