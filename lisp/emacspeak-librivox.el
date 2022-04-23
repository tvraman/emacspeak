;;; emacspeak-librivox.el --- LIBRIVOX API client  -*- lexical-binding: t; -*-
;; $Id: emacspeak-librivox.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;; $Author: tv.raman.tv $
;; Description:  Speech-enable LIBRIVOX --- Free public-domain Audio Books
;; Keywords: Emacspeak,  Audio Desktop librivox
;;{{{  LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;; 
;;  $Revision: 4532 $ |
;; Location undetermined
;; 

;;}}}
;;{{{  Copyright:

;; Copyright (C) 1995 -- 2021, T. V. Raman
;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
;; All Rights Reserved.
;; 
;; This file is not part of GNU Emacs, but the same permissions apply.
;; 
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or fitness FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;; LIBRIVOX == @url{http://www.librivox.org} --- Free Audio Books.
;; API Info: @url{https://librivox.org/api/info}
;; It provides a simple Web  API
;; This module implements an Emacspeak Librivox client.
;; 
;; @subsection Usage
;; 
;; main entry point is command @code{emacspeak-librivox} bound to @kbd{C-; l}.
;; This prompts with the following choices:
;; @itemize
;; @item @kbd{a} Author --- Search  by Author.
;; @item @kbd{t} Title --- Search  by Title.
;; @item @kbd{g} Genre --- Search  by Genre --- with minibuffer completion.
;; @item @kbd{p} Play --- Play a  book.
;; @item @kbd{d} Directory --- Browse local cache.
;; @end itemize
;; 
;; Search results are displayed in a Web page that provides controls
;; for accessing the book.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'dom)
(require 'g-utils)
(declare-function emacspeak-xslt-get "emacspeak-xslt" (style))

;;}}}
;;{{{ Variables:

(defvar emacspeak-librivox-buffer-name
"Librivox Interaction*"
  "Name of Librivox interaction buffer.")

;;}}}
;;{{{ API:

(defvar emacspeak-librivox-api-base
  "https://librivox.org/api/feed/"
  "Base REST end-point for Librivox API  access.")

;; audiobooks:
;; Params from API Documentation:

;; * id - fetches a single record
;; * since - takes a UNIX timestamp
;; returns all projects cataloged since that time
;; * author - all records by that author last name
;; * title - all matching titles
;; * genre - all projects of the matching genre
;; * extended - =1 will return the full set of data about the project
;; * limit (default is 50)
;;   * offset
(defvar emacspeak-librivox-results-limit 100
  "Number of results to retrieve at a time.")

(defun emacspeak-librivox-audiobooks-uri (pattern  offset)
  "Search URI for audiobooks."
  (cl-declare (special emacspeak-librivox-api-base))
  (concat
   emacspeak-librivox-api-base
   (format "audiobooks?offset=%s&limit=%s&format=json&"
           offset emacspeak-librivox-results-limit)
   pattern))

;; Audio Tracks API:
;;; Params:
;; * id - of track itself
;; * project_id - all tracks for project

(defun emacspeak-librivox-audiotracks-base (pattern)
  "Base URI for audiotracks."
  (cl-declare (special emacspeak-librivox-api-base))
  (concat emacspeak-librivox-api-base "audiotracks?format=json&" pattern))

;; Simple Authors API:
;;; Params:
;; * id - of author
;; * last_name - exact match

(defun emacspeak-librivox-authors-base ()
  "Base URI for authors."
  (cl-declare (special emacspeak-librivox-api-base))
  (concat emacspeak-librivox-api-base "authors"))

;;}}}
;;{{{ Search Commands:

(defun emacspeak-librivox-display-author (author)
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
    (cl-loop
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
    (when rss (insert (format "<td><a   href='%s'>Listen </a></td>\n" rss)))
    (when zip (insert (format "<td><a href='%s'>Download</a></td>\n" zip)))
    (when text (insert (format "<td><a href='%s'>Full Text</a></td>\n" text)))
    (when time (insert (format "<td>Time: %s</td>\n" time)))
    (insert "</tr></table>\n\n")
    (emacspeak-librivox-display-authors (g-json-get 'authors book))
    (when desc (insert "<p>" desc "</p>\n\n"))))

(defun emacspeak-librivox--render (title books offset)
  "render results page. "
  (with-temp-buffer
    (insert "<title>" title "</title>\n")
    (insert "<h1>" title "</h1>\n")
    (insert
     "<p> Press <code>e e </code> on a <em>listen</em> link to play the
book.</p>")
    ;;; convert to list to avoid strange binding  error  when using a vector
    (setq books (append books nil))
    (cl-loop
     for b in  books
     and i from (1+ offset)
     do
     (emacspeak-librivox-display-book b i))
    (when (= emacspeak-librivox-results-limit (length books))
      (insert
       (format
        "Re-execute this command with an interactive prefix argument and
specify offset %s to get more results."
        (+ offset emacspeak-librivox-results-limit))))
    (browse-url-of-buffer)))

(defun emacspeak-librivox-search (pattern &optional  offset)
  "Search for books.
Argument `pattern' is of the form:
`author=pattern' Search by author.
`title=pattern' Search by title.
^all Browse books.
Optional arg `offset' (default 0) is used for getting more results."
  (cl-declare (special  emacspeak-librivox-results-limit))
  (or offset (setq offset 0))
  (let* ((title
          (format
           "Search: %s Offset: %s"
           (url-unhex-string pattern) offset))
         (url (emacspeak-librivox-audiobooks-uri pattern offset))
         (result (g-json-get-result
                  (format
                   "%s  %s '%s'"
                   g-curl-program g-curl-common-options url)))
         (books (g-json-get 'books result)))
    (unless books (message "No results."))
    (emacspeak-auditory-icon 'task-done)
    (when books
      (emacspeak-eww-autospeak)
      (add-hook
       'emacspeak-eww-post-process-hook
       #'(lambda ()
           (cl-declare (special emacspeak-we-url-executor))
           (setq emacspeak-we-url-executor 'emacspeak-librivox-play)))
      (emacspeak-librivox--render title books offset))))

(defvar emacspeak-librivox-genre-list
  '(
"Non-fiction" "Action & Adventure" "Action & Adventure Fiction"
    "Ancient" "Animals" "Animals & Nature"
    "Anthologies" "Antiquity" "Art, Design & Architecture"
    "Arts" "Astronomy, Physics & Mechanics" "Ballads"
    "Bibles" "Biography & Autobiography" "Business & Economics"
    "Chemistry" "Children's Fiction" "Children's Non-fiction"
    "Christian Fiction" "Christianity - Biographies"
    "Christianity - Commentary"
    "Christianity - Other" "Classics (Antiquity)" "Comedy"
    "Contemporary" "Cooking" "Crafts & Hobbies"
    "Crime & Mystery Fiction" "Culture & Heritage" "Detective Fiction"
    "Douay-Rheims Version" "Drama" "Dramatic Readings"
    "Early Modern" "Earth Sciences" "Education"
    "Elegies & Odes" "Epics" "Epistolary Fiction"
    "Erotica" "Essays" "Essays & Short Works"
    "Exploration" "Family" "Family & Relationships"
    "Family Life" "Fantastic Fiction" "Fantasy Fiction"
    "Fictional Biographies & Memoirs" "Free Verse" "Games"
    "Gardening" "General" "General Fiction"
    "Gothic Fiction" "Health & Fitness" "Historical"
    "Historical Fiction" "History " "Horror & Supernatural Fiction"
    "House & Home" "Humor" "Humorous Fiction"
    "King James Version" "Language learning" "Law"
    "Letters" "Life Sciences" "Literary Collections"
    "Literary Criticism" "Literary Fiction" "Lyric"
    "Mathematics" "Medical" "Medieval"
    "Memoirs" "Middle Ages/Middle History" "Modern"
    "Modern (19th C)" "Modern (20th C)"
    "Multi-version (Weekly and Fortnightly poetry)"
    "Music" "Myths, Legends & Fairy Tales" "Narratives"
    "Nature" "Nature & Animal Fiction" "Nautical & Marine Fiction"
    "Other religions" "Performing Arts" "Philosophy"
    "Plays" "Poetry" "Political Science"
    "Psychology" "Published 1800 -1900" "Published 1900 onward"
    "Published before 1800" "Reference" "Religion"
    "Religious Fiction" "Romance" "Sagas"
    "Satire" "School" "Science"
    "Science Fiction" "Self-Help" "Short Stories"
    "Short non-fiction" "Short works" "Single Author Collections"
    "Single author" "Social Science" "Sonnets"
    "Sports & Recreation" "Sports Fiction"
    "Suspense, Espionage, Political & Thrillers"
    "Technology & Engineering" "Tragedy" "Travel & Geography"
    "Travel Fiction" "True Crime" "War & Military"
    "War & Military Fiction" "Westerns" "Weymouth New Testament"
    "Writing & Linguistics" "Young's Literal Translation"
    )
  "List of genres.")


(defun emacspeak-librivox-search-by-genre (genre &optional offset)
  "Search by genre.
Optional prefix arg `offset' prompts for offset."
  (interactive
   (list
    (let ((completion-ignore-case t))
      (completing-read "Genre: " emacspeak-librivox-genre-list))
    current-prefix-arg))
  (cl-declare (special emacspeak-librivox-genre-list))
  (when offset (setq offset (read-number "Offset: ")))
  (emacspeak-librivox-search
   (format "genre=%s"
           (url-encode-url genre))
   offset))


(defun emacspeak-librivox-search-by-author (author &optional offset)
  "Search by author. Both exact and partial matches for
`author'. Optional interactive prefix arg `offset' prompts for offset
--- use this for retrieving next set of results."
  (interactive "sAuthor: \nP")
  (when offset
    (setq offset (read-number "Offset: ")))
  (emacspeak-librivox-search
   (format "author=%s"
           (url-hexify-string author))
   offset))


(defun emacspeak-librivox-search-by-title (title &optional offset)
  "Search by title. Both exact and partial matches for `title'. Optional
prefix arg `offset' prompts for offset --- use this for retrieving
more results."
  (interactive "sTitle: \nP")
  (emacspeak-librivox-search
   (format "title=%s"
           (url-hexify-string title))
   offset))

;;}}}
;;{{{ Top-Level Dispatch:

;;;###autoload
(defun emacspeak-librivox (search-type)
  "Launch a Librivox Search."
  (interactive
   (list
    (read-char "a: Author, t: Title,  p:Play, g:Genre, d: Browse Local")))
  (cl-ecase search-type
    (?d (dired (expand-file-name "librivox" emacspeak-user-directory)))
    (?a (call-interactively 'emacspeak-librivox-search-by-author))
    (?p (call-interactively 'emacspeak-librivox-play))
    (?t (call-interactively 'emacspeak-librivox-search-by-title))
    (?g (call-interactively 'emacspeak-librivox-search-by-genre))))

;;}}}
;;{{{ Cache Playlists:
(defcustom emacspeak-librivox-local-cache
  (expand-file-name "librivox" emacspeak-user-directory)
  "Location where we cache LIBRIVOX playlists."
  :type 'directory
  :group 'emacspeak-librivox)

(defun emacspeak-librivox-ensure-cache ()
  "Create LIBRIVOX cache directory if needed."
  (cl-declare (special emacspeak-librivox-local-cache))
  (unless (file-exists-p emacspeak-librivox-local-cache)
    (make-directory  emacspeak-librivox-local-cache 'parents)))

(defun emacspeak-librivox-get-m3u-name (rss)
  "Parse RSS from temporary location to create a real file name."
  (emacspeak-librivox-ensure-cache)
  (cl-assert  (file-exists-p rss) nil "RSS file not found.")
  (with-current-buffer (find-file rss)
    (let* ((title
            (dom-by-tag
             (libxml-parse-xml-region (point-min) (point-max))
             'title)))
      (when title
        (setq title (dom-text (cl-first title)))
        (setq title (replace-regexp-in-string " +" "-" title)))
      (kill-buffer)
      (expand-file-name
       (format "%s.m3u" (or title "Untitled"))
       emacspeak-librivox-local-cache))))

;;}}}
;;{{{ Play Librivox Streams:

(defun emacspeak-librivox-play (rss-url)
  "Play book stream"
  (interactive
   (list
    (emacspeak-eww-read-url)))
  (cl-declare (special g-curl-program g-curl-common-options
                       emacspeak-xslt-program))
  (let ((file  (make-temp-file "librivox" nil ".rss"))
        (m3u-file nil))
    (shell-command
     (format "%s %s %s > %s"
             g-curl-program g-curl-common-options rss-url file))
    (setq m3u-file (emacspeak-librivox-get-m3u-name file))
    (shell-command
     (format "%s %s %s > \"%s\""
             emacspeak-xslt-program (emacspeak-xslt-get "rss2m3u.xsl")
             file m3u-file))
    (emacspeak-m-player m3u-file 'playlist)))

;;}}}
(provide 'emacspeak-librivox)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}
