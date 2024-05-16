;;; emacspeak-websearch.el --- search utilities  -*- lexical-binding: t; -*-
;;
;; $Author: tv.raman.tv $
;; Description:  Emacspeak extension to make Web searching convenient
;; Keywords: Emacspeak, WWW interaction
;;;   LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;; 
;;  $Revision: 4625 $ |
;; Location https://github.com/tvraman/emacspeak
;; 

;;;   Copyright:

;; Copyright (C) 1995 -- 2024, T. V. Raman
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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:

;; This module provides utility functions for searching the WWW

;;; Code:


;;  required modules
(eval-when-compile (require 'cl-lib))
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'emacspeak-google)
(require 'gweb)
(declare-function word-at-point "thingatpt" (&optional no-properties))
(declare-function calendar-cursor-to-date "calendar" (&optional error event))
(declare-function emacspeak-eww-autospeak "emacspeak-eww" nil)

;;;  Forward Declarations:

(declare-function gweb-google-autocomplete "gweb" (&optional prompt))
(declare-function calendar-astro-date-string "cal-julian" (&optional date))

;;;  searcher table

(defvar emacspeak-websearch-table (make-hash-table)
  "Table to map  search engine names to appropriate searcher functions.")

(defun emacspeak-websearch-set-searcher  (engine searcher)
  (cl-declare (special emacspeak-websearch-table))
  (setf (gethash engine emacspeak-websearch-table) searcher))

(defsubst emacspeak-websearch-get-searcher (engine)
  (cl-declare (special emacspeak-websearch-table))
  (gethash engine emacspeak-websearch-table))

;;;  Key table

(defvar emacspeak-websearch-keytable (make-hash-table)
  "Table holding mapping from keys to appropriate search engine names.")

(defun emacspeak-websearch-set-key  (key engine)
  (cl-declare (special emacspeak-websearch-keytable))
  (setf (gethash key emacspeak-websearch-keytable) engine))

(defsubst emacspeak-websearch-get-engine (key)
  (cl-declare (special emacspeak-websearch-keytable))
  (gethash key emacspeak-websearch-keytable))

;;;  top-level dispatch

(defun emacspeak-websearch-help ()
  "Displays key mapping used by Emacspeak Websearch."
  (interactive)
  (let ((inhibit-read-only  t)
        (map
         (cl-loop
          for key being the hash-keys of emacspeak-websearch-keytable
          collect (cons key (gethash key emacspeak-websearch-keytable)))))
    (setq map
          (sort map #'(lambda (a b) (< (car a) (car b)))))
    (with-current-buffer (help-buffer)
      (erase-buffer)
      (help-mode)
      (cl-loop
       for m in map do
       (insert (format "%s:\t%s\n"
                       (key-description (list (car m)))
                       (emacspeak-websearch-get-searcher (cdr m)))))
      (goto-char (point-min)))
    (pop-to-buffer (help-buffer))
    (emacspeak-speak-mode-line)
    (emacspeak-icon 'help)))

(emacspeak-websearch-set-searcher  'help
                                   'emacspeak-websearch-help)

(emacspeak-websearch-set-key ?? 'help)
;;;###autoload
(defun emacspeak-websearch-dispatch  ()
  " Press `?' to list available search engines.
   This interface attempts to speak the most relevant information
   on the result page."
  (interactive)
  (let ((engine nil)
        (searcher nil))
    (while (null engine)
      (setq engine
            (emacspeak-websearch-get-engine
             (read-char
              (concat "Websearch? "
                      (documentation this-command))))))
    (setq searcher (emacspeak-websearch-get-searcher engine))
    (if searcher
        (call-interactively searcher)
      (error "I do not know how to search using %s" engine))))

;;;  helpers to read the query

(defvar emacspeak-websearch-history nil
  "Holds history of search queries.")

(defun emacspeak-websearch-read-query (prompt &optional
                                              default
                                              initial)
  (let ((answer
         (read-from-minibuffer
          prompt
          initial  nil nil
          (car emacspeak-websearch-history)
          (or default (word-at-point)))))
    (cl-pushnew answer  emacspeak-websearch-history :test #'string=)
    answer))

;;; post-processor
(defun emacspeak-websearch-post-process (locator speaker &rest args)
  "Set up post processing steps on a result page.
LOCATOR is a string to search for in the results page.
SPEAKER is a function to call to speak relevant information.
ARGS specifies additional arguments to SPEAKER if any."
  (cl-declare (special emacspeak-eww-post-hook))
  (add-hook
   'emacspeak-eww-post-hook
   (eval
    `#'(lambda nil
         (let ((inhibit-read-only t))
           (condition-case nil
               (cond
                ((search-forward ,locator nil t)
                 (recenter 0)
                 (apply(quote ,speaker) ,args))
                (t (message "Your search appears to have failed.")))
             (error nil)))))
   'at-end))

;;;  Computer Science Bibliography

(emacspeak-websearch-set-searcher 'biblio
                                  'emacspeak-websearch-biblio-search)

(emacspeak-websearch-set-key 2 'biblio)

(defvar emacspeak-websearch-biblio-uri
  (concat
   "http://liinwww.ira.uka.de/searchbib/index"
   "?partial=on&case=on&results=citation&maxnum=200&query=")
  "URI to search the Computer Science Bibliographies.")

(defun emacspeak-websearch-biblio-search (query)
  "Search Computer Science Bibliographies."
  (interactive
   (list
    (emacspeak-websearch-read-query "Search CS Bibliographies  for: ")))
  (cl-declare (special emacspeak-websearch-biblio-uri))
  (browse-url
   (concat emacspeak-websearch-biblio-uri
           (url-hexify-string query)))
  (emacspeak-websearch-post-process
   query
   'emacspeak-speak-line))

;;;  FolDoc

(emacspeak-websearch-set-searcher 'foldoc
                                  'emacspeak-websearch-foldoc-search)
(emacspeak-websearch-set-key ?f 'foldoc)

(defvar emacspeak-websearch-foldoc-uri
  "http://foldoc.org/"
  "URI for launching a FolDoc  search.")

(defun emacspeak-websearch-foldoc-search (query)
  "Perform a FolDoc search. "
  (interactive
   (list
    (emacspeak-websearch-read-query "Computing Dictionary Query: ")))
  (cl-declare (special emacspeak-websearch-foldoc-uri))
  (browse-url
   (concat emacspeak-websearch-foldoc-uri
           (url-hexify-string query)))
  (emacspeak-websearch-post-process
   query
   'emacspeak-speak-line))

;;;  Gutenberg

(emacspeak-websearch-set-searcher 'gutenberg
                                  'emacspeak-websearch-gutenberg)

(emacspeak-websearch-set-key ?G 'gutenberg)

(defvar emacspeak-websearch-gutenberg-uri
  "http://digital.library.upenn.edu/webbin/book/search?"
  "URI for Gutenberg search")

(defun emacspeak-websearch-gutenberg (type query)
  "Perform an Gutenberg search"
  (interactive
   (list
    (read-char "Author a, Title t")
    (emacspeak-websearch-read-query "Gutenberg query: ")))
  (cl-declare (special emacspeak-websearch-gutenberg-uri))
  (browse-url
   (concat emacspeak-websearch-gutenberg-uri
           (cl-ecase type
             (?a "author=")
             (?t "title="))
           (url-hexify-string query)))
  (emacspeak-websearch-post-process
   query
   'emacspeak-speak-line))

;;;  google
(emacspeak-websearch-set-searcher 'google-lucky
                                  'emacspeak-websearch-google-feeling-lucky)

(emacspeak-websearch-set-key ?\  'google-lucky)

(emacspeak-websearch-set-searcher 'agoogle
                                  'emacspeak-websearch-accessible-google)

(emacspeak-websearch-set-key ?a 'agoogle)

(emacspeak-websearch-set-key ?i 'google-with-toolbelt)
(emacspeak-websearch-set-key ?g 'google)
(emacspeak-websearch-set-key ?u 'web-filter)

(emacspeak-websearch-set-searcher 'google 'emacspeak-websearch-google)

(emacspeak-websearch-set-searcher 'web-filter 'emacspeak-websearch-web-filter-google)
(emacspeak-websearch-set-searcher 'google-with-toolbelt
                                  'emacspeak-websearch-google-with-toolbelt)

(defvar emacspeak-websearch-google-uri-template
  "www.google.com/search?q="
  "URI for Google search")

(defsubst emacspeak-websearch-google-uri ()
  "Return URI end-point for Google search."
  (cl-declare (special emacspeak-google-use-https
                       emacspeak-websearch-google-uri-template))
  (concat
   (if emacspeak-google-use-https
       "https://"
     "http://")
   emacspeak-websearch-google-uri-template))

(defvar emacspeak-websearch-google-options nil
  "Additional options to pass to Google e.g. &xx=yy...")

(defadvice gweb-google-autocomplete (after emacspeak pre act comp)
  "Cache the query."
  (cl-declare (special emacspeak-google-query))
  (setq emacspeak-google-query ad-return-value))
(defvar ems--websearch-google-filter
  '("main")
  "Ids of nodes we keep in Google results page.")

(defvar emacspeak-websearch-google-number-of-results 25
  "Number of Google search results.")
(declare-function emacspeak-eww-next-h "emacspeak-eww" (&optional speak))
(declare-function emacspeak-eww-next-h1 "emacspeak-eww" (&optional speak))

;;;###autoload
(defun emacspeak-websearch-google (query &optional flag)
  "Perform a Google search.  First optional interactive prefix arg
`flag' prompts for additional search options. Second interactive
prefix arg is equivalent to hitting the I'm Feeling Lucky button on Google. "
  (interactive (list (gweb-google-autocomplete) current-prefix-arg))
  (cl-declare (special
               emacspeak-google-query
               emacspeak-google-toolbelt
               ems--websearch-google-filter
               emacspeak-websearch-google-options
               emacspeak-websearch-google-number-of-results))
  (setq emacspeak-google-toolbelt nil)
  (let ((toolbelt (emacspeak-google-toolbelt))
        (search-url nil)
        (add-toolbelt (and flag  (consp flag) (= 4 (car flag))))
        (lucky (and flag  (consp flag) (= 16 (car flag)))))
    (emacspeak-google-cache-query query)
    (emacspeak-google-cache-toolbelt toolbelt)
    (setq search-url
          (concat
           (emacspeak-websearch-google-uri)
           query
           (format "&num=%s%s"          ; accumulate options
                   emacspeak-websearch-google-number-of-results
                   (or emacspeak-websearch-google-options ""))
           (when lucky
             (concat
              "&btnI="
              (url-hexify-string "I'm Feeling Lucky")))))
    (cond
     (add-toolbelt (emacspeak-google-toolbelt-change))
     (lucky
      (emacspeak-eww-autospeak)
      (browse-url search-url))
     (t                                 ; always just show results
      (add-hook
       'emacspeak-eww-post-hook
       #'(lambda ()
           (goto-char (point-min))
           (emacspeak-eww-next-h)
           (dtk-stop)
           (emacspeak-eww-next-h)
           (emacspeak-speak-windowful))
       'at-end)      
      (emacspeak-we-extract-by-id-list
       ems--websearch-google-filter
       search-url)))))

(defvar emacspeak-websearch-accessible-google-url
  "https://www.google.com/search?num=25&lite=90586&q=%s"
  "Using Google Lite.")

(defun emacspeak-websearch-accessible-google(query &optional options)
  "Use Google Lite.
Optional prefix arg prompts for toolbelt options."
  (interactive
   (list
    (gweb-google-autocomplete "AGoogle: ")
    current-prefix-arg))
  (cl-declare (special
               emacspeak-eww-masquerade
               ems--websearch-google-filter
               emacspeak-websearch-accessible-google-url
               emacspeak-google-toolbelt))
  (setq emacspeak-google-toolbelt nil)
  (let ((emacspeak-eww-masquerade t)
        (toolbelt (emacspeak-google-toolbelt)))
    (emacspeak-google-cache-query query)
    (emacspeak-google-cache-toolbelt toolbelt)
    (cond
     (options (emacspeak-google-toolbelt-change))
     (t
      (add-hook
       'emacspeak-eww-post-hook
       #'(lambda ()
           (goto-char (point-min))
           (emacspeak-eww-next-h) (search-forward "Search Tools" nil
                                                  t)
           (dtk-stop)
           (emacspeak-eww-next-h)
           (emacspeak-speak-windowful)))
      (emacspeak-we-extract-by-id-list
       ems--websearch-google-filter
       (format emacspeak-websearch-accessible-google-url query))))))

(defvar emacspeak-websearch-wf-google-url
  "https://www.google.com/search?num=25&lite=90586&udm=1&q=%s"
  "Using Google Lite with Web Filter turned on.")

(defun emacspeak-websearch-web-filter-google (query &optional options)
  "Use Google Lite with Web filter.
Optional prefix arg prompts for toolbelt options."
  (interactive
   (list (gweb-google-autocomplete "WFGoogle: ") current-prefix-arg))
  (cl-declare (special
               emacspeak-eww-masquerade
               ems--websearch-google-filter
               emacspeak-websearch-wf-google-url
               emacspeak-google-toolbelt))
  (setq emacspeak-google-toolbelt nil)
  (let ((emacspeak-eww-masquerade t)
        (toolbelt (emacspeak-google-toolbelt)))
    (emacspeak-google-cache-query query)
    (emacspeak-google-cache-toolbelt toolbelt)
    (cond
     (options (emacspeak-google-toolbelt-change))
     (t
      (add-hook
       'emacspeak-eww-post-hook
       #'(lambda ()
           (goto-char (point-min))
           (emacspeak-eww-next-h) (search-forward "Search Tools" nil
                                                  t)
           (dtk-stop)
           (emacspeak-eww-next-h)
           (emacspeak-speak-windowful)))
      (emacspeak-we-extract-by-id-list
       ems--websearch-google-filter
       (format emacspeak-websearch-wf-google-url query))))))

;;;###autoload
(defun emacspeak-websearch-google-with-toolbelt (query)
  "Launch Google search with toolbelt."
  (interactive (list (gweb-google-autocomplete "IGoogle: ")))
  (emacspeak-websearch-accessible-google query 'use-toolbelt))

(defun emacspeak-websearch-google-feeling-lucky (query)
  "Do a I'm Feeling Lucky Google search."
  (interactive
   (list
    (gweb-google-autocomplete "Google Lucky Search: ")))
  (emacspeak-websearch-google query '(16)))

(defun emacspeak-websearch-google-search-in-date-range ()
  "Use this from inside the calendar to do Google date-range searches."
  (interactive)
  (cl-declare (special calendar-mark-ring))
  (let ((query (emacspeak-websearch-read-query "Google for: "))
        (from (read (calendar-astro-date-string (calendar-cursor-to-date t))))
        (to
         (read
          (calendar-astro-date-string
           (or (car calendar-mark-ring)
               (error "No mark set"))))))
    (emacspeak-websearch-google
     (concat
      (url-hexify-string query)
      (format "+daterange:%s-%s"
              (min from to)
              (max from to))))))

(when (featurep 'calendar)
  (cl-declaim (special calendar-mode-map))
  (define-key calendar-mode-map "gg"
              'emacspeak-websearch-google-search-in-date-range))

;;;  Google News

(emacspeak-websearch-set-searcher 'google-news
                                  'emacspeak-websearch-google-news)

(emacspeak-websearch-set-key ?n 'google-news)

(defun emacspeak-websearch-google-news ()
  "Invoke Google News url template."
  (interactive)
  (let ((name "Google News Search"))
    (emacspeak-url-template-open
     (emacspeak-url-template-get name))))

;;;   Ask Jeeves

(emacspeak-websearch-set-searcher 'jeeves
                                  'emacspeak-websearch-ask-jeeves)
(emacspeak-websearch-set-key ?j 'jeeves)

(defvar emacspeak-websearch-jeeves-uri
  "http://www.ask.com/web?qsrc=0&o=0&ASKDSBHO=0&q="
  "URI for Ask Jeeves  search")

(defun emacspeak-websearch-ask-jeeves (query)
  "Ask Jeeves for the answer."
  (interactive
   (list (emacspeak-websearch-read-query "Ask Jeeves for: ")))
  (cl-declare (special emacspeak-websearch-jeeves-uri))
  (browse-url
   (concat emacspeak-websearch-jeeves-uri
           (url-hexify-string query)))
  (emacspeak-websearch-post-process query 'emacspeak-speak-line))

;;;  wikipedia

(emacspeak-websearch-set-searcher 'wikipedia
                                  'emacspeak-websearch-wikipedia-search)

(emacspeak-websearch-set-key ?w 'wikipedia)

(defun emacspeak-websearch-wikipedia-search (query)
  "Search Wikipedia using Google.
Use URL Template `wikipedia at point' to advantage in the results buffer."
  (interactive
   (list (emacspeak-websearch-read-query "Search Wikipedia: ")))
  (emacspeak-websearch-google
   (url-hexify-string (format "site:wikipedia.org %s"query))))

;;;  YouTube Search:

(emacspeak-websearch-set-searcher 'youtube-search
                                  'emacspeak-websearch-youtube-search)

(emacspeak-websearch-set-key ?y 'youtube-search)

(defun emacspeak-websearch-youtube-search (query)
  "YouTube search."
  (interactive (list (gweb-youtube-autocomplete)))
  (emacspeak-websearch-google
   (url-hexify-string (format "site:youtube.com  %s"query))))

;;;  Shopping at Amazon

(emacspeak-websearch-set-searcher 'amazon-search
                                  'emacspeak-websearch-amazon-search)

(emacspeak-websearch-set-key 1 'amazon-search)

(defvar emacspeak-websearch-amazon-search-form
  "http://www.amazon.com/access"
  "Form for Amazon store search.")

(defun emacspeak-websearch-amazon-search ()
  "Amazon search."
  (interactive)
  (cl-declare (special emacspeak-websearch-amazon-search-form))
  (browse-url emacspeak-websearch-amazon-search-form))

(provide 'emacspeak-websearch)
;;;  end of file

