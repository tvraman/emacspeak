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

(declare-function gweb-google-autocomplete "gweb" (&optional prompt))
(declare-function calendar-astro-date-string "cal-julian" (&optional date))
;;;###autoload
(define-prefix-command 'emacspeak-websearch)
(cl-declaim (special emacspeak-websearch))

(cl-loop
 for b in
 '(
   ("C-a"       emacspeak-websearch-amazon-search)
   ("C-b"       emacspeak-websearch-biblio-search)
   ("SPC"       emacspeak-websearch-google-feeling-lucky)
   ("?"         emacspeak-websearch-help)
   ("G"         emacspeak-websearch-gutenberg)
   ("a"         emacspeak-websearch-accessible-google)
   ("f"         emacspeak-websearch-foldoc-search)
   ("g"         emacspeak-websearch-google)
   ("i"         emacspeak-websearch-google-with-toolbelt)
   ("j"         emacspeak-websearch-ask-jeeves)
   ("n"         emacspeak-websearch-google-news)
   ("u"         emacspeak-websearch-web-filter-google)
   ("w"         emacspeak-websearch-wikipedia-search)
   ("y"         emacspeak-websearch-youtube-search))
 do
 (emacspeak-keymap-update emacspeak-websearch b ))

(defun emacspeak-websearch-help ()
  "Displays key mapping used by Emacspeak Websearch."
  (interactive)
  (funcall-interactively 'describe-bindings (kbd "C-e /") ))

;;;  helpers to read the query

(defvar ems--ws-history nil
  "Holds history of search queries.")

(defsubst emacspeak-websearch-read-query (prompt)
  "Read search query"
  (let ((q (read-from-minibuffer prompt nil  nil nil 'ems--ws-history )))
    (cl-pushnew q  ems--ws-history :test #'string=)
    q))

;;; post-processor
(defun emacspeak-websearch-post (locator speaker &rest args)
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

(defun emacspeak-websearch-biblio-search (query)
  "Search Computer Science Bibliographies."
  (interactive (list (emacspeak-websearch-read-query "CS Biblio: ")))
  (let ((url (concat
              "http://liinwww.ira.uka.de/searchbib/index"
              "?partial=on&case=on&results=citation&maxnum=200&query=")))
    (browse-url
     (concat url (url-hexify-string query))))
  (emacspeak-websearch-post
   query
   'emacspeak-speak-line))

;;;  FolDoc

(defun emacspeak-websearch-foldoc-search (query)
  "Perform a FolDoc search. "
  (interactive (list (emacspeak-websearch-read-query "Computing Dict: ")))
  (browse-url
   (concat
    "http://foldoc.org/"
    (url-hexify-string query)))
  (emacspeak-websearch-post
   query
   'emacspeak-speak-line))

;;;  Gutenberg

(defun emacspeak-websearch-gutenberg (type query)
  "Perform an Gutenberg search"
  (interactive
   (list
    (read-char "Author a, Title t")
    (emacspeak-websearch-read-query "Gutenberg query: ")))
  (browse-url
   (concat
    "http://digital.library.upenn.edu/webbin/book/search?"
    (cl-ecase type
      (?a "author=")
      (?t "title="))
    (url-hexify-string query)))
  (emacspeak-websearch-post query 'emacspeak-speak-line))

;;;  google

(defvar emacspeak-websearch-google-base
  "www.google.com/search?q="
  "Base of URI for Google search")

(defsubst emacspeak-websearch-google-uri ()
  "Return URI end-point for Google search."
  (cl-declare (special emacspeak-google-use-https
                       emacspeak-websearch-google-base))
  (concat
   (if emacspeak-google-use-https
       "https://"
     "http://")
   emacspeak-websearch-google-base))

(defvar emacspeak-websearch-google-options nil
  "Additional options to pass to Google e.g. &xx=yy...")

(defadvice gweb-google-autocomplete (after emacspeak pre act comp)
  "Cache the query."
  (cl-declare (special emacspeak-google-query))
  (setq emacspeak-google-query ad-return-value))

(defconst ems--google-filter
  '("main")
  "Ids of nodes we keep in Google results page.")

(declare-function emacspeak-eww-next-h "emacspeak-eww" (&optional speak))
(declare-function emacspeak-eww-next-h1 "emacspeak-eww" (&optional speak))

(defun emacspeak-websearch-google (query &optional flag)
  "Perform a Google search.  First optional interactive prefix arg
`flag' prompts for additional search options. Second interactive
prefix arg is equivalent to hitting the I'm Feeling Lucky button on Google. "
  (interactive (list (gweb-google-autocomplete) current-prefix-arg))
  (cl-declare (special
               emacspeak-google-query
               emacspeak-google-toolbelt
               ems--google-filter
               emacspeak-websearch-google-options
               25))
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
                   25
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
       ems--google-filter
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
               ems--google-filter
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
       ems--google-filter
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
               ems--google-filter
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
       ems--google-filter
       (format emacspeak-websearch-wf-google-url query))))))

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

(defun emacspeak-websearch-google-news ()
  "Invoke Google News url template."
  (interactive)
  (let ((name "Google News Search"))
    (emacspeak-url-template-open
     (emacspeak-url-template-get name))))

;;;   Ask Jeeves

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
  (emacspeak-websearch-post query 'emacspeak-speak-line))

;;;  wikipedia

(defun emacspeak-websearch-wikipedia-search (query)
  "Search Wikipedia using Google.
Use URL Template `wikipedia at point' to advantage in the results buffer."
  (interactive
   (list (emacspeak-websearch-read-query "Search Wikipedia: ")))
  (emacspeak-websearch-google
   (url-hexify-string (format "site:wikipedia.org %s"query))))

;;;  YouTube Search:

(defun emacspeak-websearch-youtube-search (query)
  "YouTube search."
  (interactive (list (gweb-youtube-autocomplete)))
  (emacspeak-websearch-google
   (url-hexify-string (format "site:youtube.com  %s"query))))

;;;  Shopping at Amazon

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
