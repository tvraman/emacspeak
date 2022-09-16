;;; emacspeak-google.el --- Google Search Tools  -*- lexical-binding: t; -*-
;; $Id: emacspeak-google.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;; $Author: tv.raman.tv $
;; Description:  Speech-enable GOOGLE An Emacs Interface to google
;; Keywords: Emacspeak,  Audio Desktop google
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
;; Copyright (C) 1995 -- 2022, T. V. Raman
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
;; MERCHANTABILITY or FITNGOOGLE FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;; There are a number of search tools that can be implemented on
;; the Google search page --- in a JS-powered browser, these
;; show up as the Google tool-belt.
;; This module implements a minor mode for use in Google result
;; pages that enables these tools via single keyboard commands.
;; Originally all options were available as tbs=p:v
;; Now, some specialized searches, e.g. blog search are tbm=
;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(eval-when-compile (require 'derived))
(require 'emacspeak-preamble)
(require 'gweb)

;;}}}
;;{{{ Data Structures

;; One tool on a tool-belt

(cl-defstruct emacspeak-google-tool
  name ; human readable
  param ; url param bit
  range ; range of possible values
  default
  value ; current setting
  type ;tbs/tbm
  )
(defvar emacspeak-google-use-https t
  "Specify whether we use secure connections for Google search.")

(defvar emacspeak-google-query nil
  "Current Google Query.
This variable is buffer-local.")
(make-variable-buffer-local 'emacspeak-google-query)

(defvar emacspeak-google-toolbelt nil
  "List of tools on the toolbelt.")

(make-variable-buffer-local 'emacspeak-google-toolbelt)

(defun emacspeak-google-toolbelt-to-tbm (belt)
  "Return value for use in tbm parameter in search queries."
  (let
      ((settings
        (delq nil
              (mapcar
               #'(lambda (tool)
                   (when (eq 'tbm (emacspeak-google-tool-type tool))
                     (cond
                      ((equal (emacspeak-google-tool-value tool)
                              (emacspeak-google-tool-default tool))
                       nil)
                      (t (format "%s"
                                 (emacspeak-google-tool-param tool))))))
               belt))))
    (when settings
      (concat "&tbm="
              (mapconcat #'identity settings ",")))))

(defun emacspeak-google-toolbelt-to-tbs (belt)
  "Return value for use in tbs parameter in search queries."
  (let
      ((settings
        (delq nil
              (mapcar
               #'(lambda (tool)
                   (when (eq 'tbs (emacspeak-google-tool-type tool))
                     (cond
                      ((equal (emacspeak-google-tool-value tool)
                              (emacspeak-google-tool-default tool))
                       nil)
                      (t (format "%s:%s"
                                 (emacspeak-google-tool-param tool)
                                 (emacspeak-google-tool-value tool))))))
               belt))))
    (when settings
      (concat "&tbs="
              (mapconcat #'identity settings ",")))))
(defvar emacspeak-google-toolbelt-names  nil
  "Cache of available toolbelt names.")
(make-variable-buffer-local 'emacspeak-google-toolbelt-names)
(defun emacspeak-google-toolbelt ()
  "Returns buffer-local toolbelt or a a newly initialized toolbelt."
  (cl-declare (special emacspeak-google-toolbelt))
  (or emacspeak-google-toolbelt
      (setq
       emacspeak-google-toolbelt
       (list
        ;; video vid: 1/0
        (make-emacspeak-google-tool
         :name "video"
         :param "vid"
         :range '(0 1)
         :default 0
         :type 'tbm
         :value 0)
        ;; Recent
        (make-emacspeak-google-tool
         :name "recent"
         :param "rcnt"
         :range '(0 1)
         :default 0
         :value 0
         :type 'tbs)
        ;; Duration restrict for video
        (make-emacspeak-google-tool
         :name "video-duration"
         :param "dur"
         :range '("m" "s" "l")
         :default "m"
         :value "m"
         :type 'tbs)
        ;; Recipes
        (make-emacspeak-google-tool
         :name "recipes"
         :param "rcp"
         :range '(0 1)
         :default 0
         :value 0
         :type 'tbm)
        ;; places/local:
        (make-emacspeak-google-tool
         :name "places"
         :param "plcs"
         :range '(0 1)
         :default 0
         :value 0
         :type 'tbm)
        ;; patents
        (make-emacspeak-google-tool
         :name "patents"
         :param "pts"
         :range '(0 1)
         :default 0
         :value 0
         :type 'tbm)
        ;; discussions/forums
        (make-emacspeak-google-tool
         :name "group-discussions"
         :param "dsc"
         :range '(0 1)
         :default 0
         :value 0
         :type 'tbm)
        ;;;In-Depth Article
        (make-emacspeak-google-tool
         :name "in-depth"
         :param "ida"
         :range '(0 1)
         :default 0
         :value 0
         :type 'tbs)

        ;; Blog mode
        (make-emacspeak-google-tool
         :name "blog"
         :param "blg"
         :range '(0 1)
         :default 0
         :value 0
         :type 'tbm)
        ;; Books mode
        (make-emacspeak-google-tool
         :name "books"
         :param "bks"
         :range '(0 1)
         :default 0
         :type 'tbm
         :value 0)
        ;; epub
        (make-emacspeak-google-tool
         :name "books-format"
         :param "bft"
         :range '("p" "e")
         :default "e"
         :type 'tbs
         :value "e")
        ;; Books viewability
        (make-emacspeak-google-tool
         :name "books-viewability"
         :param "bkv"
         :range '("a" "f")
         :default "a"
         :value "a"
         :type 'tbs)
        ;; Book Type
        (make-emacspeak-google-tool
         :name "books-type"
         :param "bkt"
         :range '("b" "p" "m")
         :default "b"
         :value "b"
         :type 'tbs)
        ;; Forums Mode
        (make-emacspeak-google-tool
         :name "forums"
         :param "frm"
         :range '(0 1)
         :default 0
         :value 0
         :type 'tbs)
        ;; News Mode
        (make-emacspeak-google-tool
         :name "news"
         :param "nws"
         :range '(0 1)
         :default 0
         :value 0
         :type 'tbm)
        ;; Reviews
        (make-emacspeak-google-tool
         :name "reviews"
         :param "rvw"
         :range '(0 1)
         :default 0
         :value 0
         :type 'tbs)
        ;; Web History Visited
        (make-emacspeak-google-tool
         :name "web-history-visited"
         :param "whv"
         :range '(0 1)
         :default 0
         :type 'tbs
         :value 0)
        ;; Web History Not Visited
        (make-emacspeak-google-tool
         :name "web-history-not-visited"
         :param "whnv"
         :type 'tbs
         :range '(0 1)
         :default 0
         :value 0)
        ;; Images
        (make-emacspeak-google-tool
         :name "images"
         :param "isch"
         :range '(0 1)
         :default 0
         :value 0
         :type 'tbm)
        ;; Structured Snippets
        (make-emacspeak-google-tool
         :name "structured-snippets"
         :param "sts"
         :range '(0 1)
         :default 0
         :value 0
         :type 'tbs)
        ;; sort by date
        (make-emacspeak-google-tool
         :name "sort-by-date"
         :param "std"
         :range '(0 1)
         :default 0
         :value 0
         :type 'tbs)
        ;; Timeline
        (make-emacspeak-google-tool
         :name "timeline"
         :param "tl"
         :range '(0 1)
         :default 0
         :type 'tbs
         :value 0)
        ;; Timeline Low
        (make-emacspeak-google-tool
         :name "timeline-low"
         :param "tll"
         :type 'tbs
         :range "YYYY/MM"
         :default ""
         :value "")
        ;; Date Filter
        (make-emacspeak-google-tool
         :name "date-filter"
         :param "qdr"
         :range '("d"h" "n" " "m" "w" "y")
         :default ""
         :type 'tbs
         :value "")
        ;; Timeline High
        (make-emacspeak-google-tool
         :name "timeline-high"
         :param "tlh"
         :range "YYYY/MM"
         :default ""
         :type 'tbs
         :value "")
        ;; more:commercial promotion with prices
        (make-emacspeak-google-tool
         :name "commercial"
         :param "cpk"
         :range '(0 1)
         :default 0
         :type 'tbs
         :value 0)
        ;; verbatim/literal search
        (make-emacspeak-google-tool
         :name "literal"
         :param "li"
         :range '(0 1)
         :default 0
         :type 'tbs
         :value 0)
        ;; shopping
        (make-emacspeak-google-tool
         :name "Shopping"
         :param "shop"
         :range '(0 1)
         :default 0
         :type 'tbm
         :value 0)
        (make-emacspeak-google-tool
         :name "commercial-prices"
         :param "cp"
         :range '(0 1)
         :default 0
         :type 'tbs
         :value 0)
        ;; less:commercial (demotion)
        (make-emacspeak-google-tool
         :name "non-commercial"
         :param "cdcpk"
         :range '(0 1)
         :default 0
         :type 'tbs
         :value 0)
        ;; soc
        (make-emacspeak-google-tool
         :name "social"
         :param "sa"
         :range '(0 1)
         :default 0
         :type 'tbs
         :value 0)))))

;;}}}
;;{{{  URL Fixup

;; prefix: https://www.google.com/url?q=
;; Suffix: &sa=...

(defun emacspeak-google-canonicalize-result-url (url)
  "Strip out the actual result URL from the redirect wrapper."
  (cl-declare (special emacspeak-google-use-https))
  (url-unhex-string
   (substring url
              (if emacspeak-google-use-https 29 28)
              (string-match "&sa=" url))))

(defun emacspeak-google-result-url-prefix ()
  "Return prefix of result urls."
  (cl-declare (special emacspeak-google-use-https))
  (format "%s://www.google.com/url?q="
          (if emacspeak-google-use-https "https" "http")))

;;}}}
;;{{{Cache query, toolbelt

(defun emacspeak-google-cache-query(query)
  "Setup post process hook to cache google query when rendered."
  (cl-declare (special emacspeak-google-query))
  (let ((cache
         (eval
          `#'(lambda nil
               (setq emacspeak-google-query ,query)))))
    (add-hook 'emacspeak-eww-post-process-hook cache 'at-end)))

(defun emacspeak-google-cache-toolbelt(belt)
  "Setup post process hook to cache google toolbelt when rendered."
  (cl-declare (special emacspeak-google-toolbelt))
  (let ((cache
         (eval 
          `#'(lambda nil
               (setq emacspeak-google-toolbelt' ,belt)))))
    (add-hook 'emacspeak-eww-post-process-hook cache 'at-end)))

;;}}}
;;{{{  google tools

(declare-function eww-current-url "eww" nil)

(defun emacspeak-google-who-links-to-this-page ()
  "Perform a google search to locate documents that link to the
current page."
  (interactive)
  (emacspeak-websearch-google
   (format "link:%s"
           (eww-current-url))))

(defun emacspeak-google-extract-from-cache ()
  "Extract current  page from the Google cache. "
  (interactive)
  (browse-url
   (format "http://webcache.googleusercontent.com/search?q=cache:%s"
           (shr-url-at-point nil))))

(defun emacspeak-google-on-this-site ()
  "Perform a google search restricted to the current WWW site."
  (interactive)
  (emacspeak-websearch-google
   (format "site:%s %s"
           (aref
            (url-generic-parse-url (eww-current-url)) 3)
           (read-from-minibuffer "Search this site for: "))))

(defvar emacspeak-google-related-uri
  "http://www.google.com/search?hl=en&num=25&q=related:")

(defun emacspeak-google-similar-to-this-page (url)
  "Ask Google to find documents similar to this one."
  (interactive
   (list
    (read-from-minibuffer "URL:"
                          (eww-current-url))))
  (cl-declare (special emacspeak-google-related-uri))
  (emacspeak-we-extract-by-id
   "res"
   (format
    "%s%s"
    emacspeak-google-related-uri url)))

;;}}}
;;{{{ Interactive Commands

(cl-loop for this-tool in
         (emacspeak-google-toolbelt)
         do
         (eval
          `(defun
               ,(intern
                 (format
                  "emacspeak-google-toolbelt-change-%s"
                  (emacspeak-google-tool-name this-tool)))
               ()
             ,(format
               "Change  %s in the currently active toolbelt."
               (emacspeak-google-tool-name this-tool))
             (interactive)
             (let*
                 ((belt (emacspeak-google-toolbelt))
                  (tool
                   (cl-find-if
                    #'(lambda (tool)
                        (string= (emacspeak-google-tool-name tool)
                                 ,(emacspeak-google-tool-name this-tool)))
                    belt))
                  (param (emacspeak-google-tool-param tool))
                  (value (emacspeak-google-tool-value tool))
                  (range (emacspeak-google-tool-range tool)))
               (cond
                ((and (listp range)
                      (= 2 (length range)))
                 ;; toggle value
                 (setf (emacspeak-google-tool-value tool)
                       (if (equal value (cl-first range))
                           (cl-second range)
                         (cl-first range))))
                ((listp range)
                 ;; Prompt using completion
                 (setf  (emacspeak-google-tool-value tool)
                        (completing-read
                         "Set tool to: "
                         range)))
                ((stringp range)
                 (setf (emacspeak-google-tool-value tool)
                       (read-from-minibuffer  range)))
                (t (error "Unexpected type!")))
               (let
                   ((emacspeak-websearch-google-options
                     (concat
                      (emacspeak-google-toolbelt-to-tbs belt)
                      (emacspeak-google-toolbelt-to-tbm belt))))
                 (emacspeak-google-cache-toolbelt belt)
                 (emacspeak-websearch-google
                  (or emacspeak-google-query
                      (gweb-google-autocomplete))))))))

(defun emacspeak-google-toolbelt-names ()
  "Return memoized cache of names."
  (cl-declare (special emacspeak-google-toolbelt-names))
  (or emacspeak-google-toolbelt-names
      (setq emacspeak-google-toolbelt-names
            (cl-loop
             for b in emacspeak-google-toolbelt
             collect (emacspeak-google-tool-name b)))))

(defun emacspeak-google-toolbelt-names-from-toolbelt (toolbelt)
  "Return list of names in toolbelt."
  (cl-loop
   for b in toolbelt
   collect (emacspeak-google-tool-name b)))

(defun emacspeak-google-toolbelt-change ()
  "Command to change values in the toolbelt and execute the query."
  (interactive)
  (call-interactively
   (read
    (format  "emacspeak-google-toolbelt-change-%s"
             (completing-read
              "Toolbelt: "
              (emacspeak-google-toolbelt-names)
              nil t)))))

(defun emacspeak-google-show-toolbelt()
  "Reload search page with toolbelt showing."
  (interactive)
  (cl-declare (special emacspeak-google-query
                       emacspeak-websearch-google-options))
  (let ((emacspeak-websearch-google-options "&tbo=1"))
    (emacspeak-websearch-google emacspeak-google-query)))

(declare-function emacspeak-eww-next-h1 "emacspeak-eww" (&optional speak))
(declare-function shr-url-at-point "shr" (image-url))

(defun emacspeak-google-open-link ()
  "Open Google link under point."
  (interactive)
  (cl-declare (special ems--websearch-google-filter))
  (let ((url (shr-url-at-point nil)))
    (cl-assert url t "No link under point.")
    (add-hook
     'emacspeak-eww-post-process-hook
     #'(lambda ()
         (emacspeak-eww-next-h1  'speak)))      
    (emacspeak-we-extract-by-id-list
     ems--websearch-google-filter
     url)))

;;}}}
;;{{{ Sign in, Sign out:

(defvar emacspeak-google-sign-out-url
  "http://www.google.com/accounts/Logout"
  "URL for signing out of Google.")

(defvar emacspeak-google-sign-in-url
  (concat
   "https://accounts.google.com/ServiceLogin"
   "?hl=en&continue=https://www.google.com/")
  "URL for signing in to Google.")

(defun emacspeak-google-sign-in ()
  "Sign in to Google."
  (interactive)
  (cl-declare (special emacspeak-google-sign-in-url))
  (browse-url emacspeak-google-sign-in-url))

(defun emacspeak-google-sign-out ()
  "Sign out to Google."
  (interactive)
  (cl-declare (special emacspeak-google-sign-out-url))
  (browse-url emacspeak-google-sign-out-url))

;;}}}
;;{{{  keymap

(define-prefix-command  'emacspeak-google-command
                        'emacspeak-google-keymap)
(cl-declaim (special emacspeak-google-keymap))
(cl-loop
 for k in
 '(
   ("." emacspeak-google-toolbelt-change)("." emacspeak-google-toolbelt-change)
   ("A" emacspeak-google-sign-in)
   ("a" emacspeak-google-sign-out)
   ("c" emacspeak-google-extract-from-cache)
   ("g" emacspeak-websearch-google)
   ("o" emacspeak-google-open-link)
   ("i" emacspeak-google-what-is-my-ip)
   ("l" emacspeak-google-who-links-to-this-page)
   ("s" emacspeak-google-similar-to-this-page)
   )
 do
 (emacspeak-keymap-update emacspeak-google-keymap k))

;;}}}
;;{{{ Advice GMaps:

(defadvice gmaps (after emacspeak pre act comp)
  "Provide  auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))
(cl-loop for f in
         '(gmaps-driving-directions
           gmaps-bicycling-directions
           gmaps-walking-directions gmaps-transit-directions
           gmaps-places-nearby gmaps-places-search)
         do
         (eval
          `(defadvice ,f (after emacspeak pre act comp)
             "speak."
             (when (ems-interactive-p)
               (emacspeak-auditory-icon 'task-done)
               (emacspeak-speak-rest-of-buffer)))))

(defadvice gmaps-set-current-location (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-header-line)))

(defadvice gmaps-set-current-radius (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (message "Radius set to %s. " gmaps-current-radius)))

(defadvice gmaps-place-details (around emacspeak pre act comp)
  "speak."
  (cond
   ((ems-interactive-p)
    ad-do-it
    (emacspeak-speak-region  (point)
                             (or
                              (next-single-property-change
                               (point) 'place-details)
                              (point-max))))
   (t ad-do-it))
  ad-return-value)

;;}}}
;;{{{ TTS:

(defcustom emacspeak-google-tts-default-language "en-us"
  "Default language used for Google TTS."
  :type 'string
  :group 'emacspeak-google)

(defvar emacspeak-google-tts-rest-uri
  "https://www.google.com/speech-api/v1/synthesize?lang=%s&text=%s"
  "REST endpoint for network speech synthesis.")
;;;###autoload
(defun emacspeak-google-tts (text &optional lang)
  "Google Network TTS.
Optional interactive prefix arg `lang' specifies  language identifier."
  (interactive
   (list
    (read-from-minibuffer "Text: ")
    current-prefix-arg))
  (cl-declare (special
               emacspeak-google-tts-default-language
               emacspeak-google-tts-rest-uri emacspeak-m-player-program))
  (or lang (setq lang "en-us"))
  (unless (stringp lang) (setq lang (read-string  "Lang:")))
  (let ((url (format emacspeak-google-tts-rest-uri
                     (or lang emacspeak-google-tts-default-language)
                     (url-hexify-string  text))))
    (kill-new url)
    (start-process
     "google-tts" nil  emacspeak-m-player-program url)))

;;;###autoload
(defun emacspeak-google-tts-region (start end &optional ask-lang)
  "Speak region using Google Network TTS."
  (interactive
   (list (region-beginning) (region-end) current-prefix-arg))
  (emacspeak-google-tts (buffer-substring-no-properties start end) ask-lang))

;;}}}
;;{{{ What Is My IP:

(defun emacspeak-google-what-is-my-ip ()
  "Show my public IP"
  (interactive)
  (emacspeak-websearch-google "what+is+my+ip"))

;;}}}
;;{{{ Google Knowledge Graph:

;; Google Knowledge Graph Search API  
;;  G https://developers.google.com/knowledge-graph/

(defcustom emacspeak-google-kg-key  nil
  "API Key for Google Knowledge Graph."
  :type
  '(choice
    (const :tag "None" "")
    (string :tag "Key" :value ""))
  :group 'emacspeak-google)

(defvar emacspeak-google-kg-rest-end-point
  (concat
   "https://kgsearch.googleapis.com/v1/entities:search"
   "?%s=%s&key=%s&indent=1&limit=%s")
  "Rest end-point for KG Search.")

(defun emacspeak-google-kg-id-uri (id)
  "Return URL for KG Search by id."
  (cl-declare (special emacspeak-google-kg-rest-end-point))
  (format
   emacspeak-google-kg-rest-end-point
   "ids"
   (url-hexify-string (substring id 3))
   emacspeak-google-kg-key
   1))

(defun emacspeak-google-kg-query-uri (query &optional limit)
  "Return URL for KG Search."
  (cl-declare (special emacspeak-google-kg-rest-end-point))
  (or limit (setq limit 5))
  (format
   emacspeak-google-kg-rest-end-point
   "query"
   (url-hexify-string query)
   emacspeak-google-kg-key
   limit))

(defun emacspeak-google-kg-json-ld (query &optional limit)
  "Return JSON-LD structure."
  (or limit (setq limit 5))
  (g-json-from-url
   (emacspeak-google-kg-query-uri query limit)))

(defun emacspeak-google-kg-results (query &optional limit)
  "Return list of results."
  (or limit (setq limit 5))
  (cl-map  'list
           #'(lambda (r) (g-json-get 'result r))
           (g-json-get 'itemListElement
                       (emacspeak-google-kg-json-ld query limit))))

(defun emacspeak-google-kg-format-result (result)
  "Format result as HTML."
  (let-alist result
    (format
     "<p><a href='%s'>%s</a> is a <code>[%s]</code>.
<strong>%s</strong></p>
<p>%s</p>
<p><a href='%s'>Id: %s</a>
<img src='%s'/></p>\n"
     (g-json-get 'url .detailedDescription) .name
     (mapconcat #'identity .@type ", ")
     .description
     (or (g-json-get 'articleBody .detailedDescription) "")
     (emacspeak-google-kg-id-uri .@id)
     .@id
     (g-json-get 'contentUrl .image))))

(defun emacspeak-google-knowledge-search (query &optional limit)
  "Perform a Google Knowledge Graph search.
Optional interactive prefix arg `limit' prompts for number of
results, default is 1."
  (interactive "sQuery:\nP")
  (setq limit
        (cond
         (limit  (read-number "Number of results: "))
         (t  5)))
  (let ((results (emacspeak-google-kg-results query limit)))
    (unless results (error "No results"))
    (with-temp-buffer
      (insert (format "<html><head><title>%s</title></head><body>\n" query))
      (cond
       ((> limit 1)
        (insert "<ol>\n")
        (cl-loop
         for r in results do
         (insert "<li>")
         (insert (emacspeak-google-kg-format-result r))
         (insert "</li>\n"))
        (insert "</ol>\n"))
       (t(insert  (emacspeak-google-kg-format-result (cl-first results)))))
      (insert "</body></html>\n")
      (emacspeak-eww-autospeak)
      (browse-url-of-buffer))))

;;}}}
;;{{{youtube to rss:

(defun emacspeak-google-yt-feed (url) 
  "Turn YT Channel or Playlist url into an RSS feed and open it."
  (interactive (list (emacspeak-eww-read-url)))
  (let ((r "https://www.youtube.com/feeds/videos.xml?%s=%s")
        (u (url-generic-parse-url url))
        (params nil)
        (playlist nil)
        (channel nil))
    (when (string-match "list=" url)
      (setq params
            (mapcar
             #'      (lambda         (s) (split-string s "="))
             (split-string (url-filename u) "&")))
      (setq playlist
            (or 
             (cadr (assoc "list" params))
             (cadr (assoc "/playlist?list" params)))))
    (when (string-match "channel" url)
      (setq channel
            (substring url
                       (+ (string-match "channel" url)
                          (length "channel/")))))
    (cond
     (playlist
      (kill-new (format r "playlist_id" playlist))
      (funcall-interactively
       #'emacspeak-feeds-atom-display
       (format r "playlist_id" playlist)))
     (channel
      (kill-new (format r "channel_id" channel))
      (funcall-interactively
       #'emacspeak-feeds-atom-display
       (format r "channel_id" channel)))
     (t (error "URL is not a channel or playlist.")))))

;;}}}
(provide 'emacspeak-google)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}
