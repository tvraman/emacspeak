;;; emacspeak-google.el --- Google Search Tools  -*- lexical-binding: t; -*-
;; $Id: emacspeak-google.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;; $Author: tv.raman.tv $
;; Description:  Speech-enable GOOGLE An Emacs Interface to google
;; Keywords: Emacspeak,  Audio Desktop google
;;;   LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;;
;;  $Revision: 4532 $ |
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
;; MERCHANTABILITY or FITNGOOGLE FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Commentary:
;; There are a number of search tools that can be implemented on
;; the Google search page --- in a JS-powered browser, these
;; show up as the Google tool-belt.
;; This module implements a minor mode for use in Google result
;; pages that enables these tools via single keyboard commands.
;; Originally all options were available as tbs=p:v
;; Now, some specialized searches, e.g. blog search are tbm=
;;; Code:

;;   Required modules:

(eval-when-compile (require 'cl-lib))
(cl-declaim  (optimize  (safety 0) (speed 3)))
(eval-when-compile (require 'derived))
(require 'emacspeak-preamble)
(require 'emacspeak-we)
(require 'gweb)

;;;  Data Structures

;; One tool on a tool-belt

(cl-defstruct ems--g-tool
  name ; human readable
  param ; url param bit
  range ; range of possible values
  default
  value ; current setting
  type ;tbs/tbm
  )

(defvar-local emacspeak-google-query nil
  "Current Google Query. ")

(defvar-local emacspeak-google-toolbelt nil
  "List of tools on the toolbelt.")

(defun emacspeak-google-toolbelt-to-tbm (belt)
  "Return value for use in tbm parameter in search queries."
  (let
      ((settings
        (delq nil
              (mapcar
               #'(lambda (tool)
                   (when (eq 'tbm (ems--g-tool-type tool))
                     (cond
                      ((equal (ems--g-tool-value tool)
                              (ems--g-tool-default tool))
                       nil)
                      (t (format "%s"
                                 (ems--g-tool-param tool))))))
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
                   (when (eq 'tbs (ems--g-tool-type tool))
                     (cond
                      ((equal (ems--g-tool-value tool)
                              (ems--g-tool-default tool))
                       nil)
                      (t (format "%s:%s"
                                 (ems--g-tool-param tool)
                                 (ems--g-tool-value tool))))))
               belt))))
    (when settings
      (concat "&tbs="
              (mapconcat #'identity settings ",")))))
(defvar-local emacspeak-google-toolbelt-names  nil
  "Cache of available toolbelt names.")

(defun emacspeak-google-toolbelt ()
  "Returns buffer-local toolbelt or a a newly initialized toolbelt."
  (cl-declare (special emacspeak-google-toolbelt))
  (or emacspeak-google-toolbelt
      (setq
       emacspeak-google-toolbelt
       (list
        ;; video vid: 1/0
        (make-ems--g-tool
         :name "video"
         :param "vid"
         :range '(0 1)
         :default 0
         :type 'tbm
         :value 0)
        ;; discussions/forums
        (make-ems--g-tool
         :name "group-discussions"
         :param "dsc"
         :range '(0 1)
         :default 0
         :value 0
         :type 'tbm)
        ;;;In-Depth Article
        (make-ems--g-tool
         :name "in-depth"
         :param "ida"
         :range '(0 1)
         :default 0
         :value 0
         :type 'tbs)

        ;; Books mode
        (make-ems--g-tool
         :name "books"
         :param "bks"
         :range '(0 1)
         :default 0
         :type 'tbm
         :value 0)
        ;; epub
        (make-ems--g-tool
         :name "books-format"
         :param "bft"
         :range '("p" "e")
         :default "e"
         :type 'tbs
         :value "e")
        ;; Books viewability
        (make-ems--g-tool
         :name "books-viewability"
         :param "bkv"
         :range '("a" "f")
         :default "a"
         :value "a"
         :type 'tbs)
        ;; Book Type
        (make-ems--g-tool
         :name "books-type"
         :param "bkt"
         :range '("b" "p" "m")
         :default "b"
         :value "b"
         :type 'tbs)
        ;; Forums Mode
        (make-ems--g-tool
         :name "forums"
         :param "frm"
         :range '(0 1)
         :default 0
         :value 0
         :type 'tbs)
        ;; News Mode
        (make-ems--g-tool
         :name "news"
         :param "nws"
         :range '(0 1)
         :default 0
         :value 0
         :type 'tbm)
        ;; Reviews
        (make-ems--g-tool
         :name "reviews"
         :param "rvw"
         :range '(0 1)
         :default 0
         :value 0
         :type 'tbs)
        ;; Web History Visited
        (make-ems--g-tool
         :name "web-history-visited"
         :param "whv"
         :range '(0 1)
         :default 0
         :type 'tbs
         :value 0)
        ;; Web History Not Visited
        (make-ems--g-tool
         :name "web-history-not-visited"
         :param "whnv"
         :type 'tbs
         :range '(0 1)
         :default 0
         :value 0)
        ;; Images
        (make-ems--g-tool
         :name "images"
         :param "isch"
         :range '(0 1)
         :default 0
         :value 0
         :type 'tbm)
        ;; sort by date
        (make-ems--g-tool
         :name "sort-by-date"
         :param "std"
         :range '(0 1)
         :default 0
         :value 0
         :type 'tbs)
        ;; Timeline
        (make-ems--g-tool
         :name "timeline"
         :param "tl"
         :range '(0 1)
         :default 0
         :type 'tbs
         :value 0)
        ;; Timeline Low
        (make-ems--g-tool
         :name "timeline-low"
         :param "tll"
         :type 'tbs
         :range '("YYYY" "MM")
         :default ""
         :value "")
        ;; Timeline High
        (make-ems--g-tool
         :name "timeline-high"
         :param "tlh"
         :range '("YYYY" "mm")
         :default ""
         :type 'tbs
         :value "")
        ;; more:commercial promotion with prices
        (make-ems--g-tool
         :name "commercial"
         :param "cpk"
         :range '(0 1)
         :default 0
         :type 'tbs
         :value 0)
        ;; verbatim/literal search
        (make-ems--g-tool
         :name "literal"
         :param "li"
         :range '(0 1)
         :default 0
         :type 'tbs
         :value 0)
        ;; shopping
        (make-ems--g-tool
         :name "Shopping"
         :param "shop"
         :range '(0 1)
         :default 0
         :type 'tbm
         :value 0)
        (make-ems--g-tool
         :name "commercial-prices"
         :param "cp"
         :range '(0 1)
         :default 0
         :type 'tbs
         :value 0)
        ;; less:commercial (demotion)
        (make-ems--g-tool
         :name "non-commercial"
         :param "cdcpk"
         :range '(0 1)
         :default 0
         :type 'tbs
         :value 0)
        ;; soc
        (make-ems--g-tool
         :name "social"
         :param "sa"
         :range '(0 1)
         :default 0
         :type 'tbs
         :value 0)))))

;;;   URL Fixup

;; prefix: https://www.google.com/url?q=
;; Suffix: &sa=...

(defun emacspeak-google-canonicalize-result-url (url)
  "Strip out the actual result URL from the redirect wrapper."
  (url-unhex-string
   (substring url 29 (string-match "&sa=" url))))

(defun emacspeak-google-result-url-prefix ()
  "Return prefix of result urls."
  "https://www.google.com/url?q=")

;;; Cache query, toolbelt

(defun emacspeak-google-cache-query(query)
  "Setup post process hook to cache google query when rendered."
  (cl-declare (special emacspeak-google-query))
  (let ((cache
         (eval
          `#'(lambda nil
               (setq emacspeak-google-query ,query)))))
    (add-hook 'emacspeak-eww-post-hook cache 'at-end)))

(defun emacspeak-google-cache-toolbelt(belt)
  "Setup post process hook to cache google toolbelt when rendered."
  (cl-declare (special emacspeak-google-toolbelt))
  (let ((cache
         (eval
          `#'(lambda nil
               (setq emacspeak-google-toolbelt' ,belt)))))
    (add-hook 'emacspeak-eww-post-hook cache 'at-end)))

;;;   google tools

(declare-function eww-current-url "eww" nil)
(declare-function
 emacspeak-websearch-google "emacspeak-websearch" (arg1 &optional arg2))

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

;;;  Interactive Commands

(cl-loop
 for this-tool in (emacspeak-google-toolbelt) do
 (eval
  `(defun ,(intern (format "ems--g-tb-set-%s" (ems--g-tool-name this-tool))) ()
     ,(format
       "Change  %s in the currently active toolbelt."
       (ems--g-tool-name this-tool))
     (interactive)
     (let*
         ((belt (emacspeak-google-toolbelt))
          (tool
           (cl-find-if
            #'(lambda (tool)
                (string= (ems--g-tool-name tool)
                         ,(ems--g-tool-name this-tool)))
            belt))
          (param (ems--g-tool-param tool))
          (value (ems--g-tool-value tool))
          (range (ems--g-tool-range tool))
          (slot nil))
       (cond
        ((and (listp range) (= 2 (length range))) ;; toggle value
         (setq slot
               (if (equal value (cl-first range))
                   (cl-second range)
                 (cl-first range)))
         (setf (ems--g-tool-value tool) slot))
        ((listp range)
         ;; Prompt using completion
         (setq slot
               (completing-read
                "Set tool to: "
                range)))
        (setf   (ems--g-tool-value tool) slot)
        ((stringp range)
         (setq slot
               (read-from-minibuffer  range))
         (setf  (ems--g-tool-value tool) slot))
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
             collect (ems--g-tool-name b)))))

(defun emacspeak-google-toolbelt-names-from-toolbelt (toolbelt)
  "Return list of names in toolbelt."
  (cl-loop
   for b in toolbelt
   collect (ems--g-tool-name b)))

(defun emacspeak-google-toolbelt-change ()
  "Command to change values in the toolbelt and execute the query."
  (interactive)
  (call-interactively
   (read
    (format  "ems--g-tb-set-%s"
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
     'emacspeak-eww-post-hook
     #'(lambda ()
         (emacspeak-eww-next-h1  'speak)))
    (emacspeak-we-extract-by-id-list
     ems--websearch-google-filter
     url)))

;;;  Sign in, Sign out:

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

;;;   keymap

(define-prefix-command  'emacspeak-google-command
                        'emacspeak-google-keymap)
(cl-declaim (special emacspeak-google-keymap))
(cl-loop
 for k in
 '(
   ("." emacspeak-google-toolbelt-change)
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

;;;  Advice GMaps:

(defadvice gmaps (after emacspeak pre act comp)
  "Provide  auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-icon 'open-object)
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
               (emacspeak-icon 'task-done)
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

;;;  TTS:

(define-prefix-command 'emacspeak-google-tts)

(defvar-local emacspeak-google-tts-default-language "en-us"
  "Default language used for Google TTS.")

(defvar emacspeak-google-tts-rest-uri
  "https://www.google.com/speech-api/v1/synthesize?lang=%s&text=%s"
  "REST endpoint for network speech synthesis.")
;;;###autoload
(defun emacspeak-google-tts-speak (text &optional lang)
  "Google Network TTS.
Optional interactive prefix arg `lang' specifies  language identifier
which becomes buffer-local."
  (interactive
   (list
    (read-from-minibuffer "Text: ")
    current-prefix-arg))
  (cl-declare (special
               emacspeak-mpv
               emacspeak-google-tts-default-language
               emacspeak-google-tts-rest-uri ))
  (when current-prefix-arg
    (unless (stringp lang)
      (setq lang
            (read-string  "Lang:" nil nil
                          emacspeak-google-tts-default-language))
      (setq emacspeak-google-tts-default-language lang)))
  (let ((url (format emacspeak-google-tts-rest-uri
                     (or lang emacspeak-google-tts-default-language)
                     (url-hexify-string  text))))
    (kill-new url)
    (start-process
     "google-tts" nil  emacspeak-mpv  url)))

;;;###autoload
(defun emacspeak-google-tts-region (start end &optional ask-lang)
  "Speak region using Google Network TTS."
  (interactive
   (list (region-beginning) (region-end) current-prefix-arg))
  (emacspeak-google-tts-speak
   (buffer-substring-no-properties start end) ask-lang))

;;;###autoload
(defun emacspeak-google-tts-line (&optional lang)
  "TTS line using network TTS.
Use default voice for buffer."
  (interactive "P")
  (dtk-notify (format "%d" (line-number-at-pos (point))))
  (emacspeak-google-tts-region
   (line-beginning-position) (line-end-position) lang))

;;;###autoload
(defun emacspeak-google-tts-next-line ()
  "TTS next line using network TTS.
Use default voice for buffer."
  (interactive)
  (forward-line 1)
  (skip-syntax-forward "^w_")
  (dtk-notify (format "%d" (line-number-at-pos (point))))
  (emacspeak-google-tts-region
   (line-beginning-position) (line-end-position)))

;;;###autoload
(defun emacspeak-google-tts-previous-line ()
  "TTS previous line using network TTS.
Use default voice for buffer."
  (interactive)
  (forward-line -1)
  (skip-syntax-backward "^w_")
  (goto-char (line-beginning-position))
  (dtk-notify (format "%d" (line-number-at-pos (point))))
  (emacspeak-google-tts-region
   (line-beginning-position) (line-end-position)))

(cl-declaim (special emacspeak-google-tts))
(cl-loop
 for b in
 '(("l" emacspeak-google-tts-line)
   ("p" emacspeak-google-tts-previous-line)
   ("n" emacspeak-google-tts-next-line)
   ("r" emacspeak-google-tts-region)
   ("s" emacspeak-google-tts-speak))
 do
 (emacspeak-keymap-update  emacspeak-google-tts b))

;;; repeat-mode
(map-keymap
 (lambda (_key cmd)
   (when (symbolp cmd)
     (put cmd 'repeat-map 'emacspeak-google-tts)))
 emacspeak-google-tts)

;;;  What Is My IP:

(defun emacspeak-google-what-is-my-ip ()
  "Show my public IP"
  (interactive)
  (emacspeak-websearch-google "what+is+my+ip"))

;;;  Google Knowledge Graph:

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
;;; Google from Calendar:
(declare-function calendar-cursor-to-date "calendar" (&optional error event))



;;;###autoload
(defun emacspeak-google-search-after ()
  "Google from calendar --- add after:date-at-point."
  (interactive)
  (cl-assert (eq major-mode 'calendar-mode) t "Not in calendar.")
  (let ((date
         (format " after:%d/%02d/%02d"
                 (cl-third (calendar-cursor-to-date))
                 (cl-first (calendar-cursor-to-date))
                 (cl-second (calendar-cursor-to-date)))))
    (funcall-interactively
     'emacspeak-websearch-google
     (url-encode-url (concat (read-from-minibuffer "Search After") date)))))

;;;###autoload
(defun emacspeak-google-search-before ()
  "Google from calendar --- add before:date-at-point."
  (interactive)
  (cl-assert (eq major-mode 'calendar-mode) t "Not in calendar.")
  (let ((date
         (format " before:%d/%02d/%02d"
                 (cl-third (calendar-cursor-to-date))
                 (cl-first (calendar-cursor-to-date))
                 (cl-second (calendar-cursor-to-date)))))
    (funcall-interactively
     'emacspeak-websearch-google
     (url-encode-url
      (concat (read-from-minibuffer "Date Search Before: ") date)))))

;;; youtube to rss:

(defun emacspeak-google-yt-feed (url)
  "Turn YT Channel or Playlist url into an RSS feed and open it."
  (interactive (list (ems--read-url)))
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

(provide 'emacspeak-google)
;;;  end of file
