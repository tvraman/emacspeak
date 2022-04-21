;;; gweb.el --- Google Search  -*- lexical-binding: t; -*-
;; $Id$
;; $Author: raman $
;; Description:  AJAX Search -> Lisp
;; Keywords: Google   AJAX API
;;{{{  LCD Archive entry:

;; LCD Archive Entry:
;; gcal| T. V. Raman |tv.raman.tv@gmail.com
;; An emacs interface to Reader|
;; $Date: 2006/09/28 17:47:44 $ |
;;  $Revision: 1.30 $ |
;; Location undetermined
;; License: GPL
;; 

;;}}}
;;{{{ Copyright:

;; Copyright (c) 2006 and later, Google Inc.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without modification,
;; are permitted provided that the following conditions are met:

;;     * Redistributions of source code must retain the above copyright notice,
;;       this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above copyright notice,
;;       this list of conditions and the following disclaimer in the documentation
;;       and/or other materials provided with the distribution.
;;     * The name of the author may not be used to endorse or promote products
;;       derived from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
;; WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;; Commentary: Provide Google services --- such as search,
;; search-based completion etc.  For use from within Emacs tools.
;; This is meant to be fast and efficient --- and uses WebAPIs as
;; opposed to HTML scraping.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'json)
(require 'g-utils)

;;}}}
;;{{{ Customizations

;;}}}
;;{{{ Variables

(defvar gweb-referer "http://emacspeak.sf.net"
  "Referer URL to send to the API.")
(defvar gweb-history nil
  "History of Google Search queries.")

(put 'gweb-history 'history-length 100)
(put 'gweb-history 'history-delete-duplicates t)

(defvar gweb-completion-flag nil
  "Flag that records  Google Suggest in progress.")
;; This is dynamically scoped:
(defvar gweb-completion-corpus ""
  "Corpus to use for completion. Let-bind this for using a different
  corpus.
Default == Web Search.
yt == Youtube .
n == News.")

;;}}}
;;{{{ google suggest helper:

(defvar gweb-g-suggest-url
  "http://suggestqueries.google.com/complete/search?ds=%s&q=%s&client=chrome"
  "Query Suggest: ds param is empty for Search, Youtube: yt, News: n")

(defun gweb-suggest (input &optional corpus)
  "Get completion list from Google Suggest."
  (cl-declare (special gweb-completion-corpus gweb-g-suggest-url))
  (unless corpus (setq corpus gweb-completion-corpus))
  (when input
    (let* ((url (format gweb-g-suggest-url corpus (g-url-encode input)))
           (js (g-json-from-url url)))
      (setq js (aref js 1))
      (cl-loop
       for e across js collect
       (replace-regexp-in-string "</?b>" "" e)))))

(defvar gweb-google-suggest-metadata
  '(metadata .
             (
;; Google suggest returns suggestions already sorted
              (display-sort-function . identity)
                                        ; add annots function here
              ))
  "Metadata returned by google-suggest completer.")

(defun gweb-suggest-completer (string predicate action)
  "Generate completions using Google Suggest. "
  (cl-declare (special gweb-completion-corpus))
  (when (and (sit-for 0.2)(stringp string) (> (length string)  0))
    (save-current-buffer
      (set-buffer
       (let ((window (minibuffer-selected-window)))
         (if (window-live-p window)
             (window-buffer window)
           (current-buffer))))
      (cond
       ((eq action 'metadata) gweb-google-suggest-metadata)
       (t
        (complete-with-action action
                              (gweb-suggest string gweb-completion-corpus)
                              string predicate))))))

(defun gweb--autocomplete-helper (&optional prompt)
  "Helper: Read user input using Google Suggest for auto-completion.
Uses corpus found in gweb-completion-corpus"
  (let (
        (gweb-completion-flag t)
        (completion-ignore-case t)
        (word (thing-at-point 'word))
        (query nil))
    (setq gweb-history (cl-remove-duplicates gweb-history :test #'string-equal))
    (setq query
          (completing-read
           (or prompt "Google: ")
           'gweb-suggest-completer      ; collection
           nil nil                      ; predicate required-match
           word                         ; initial input
           'gweb-history))
    (g-url-encode query)))

(defun gweb-google-autocomplete (&optional prompt)
  "Autocomplete using Google Search corpus."
  (let ((gweb-completion-corpus ""))
    (gweb--autocomplete-helper (or prompt "Google: "))))

(defun gweb-youtube-autocomplete (&optional prompt)
  "Autocomplete using Youtube Search corpus."
  (let ((gweb-completion-corpus "yt"))
    (gweb--autocomplete-helper (or prompt "YouTube: "))))

(defun gweb-news-autocomplete (&optional prompt)
  "Autocomplete using News Search corpus."
  (let ((gweb-completion-corpus "n"))
    (gweb--autocomplete-helper (or prompt "News: "))))
(cl-loop
 for f in
 '(ido-complete-space minibuffer-complete-word) do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Fix up ido-complete-space for use with Google autocomplete."
     (cond
      (gweb-completion-flag (insert-char ?\ ))
      (t ad-do-it))
     (emacspeak-speak-word)
     ad-return-value)))

;;}}}
(provide 'gweb)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}
