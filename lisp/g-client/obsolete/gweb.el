;;; gweb.el --- Google Search  -*- lexical-binding: t; -*-
;;;$Id$
;;; $Author: raman $
;;; Description:  AJAX Search -> Lisp
;;; Keywords: Google   AJAX API
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; gcal| T. V. Raman |raman@cs.cornell.edu
;;; An emacs interface to Reader|
;;; $Date: 2006/09/28 17:47:44 $ |
;;;  $Revision: 1.30 $ |
;;; Location undetermined
;;; License: GPL
;;;

;;}}}
;;{{{ Copyright:

;;; Copyright (c) 2006 and later, Google Inc.
;;; All rights reserved.

;;; Redistribution and use in source and binary forms, with or without modification,
;;; are permitted provided that the following conditions are met:

;;;     * Redistributions of source code must retain the above copyright notice,
;;;       this list of conditions and the following disclaimer.
;;;     * Redistributions in binary form must reproduce the above copyright notice,
;;;       this list of conditions and the following disclaimer in the documentation
;;;       and/or other materials provided with the distribution.
;;;     * The name of the author may not be used to endorse or promote products
;;;       derived from this software without specific prior written permission.

;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
;;; WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction
;;; Commentary:
;;; Provide Google services --- such as search, search-based completion etc.
;;; For use from within Emacs tools.
;;; This is meant to be fast and efficient --- and uses WebAPIs as opposed to HTML  scraping.
;;; Code:
;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'json)
(require 'g-utils)

;;}}}
;;{{{ Customizations

(defgroup gweb nil
  "Google search"
  :group 'g)

;;}}}
;;{{{ Variables

(defvar gweb-base-url
  "http://ajax.googleapis.com/ajax/services/search/%s?v=1.0&q=%%s"
  "Base URL template for Websearch command.")

(defvar gweb-web-url
  (format gweb-base-url "web")
  "URL template for Websearch command.")

(defvar gweb-news-url
  (format gweb-base-url "news")
  "URL template for News Search  command.")

(defvar gweb-referer "http://emacspeak.sf.net"
  "Referer URL to send to the API.")

;;}}}
;;{{{ google suggest helper:

;;; Get search completions from Google
;;; Service Names: (corpus)
;; youtube : 'youtube',
;;                      books : 'books',
;;                      products : 'products-cc',
;;                      news : 'news-cc',
;;                      img : 'img',
;;                      web : 'psy'
;; youtube: 'youtube

(defvar gweb-suggest-url
  "http://clients1.google.com/complete/search?json=t&nohtml=t&nolabels=t&client=%s&q=%s"
  "URL  that gets suggestions from Google as JSON.")
;;; corpus is ds=n for News
;;; ds=r for recipes

(defun gweb-suggest (input &optional corpus)
  "Get completion list from Google Suggest."
  (declare (special gweb-suggest-url))
  (unless (> (length input) 0) (setq input minibuffer-default))
  (unless corpus (setq corpus "psy"))
  (g-using-scratch
   (let ((js nil)
         (url (format gweb-suggest-url corpus (g-url-encode input))))
     (call-process
      g-curl-program
      nil t nil
      "-s" url)
     (goto-char (point-min))
     (setq js (json-read))
     (setq js  (aref js 1))
     (cl-loop for e across js
           collect
           (replace-regexp-in-string
            "</?b>" ""
            ;;; note: psy is different:
            (if (string= corpus "psy")
                (aref e 0)
              e))))))

(defvar gweb-google-suggest-metadata
  '(metadata .
             (
                                        ; Google suggest returns suggestions already sorted
              (display-sort-function . identity)
                                        ; add annots function here
              ))
  "Metadata returned by google-suggest completer.")

(defun gweb-suggest-completer (string predicate action)
  "Generate completions using Google Suggest. "
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
                            (gweb-suggest string)
                            string predicate)))))

;;{{{  Generate suggest handlers for Google properties
(cl-loop for c in
      '("news-cc" "products-cc" "youtube" "books" "img")
      do
      (eval
       `(defun
            , (intern
               (format  "gweb-%s-suggest-completer" c))
            (string predicate action)
          ,(format
            "Generate completions using Google %s Suggest. " c)
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
                                    (gweb-suggest string ,c)
                                    string predicate)))))))

;;}}}

(defvar gweb-history nil
  "History of Google Search queries.")

(put 'gweb-history 'history-length 100)
(put 'gweb-history 'history-delete-duplicates t)

;;; Emacs 23 and beyond:
;;; i.e. if complete-with-action is defined

(defun gweb-google-autocomplete (&optional prompt)
  "Read user input using Google Suggest for auto-completion."
  (let* ((minibuffer-completing-file-name t) ;; accept spaces
         (completion-ignore-case t)
         (word (thing-at-point 'word))
         (query nil))
    (setq gweb-history (remove-duplicates gweb-history :test #'string-equal))
    (setq query
          (completing-read
           (or prompt "Google: ")
           'gweb-suggest-completer     ; collection
           nil nil                     ; predicate required-match
           word                        ; initial input
           'gweb-history))
    (g-url-encode query)))

;;;###autoload

;;; For news:



;;}}}
;;{{{ Interactive Commands:

;;; Need to be smarter about guessing default term
;;; thing-at-point can return an empty string,
;;; and this is not a good thing for Google Suggest which will error out.
(defvar gweb-search-results-handler nil
  "Hook for saving away retrieved Google results.")

;;;###autoload
(defun gweb-google-at-point (search-term &optional refresh)
  "Google for term at point, and display top result succinctly.
Attach URL at point so we can follow it later --- subsequent invocations of this command simply follow that URL.
Optional interactive prefix arg refresh forces this cached URL to be refreshed."
  (interactive
   (list
    (unless(and (not current-prefix-arg)
                (get-text-property (point) 'lucky-url))
      (gweb-google-autocomplete))
    current-prefix-arg))
  (declare (special gweb-search-results-handler))
  (cond
   ((and (not refresh)
         (get-text-property (point) 'lucky-url))
    (browse-url (get-text-property (point) 'lucky-url)))
   (t
    (let* ((results (gweb-web-results  search-term))
           (lucky (when results (aref results 0)))
           (inhibit-read-only t)
           (bounds (bounds-of-thing-at-point 'word))
           (modified-p (buffer-modified-p))
           (title (g-json-get 'titleNoFormatting lucky))
           (url (g-json-get 'url lucky))
           (content (shell-command-to-string
                     (format
                      "echo '%s' | lynx -dump -stdin 2>/dev/null"
                      (g-json-get 'content lucky)))))
      (when bounds
        (add-text-properties   (car bounds) (cdr bounds)
                               (list 'lucky-url url
                                     'face 'highlight)))
      (pushnew lucky minibuffer-history)
      (set-buffer-modified-p modified-p)
      (kill-new content)
      (when (and gweb-search-results-handler
                 (fboundp gweb-search-results-handler))
        (funcall gweb-search-results-handler results))
      (message "%s %s" title content)))))

;;}}}
(provide 'gweb)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
