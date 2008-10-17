;;; gweb.el --- Google Search
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
;;; Commentary:
;;{{{  introduction

;;; Provide Google services --- such as search, search-based completion etc.
;;; For use from within Emacs tools.
;;; This is meant to be fast and efficient --- and uses WebAPIs as opposed to HTML  scraping.

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
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

(defvar gweb-suggest-url
  "http://www.google.com/complete/search?json=true&qu=%s"
  "URL  that gets suggestions from Google as JSON.")

(defsubst gweb-suggest (input)
  "Get completion list from Google Suggest."
  (declare (special gweb-suggest-url))
  (unless (and (stringp input)
	     (> (length input) 0))
    (setq input minibuffer-default))
  (g-using-scratch
   (call-process g-curl-program nil t nil
                 "-s"
                 (format gweb-suggest-url (g-url-encode input)))
   (goto-char (point-min))
   ;; A JSON array is a vector.
   ;; read it, filter the comma separators found as symbols.
   (delq'\,
    (append                             ; vector->list
     (aref (read (current-buffer)) 2)
     nil))))

(defun gweb-suggest-completer (string predicate mode)
  "Generate completions using Google Suggest. "
  (save-current-buffer 
    (set-buffer 
     (let ((window (minibuffer-selected-window))) 
       (if (window-live-p window) 
	   (window-buffer window) 
	 (current-buffer)))) 
    (complete-with-action mode 
			  (gweb-suggest string)
			  string predicate)))
  

(defvar gweb-history nil
  "History of Google Search queries.")

(put 'gweb-history 'history-length 100)


(defun gweb-lazy-suggest (input)
  "Used to generate completions lazily."
  (lexical-let ((input input)
                table)
    (setq table (lazy-completion-table
                 table (lambda () (gweb-suggest input))))
    table))
(if (fboundp 'complete-with-action)
    (defsubst gweb-google-autocomplete (&optional prompt)
  "Read user input using Google Suggest for auto-completion."
  (let* ((minibuffer-completing-file-name t) ;; accept spaces
         (completion-ignore-case t)
         (word (thing-at-point 'word))
         (suggestions
	  (when (and word (> (length word) 0))
	    (set-text-properties 0 (length word) nil word)
	    (cons  word (gweb-suggest  word))))
         (query nil))
    (setq query
          (completing-read
           (or prompt "Google: ")
           'gweb-suggest-completer
           nil nil
           word 'gweb-history
           suggestions))
    (pushnew  query gweb-history)
    (g-url-encode query)))
;;; Emacs 22
(defsubst gweb-google-autocomplete (&optional prompt)
  "Read user input using Google Suggest for auto-completion."
  (let ((minibuffer-completing-file-name t) ;; so we can type
        ;; spaces
        (completion-ignore-case t))
    (g-url-encode
    (completing-read
     (or prompt "Google: ")
                     (dynamic-completion-table gweb-suggest))))))

;;}}}
;;{{{ Search Helpers

(defsubst gweb-results (query url-end-point)
  "Return results list obtained from url-end-point."
  (declare (special  gweb-referer))
  (let((response nil))
      (g-using-scratch
	(call-process g-curl-program nil t nil
		      "-s"
		      "-e" gweb-referer
		      (format url-end-point  query))
	(goto-char (point-min))
	(setq response (json-read))
	(when (= 200 (g-json-get 'responseStatus response))
	  (g-json-get
	   'results
	   (g-json-get 'responseData response))))))

(defsubst gweb-web-results (query)
  "Return Web Search results list."
  (declare (special gweb-web-url ))
  (gweb-results query gweb-web-url))

;;}}}
;;{{{ News Helpers:

;;; Google News Search
(defsubst gweb-news-results (query)
  "Return News Search results."
  (declare (special gweb-news-url))
(gweb-results (g-url-encode query) gweb-news-url))


(defun gweb-news-html (query)
  "Return simple HTML from News search."
  (let ((results (gweb-news-results query )))
    (when results
      (concat
       (format "<html><title>News Results For %s</title><ol>" query)
       (mapconcat 
        #'(lambda (a)
            (format "<li><a href='%s'>%s</a>\n%s
<a href='%s'>Related Stories</a></li>"
                    (cdr (assq 'unescapedUrl a))
                    (cdr (assq 'title a))
                    (cdr (assq 'content a))
                    (cdr (assq 'clusterUrl a))))
        results
        "")
       "</ol></html>"))))
    
(defun gweb-news-view (query )
  "Display News Search results  in a browser."
  (interactive "sNews Search: ")
  (let ((html (gweb-news-html query)))
    (cond
     ((null html)
      (message "No news found."))
     (t 
      (g-using-scratch
       (insert html)
       (browse-url-of-buffer))))))
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
           (lucky (aref results 0))
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
;;; byte-compile-dynamic: t
;;; end:

;;}}}
