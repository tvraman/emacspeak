;;; gfeeds.el --- Google Access To Feeds
;;;$Id: gcal.el,v 1.30 2006/09/28 17:47:44 raman Exp $
;;; $Author: raman $
;;; Description:  AJAX Feeds -> Lisp
;;; Keywords: Google   AJAX Feeds API
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

;;; Fast feed access from Google for use in Emacs

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'g-utils)

;;}}}
;;{{{ Customizations

(defgroup gfeeds nil
  "Google search"
  :group 'g)

;;}}}
;;{{{ Variables

(defvar gfeeds-feeds-url
"http://ajax.googleapis.com/ajax/services/feed/load?q=%s&v=1.0"
  "URL template for pulling feeds.")

(defvar gfeeds-referer "http://emacspeak.sf.net"
  "Referer URL to send to the API.")

;;}}}
;;{{{ gfeed Helpers

(defsubst gfeeds-feed (feed-url)
  "Return feed structure."
  (declare (special gfeeds-feeds-url gfeeds-referer))
  (let ((json-key-type 'string)
        (result nil))
    (g-using-scratch
     (call-process g-curl-program nil t nil
                   "-s"
                   "-e" gfeeds-referer
                   (format gfeeds-feeds-url (g-url-encode feed-url)))
     (goto-char (point-min))
     (setq result (json-read))
     (when (= 200 (g-json-get "responseStatus" result))
       (g-json-lookup "responseData.feed" result)))))

;;; Feed slot accessors:

(loop for slot in
      '("entries" "type" "description" "author" "link" "title")
      do
      (eval
       `(defsubst ,(intern (format "gfeeds-feed-%s" slot)) (f)
          ,(format "Return %s from feed." slot)
          (cdr (assoc ,slot f)))))

;;}}}

(provide 'gfeeds)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
;;; gfeeds.el --- Google Search
;;;$Id: gcal.el,v 1.30 2006/09/28 17:47:44 raman Exp $
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

(defgroup gfeeds nil
  "Google search"
  :group 'g)

;;}}}
;;{{{ Variables

(defvar gfeeds-search-url
"http://ajax.googleapis.com/ajax/services/search/web?v=1.0&q=%s"
  "URL template for Websearch command.")

(defvar gfeeds-referer "http://emacspeak.sf.net"
  "Referer URL to send to the API.")

;;}}}
;;{{{ Search Helpers

(defsubst gfeeds-results (query)
  "Return results list."
  (declare (special gfeeds-search-url gfeeds-referer))
  (let ((result nil)
        (json-key-type 'string))
    (g-using-scratch
     (call-process g-curl-program nil t nil
                   "-s"
                   "-e"
                   gfeeds-referer
                   (format gfeeds-search-url (g-url-encode query)))
     (goto-char (point-min))
     (setq result
           (g-json-lookup "responseData.results" (json-read))))
    result))

;;}}}
;;{{{ google suggest helper:

;;; Get search completions from Google

(defvar gfeeds-suggest-url
  "http://www.google.com/complete/search?json=true&qu=%s"
  "URL  that gets suggestions from Google as JSON.")

(defsubst gfeeds-suggest (input)
  "Get completion list from Google Suggest."
  (declare (special gfeeds-suggest-url))
  (unless (and (stringp input)
	     (> (length input) 0))
    (setq input minibuffer-default))
  (g-using-scratch
   (call-process g-curl-program nil t nil
                 "-s"
                 (format gfeeds-suggest-url (g-url-encode input)))
   (goto-char (point-min))
   ;; A JSON array is a vector.
   ;; read it, filter the comma separators found as symbols.
   (delq'\,
    (append                             ; vector->list
     (aref (read (current-buffer)) 2)
     nil))))

(defun gfeeds-suggest-completer (string predicate mode)
  "Generate completions using Google Suggest. "
  (save-current-buffer 
    (set-buffer 
     (let ((window (minibuffer-selected-window))) 
       (if (window-live-p window) 
           (window-buffer window) 
         (current-buffer)))) 
    (complete-with-action mode 
                          (gfeeds-suggest string) 
                          string predicate)))

(defvar gfeeds-history nil
  "History of Google Search queries.")

(put 'gfeeds-history 'history-length 100)


(defun gfeeds-lazy-suggest (input)
  "Used to generate completions lazily."
  (lexical-let ((input input)
                table)
    (setq table (lazy-completion-table
                 table (lambda () (gfeeds-suggest input))))
    table))

(defsubst gfeeds-google-autocomplete (&optional prompt)
  "Read user input using Google Suggest for auto-completion."
  (let* ((minibuffer-completing-file-name t) ;; accept spaces
         (completion-ignore-case t)
         (word (thing-at-point 'word))
         (suggestions
	  (when (and word (> (length word) 0))
	    (set-text-properties 0 (length word) nil word)
	    (cons  word (gfeeds-suggest  word))))
         (query nil))
    (setq query
          (completing-read
           (or prompt "Google: ")
           'gfeeds-suggest-completer
           nil nil nil
           'gfeeds-history
           suggestions))
    (pushnew  query gfeeds-history)
    (g-url-encode query)))

;;}}}
;;{{{ Interactive Commands:

;;; Need to be smarter about guessing default term 
;;; thing-at-point can return an empty string,
;;; and this is not a good thing for Google Suggest which will error out.

;;;###autoload
(defun gfeeds-google-at-point (search-term &optional refresh)
  "Google for term at point, and display top result succinctly.
Attach URL at point so we can follow it later --- subsequent invocations of this command simply follow that URL.
Optional interactive prefix arg refresh forces this cached URL to be refreshed."
  (interactive
   (list
    (unless(and (not current-prefix-arg)
                (get-text-property (point) 'lucky-url))
      (gfeeds-google-autocomplete))
    current-prefix-arg))
  (cond
   ((and (not refresh)
         (get-text-property (point) 'lucky-url))
    (browse-url (get-text-property (point) 'lucky-url)))
   (t 
    (let* ((lucky (aref (gfeeds-results  search-term) 0))
         (inhibit-read-only t)
         (bounds (bounds-of-thing-at-point 'word))
         (modified-p (buffer-modified-p))
         (title (g-json-get "titleNoFormatting" lucky))
         (url (g-json-get "url" lucky))
         (content (shell-command-to-string
		(format
		 "echo '%s' | lynx -dump -stdin 2>/dev/null"
		 (g-json-get "content" lucky)))))
      (when bounds 
        (add-text-properties   (car bounds) (cdr bounds)
                               (list 'lucky-url url
                                     'face 'highlight)))
      (pushnew lucky minibuffer-history)
      (set-buffer-modified-p modified-p)
      (kill-new content)
      (message "%s %s" title content)))))

;;}}}

(provide 'gfeeds)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
