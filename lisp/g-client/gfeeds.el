;;; gfeeds.el --- Google Access To Feeds
;;;$Id$
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
  "Google Feeds"
  :group 'g)

;;}}}
;;{{{ Variables

(defvar gfeeds-base-url
  "http://ajax.googleapis.com/ajax/services/feed/%s?q=%%s&num=10&v=1.0"
  "Base URL for Feed service.")

(defvar gfeeds-feeds-url
  (format gfeeds-base-url "load")
  "URL template for pulling feeds.")

(defvar gfeeds-lookup-url
  (format gfeeds-base-url "lookup")
  "Rest end-point for feed lookup.")

(defvar gfeeds-find-url
  (format gfeeds-base-url "find")
  "Rest end-point for finding feeds.")

(defcustom gfeeds-referer nil
  "Referer URL to send to the API.
Customize this to point to your Web location."
  :type 'string
  :group 'gfeeds)

;;}}}
;;{{{ gfeed Helpers

;;;###autoload
(defsubst gfeeds-feed (feed-url)
  "Return feed structure."
  (declare (special gfeeds-feeds-url gfeeds-referer))
  (let ((result nil))
    (g-using-scratch
     (call-process g-curl-program nil t nil
                   "-s"
                   "-e" gfeeds-referer
                   (format gfeeds-feeds-url (g-url-encode feed-url)))
     (goto-char (point-min))
     (setq result (json-read))
     (when (= 200 (g-json-get 'responseStatus result))
       (g-json-get
	'feed
	(g-json-get 'responseData result))))))

;;;###autoload
(defsubst gfeeds-lookup (url)
  "Lookup feed for a given Web page."
  (declare (special gfeeds-lookup-url gfeeds-referer))
  (let ((result nil))
    (g-using-scratch
     (call-process g-curl-program nil t nil
                   "-s"
                   "-e" gfeeds-referer
                   (format gfeeds-lookup-url (g-url-encode url)))
     (goto-char (point-min))
     (setq result (json-read))
     (when (= 200 (g-json-get 'responseStatus result))
       (g-json-get
        'url
(g-json-get 'responseData result))))))

;;;###autoload
(defsubst gfeeds-find (query)
  "Find feeds matching a query."
  (declare (special gfeeds-find-url gfeeds-referer))
  (let ((result nil))
    (g-using-scratch
     (call-process g-curl-program nil t nil
                   "-s"
                   "-e" gfeeds-referer
                   (format gfeeds-find-url (g-url-encode query)))
     (goto-char (point-min))
     (setq result (json-read))
     (when (= 200 (g-json-get 'responseStatus result))
       (g-json-get
	'entries
	(g-json-get 'responseData result))))))

;;; Feed slot accessors:

(loop for slot in
      (list 'entries 'type 'description 'author 'link 'title)
      do
      (eval
       `(defsubst ,(intern (format "gfeeds-feed-%s" slot)) (f)
          ,(format "Return %s from feed." slot)
          (cdr (assq ',slot f)))))

;;}}}
;;{{{ Convenience commands:
(defvar gfeeds-freshness-internal nil
  "Internal cached value of freshness as a time value.")

;;;###autoload
(defcustom gfeeds-freshness "1 hour"
  "Freshness used to decide if we return titles."
  :type  'string
  :set  #'(lambda (sym val)
           (declare (special gfeeds-freshness-internal))
           (setq gfeeds-freshness-internal
                 (seconds-to-time(timer-duration val)))
           (set-default sym val))
  :group 'gfeeds)

;;;###autoload
(defun gfeeds-titles (feed-url)
  "Return list of titles from feed at feed-url."
  (declare (special gfeeds-freshness-internal))
  (let ((feed (gfeeds-feed feed-url)))
    (when feed
      (cond
       (gfeeds-freshness-internal
      (delq nil
	    (mapcar
	     #'(lambda (article)
		 (let ((since (time-since  (cdr (assq 'publishedDate article))))
		       (title (cdr (assq 'title article)))
		       (link (cdr (assq 'link article))))
		   (when (and (time-less-p  since gfeeds-freshness-internal)
                              (> (length title) 0))
		     (put-text-property 0 (1- (length title))
					'link link title)
		     title)))
	     (gfeeds-feed-entries feed))))
      (t (gfeeds-feed-entries feed))))))

(defun gfeeds-html (feed-url)
  "Return a simplified HTML view."
  (let ((feed (gfeeds-feed feed-url)))
  (concat
   (format "<html><title>%s</title><ol>"
           (gfeeds-feed-title  feed))
  (mapconcat 
   #'(lambda (a)
       (format "<li><a href='%s'>%s</a>\n%s</li>"
               (cdr (assq 'link a))
               (cdr (assq 'title a))
               (cdr (assq 'contentSnippet a))))
   (gfeeds-feed-entries feed)
   "")
  "</ol></html>")
))

;;;###autoload
(defun gfeeds-view (url &optional lookup)
  "Display Feed in a browser.
Interactive prefix arg causes the feed url to be looked up given a Web site."
  (interactive
   (list
    (read-from-minibuffer "URL: "
                          (browse-url-url-at-point))
    current-prefix-arg))
  (let* ((feed-url (if lookup (gfeeds-lookup url) url))
         (html (when feed-url (gfeeds-html feed-url))))
    (cond
     ((null html)
      (message "No feed found."))
     (t 
      (g-using-scratch
       (insert html)
       (browse-url-of-buffer))))))

;;;###autoload
(defun gfeeds-lookup-and-view (site)
  "Lookup feed URL for a site and browse result."
  (interactive
   (list
    (read-from-minibuffer "Site: " (browse-url-url-at-point))))
  (gfeeds-view site 'lookup))
  
;;}}}
(provide 'gfeeds)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
;;; gfeeds.el --- Google Feeds
;;;$Id$
;;; $Author: raman $
;;; Description:  AJAX Feeds -> Lisp
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

;;; Provide Google Feed services --- such as 
;;; For use from within Emacs tools.

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'g-utils)

;;}}}
;;{{{ Customizations

(defgroup gfeeds nil
  "Google Feeds"
  :group 'g)

;;}}}

(provide 'gfeeds)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
