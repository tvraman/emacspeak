;;; emacspeak-webutils.el --- Common Web Utilities For Emacspeak
;;; $Id$
;;; $Author$
;;; Description:  Emacspeak Webutils
;;; Keywords: Emacspeak, web 
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision$ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1999 T. V. Raman <raman@cs.cornell.edu>
;;; All Rights Reserved.
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;;{{{  Introduction:

;;; This module provides common Web utilities for emacspeak.
;;; This is to avoid duplication of code between emacspeak-w3.el
;;;and emacspeak-w3m.el

;;}}}
;;{{{ required modules

;;; Code:
(require 'emacspeak-preamble)
(require 'url)
(require 'emacspeak-websearch)

;;}}}
;;{{{ variables
(defvar emacspeak-webutils-document-title nil
  "Function variable returning the current document title.")

(defvar emacspeak-webutils-url-at-point nil
  "Function variable returning the value of the url under point.")

(defvar emacspeak-webutils-current-url nil
  "Function variable returning the value of the current document url.")

(make-variable-buffer-local 'emacspeak-webutils-document-title)
(make-variable-buffer-local 'emacspeak-webutils-url-at-point)
(make-variable-buffer-local 'emacspeak-webutils-current-url)
;;}}}
(defun emacspeak-webutils-browser-check ()
  "Check to see if functions are called from a browser buffer"
  (declare (special major-mode
		    w3-mode
		    w3m-mode))
  (unless (or (eq major-mode 'w3-mode)
	      (eq major-mode 'w3m-mode))
    (error "This command cannot be used outside browser buffers.")))


;;}}}
;;{{{  google tools

;;;###autoload
(defun emacspeak-webutils-google-who-links-to-this-page ()
  "Perform a google search to locate documents that link to the
current page."
  (interactive)
  (emacspeak-webutils-browser-check)
  (emacspeak-websearch-google
   (format "link:%s"
	   (funcall emacspeak-webutils-current-url))))

;;;###autoload
(defun emacspeak-webutils-google-extract-from-cache (&optional prefix)
  "Extract current  page from the Google cache.
With a prefix argument, extracts url under point."
  (interactive "P")
  (emacspeak-webutils-browser-check)
  (emacspeak-websearch-google
   (format "cache:%s"
	   (cond
	    ((null prefix)
	     (funcall emacspeak-webutils-current-url))
	    (t
	     (funcall emacspeak-webutils-url-at-point))))))

;;;###autoload
(defun emacspeak-webutils-google-on-this-site ()
  "Perform a google search restricted to the current WWW site."
  (interactive)
  (emacspeak-webutils-browser-check)
  (emacspeak-websearch-google
   (format "site:%s %s"
           (aref
            (url-generic-parse-url (funcall emacspeak-webutils-current-url))
            3)
           (read-from-minibuffer "Search this site for: "))))

(defvar emacspeak-webutils-google-related-uri
  "http://www.google.com/search?hl=en&num=25&q=related:")

;;;###autoload
(defun emacspeak-webutils-google-similar-to-this-page (url)
  "Ask Google to find documents similar to this one."
  (interactive
   (list
    (read-from-minibuffer "URL:"
			  (funcall emacspeak-webutils-current-url))))
  (declare (special emacspeak-w3-google-related-uri))
  (browse-url
   (format
    "%s%s"
    emacspeak-webutils-google-related-uri
    url))
  (emacspeak-websearch-post-process "Similar"
                                    'emacspeak-speak-line))

;;;###autoload
(defun emacspeak-webutils-transcode-via-google (&optional untranscode)
  "Transcode URL under point via Google.
 Reverse effect with prefix arg for links on a transcoded page."
  (interactive "P")
  (emacspeak-webutils-browser-check)
  (unless (funcall emacspeak-webutils-url-at-point)
    (error "Not on a link."))
  (let ((url-mime-encoding-string "gzip"))
    (cond
     ((null untranscode)
      (browse-url
       (format "http://www.google.com/gwt/n?_gwt_noimg=1&u=%s"
	       (emacspeak-url-encode
		(funcall emacspeak-webutils-url-at-point)))))
     (t
      (let ((plain-url nil)
	    (prefix "http://www.google.com/gwt/n?u=")
	    (suffix "&_gwt_noimg=1")
	    (unhex (url-unhex-string (funcall emacspeak-webutils-url-at-point))))
	(setq plain-url (substring  unhex (length prefix) (- 0 (length suffix))))
	(when plain-url
	  (browse-url plain-url)))))))

;;;###autoload 
(defun emacspeak-webutils-transcode-current-url-via-google (&optional untranscode)
  "Transcode current URL via Google.
  Reverse effect with prefix arg."
  (interactive "P")
  (emacspeak-webutils-browser-check)
;;  (let ((url-mime-encoding-string "gzip"))
;; removing the above line makes the untranscode work
    (cond
     ((null untranscode)
      (browse-url
       (format "http://www.google.com/gwt/n?_gwt_noimg=1&u=%s"
	       (emacspeak-url-encode (funcall emacspeak-webutils-current-url)))))
     (t
      (let ((plain-url nil)
	    (prefix "http://www.google.com/gwt/n?_gwt_noimg=1&u=")
	    (unhex (url-unhex-string (funcall emacspeak-webutils-current-url))))
	(setq plain-url (substring  unhex (length prefix)))
	(when plain-url
	  (browse-url plain-url))))))

;;}}}
;;{{{ 

;;;###autoload
(defun emacspeak-webutils-jump-to-title-in-content ()
  "Jumps to the title in web document.
The first time it is called, it jumps to the first 
instance  of the title.  Repeated calls jump to further 
instances."
  (interactive)
  (let ((title (funcall emacspeak-webutils-document-title)))
    (condition-case nil
        (progn
          (if (not (eq last-command 'emacspeak-webutils-jump-to-title-in-content))
              (goto-char (point-min)))
          (goto-char
           (search-forward
            (substring title 0 (min 10 (length title)))))
          (emacspeak-speak-line)
          (emacspeak-auditory-icon 'large-movement))
      (error "Title not found in body."))))

;;;###autoload
(defun emacspeak-webutils-play-media-at-point ()
  "Play media url under point "
  (interactive )
  (declare (special emacspeak-media-player))
  (let ((url (funcall emacspeak-webutils-url-at-point)))
    (message "Playing media  URL under point")
    (funcall emacspeak-media-player  url)))

;;}}}

(provide 'emacspeak-webutils)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
