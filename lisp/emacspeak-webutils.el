;;; emacspeak-webutils.el --- Common Web Utilities For Emacspeak
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak Webutils
;;; Keywords: Emacspeak, web
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-06-12 06:16:25 -0700 (Tue, 12 Jun 2007) $ |
;;;  $Revision: 4634 $ |
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
(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'backquote)
(require 'emacspeak-preamble)
(require 'url)
(require 'emacspeak-xslt)
(require 'emacspeak-websearch)

;;}}}
;;{{{ helper macros:

;;; tVR: moving these from emacspeak-w3 to this module.

(defmacro emacspeak-webutils-without-xsl (&rest body)
  "Execute body with XSL turned off."
  (`
   (progn
     (declare (special emacspeak-w3-xsl-p))
     (when emacspeak-w3-xsl-p
       (setq emacspeak-w3-xsl-p nil)
       (add-hook 'emacspeak-w3-post-process-hook
                 #'(lambda ()
                     (declare (special emacspeak-w3-xsl-p))
                     (setq emacspeak-w3-xsl-p t))))
     (,@ body))))

(defmacro emacspeak-webutils-with-xsl (&rest body)
  "Execute body with XSL turned on."
  (`
   (progn
     (declare (special emacspeak-w3-xsl-p))
     (unless emacspeak-w3-xsl-p
       (setq emacspeak-w3-xsl-p t)
       (add-hook 'emacspeak-w3-post-process-hook
                 #'(lambda ()
                     (declare (special emacspeak-w3-xsl-p))
                     (setq emacspeak-w3-xsl-p nil))))
     (,@ body))))

(defmacro emacspeak-webutils-with-xsl-environment (style params &rest body)
  "Execute body with XSL turned on
and xsl environment specified by style and params."
  `(let ((save-flag ,emacspeak-w3-xsl-p)
         (save-style ,emacspeak-w3-xsl-transform)
         (save-params ,emacspeak-w3-xsl-params))
     (setq emacspeak-w3-xsl-p t
           emacspeak-w3-xsl-transform ,style
           emacspeak-w3-xsl-params ,params)
     (add-hook
      'emacspeak-w3-post-process-hook
      (eval
       `(function
         (lambda ()
           (declare (special emacspeak-w3-xsl-p
                             emacspeak-w3-xsl-transform
                             emacspeak-w3-xsl-params))
           (setq emacspeak-w3-xsl-p ,save-flag
                 emacspeak-w3-xsl-transform ,save-style
                 emacspeak-w3-xsl-params ,save-params)))))
     ,@body))

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
;;{{{ utils

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
(defvar emacspeak-webutils-google-transcoder-url
  "http://www.google.com/gwt/n?_gwt_noimg=1&output=xhtml&u=%s"
  "URL pattern for accessing Google transcoder.")


(defsubst emacspeak-webutils-transcoded-to-plain-url (url)
  "Extract plain URL from Google transcoder URL."
  (let ((prefix (substring emacspeak-webutils-google-transcoder-url 0
						   (1+ (position ?? emacspeak-webutils-google-transcoder-url)))))
	(when (equal prefix (substring url 0 (length prefix)))
		  (let* ((args (substring url (length prefix)))
				 (arg-alist (url-parse-args (subst-char-in-string ?& ?\; args))))
			(url-unhex-string (cdr (assoc "u" arg-alist)))))))

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
       (format emacspeak-webutils-google-transcoder-url
               (emacspeak-url-encode
                (funcall emacspeak-webutils-url-at-point)))))
     (t
      (let ((plain-url (emacspeak-webutils-transcoded-to-plain-url (funcall emacspeak-webutils-url-at-point))))
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
     (format emacspeak-webutils-google-transcoder-url
             (emacspeak-url-encode (funcall emacspeak-webutils-current-url)))))
   (t
    (let ((plain-url (emacspeak-webutils-transcoded-to-plain-url (funcall emacspeak-webutils-current-url))))
	  (when plain-url
        (browse-url plain-url))))))

;;}}}
;;{{{ tools

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

;;;###autoload
(defun emacspeak-webutils-view-feed-via-google-reader ()
  "Pulls feed under point via Google Reader."
  (interactive)
  (let ((feed (funcall emacspeak-webutils-url-at-point)))
    (cond
     ((null feed)
      (error "No url under point."))
     (t (emacspeak-atom-display
         (format
          "http://www.google.com/reader/public/atom/feed/%s?n=100"
          (emacspeak-url-encode feed))
         'speak)))))

(defun emacspeak-webutils-open-in-other-browser ()
  "Opens link in alternate browser.
 If using default browser is w3, uses w3m and vice-versa"
  (interactive)
  (declare (special major-mode
                    w3-mode
                    w3m-mode))
  (emacspeak-webutils-browser-check)
  (if (eq major-mode 'w3-mode)
      (w3m-browse-url  (funcall emacspeak-webutils-url-at-point))
    (browse-url-w3 (funcall emacspeak-webutils-url-at-point))))

;;}}}
;;{{{ display authenticated feeds:

;;; these commands use url to pull ATOM/RSS feeds
;;; before handing it off to xsltproc for conversion to xhtml

(defun emacspeak-webutils-feed-display(feed-url style)
  "Fetch feed via Emacs and display using xsltproc."
  (let ((buffer (url-retrieve-synchronously feed-url)))
    (cond
     ((null buffer)
      (message "Nothing to display."))
     (t
      (save-excursion
        (set-buffer buffer)
        (goto-char (point-min))
        (search-forward "\n\n")
        (delete-region (point-min) (point))
        (shell-command-on-region
         (point-min)
         (point-max)
         (format "%s %s -"
                 emacspeak-xslt-program style)
         'replace)
        (browse-url-of-buffer))))))

;;;###autoload
(defun emacspeak-webutils-rss-display (feed-url)
  "Display RSS feed."
  (interactive
   (list
    (read-from-minibuffer "Feed: "
                          (if emacspeak-webutils-url-at-point
                          (funcall
                           emacspeak-webutils-url-at-point)
                          (browse-url-url-at-point)))))
(emacspeak-webutils-feed-display feed-url
                           (expand-file-name "rss.xsl"
                                                 emacspeak-xslt-directory)))

;;;###autoload
(defun emacspeak-webutils-atom-display (feed-url)
  "Display ATOM feed."
  (interactive
   (list
    (read-from-minibuffer "Feed: "
                          (if emacspeak-webutils-url-at-point
                              (funcall
                               emacspeak-webutils-url-at-point)
                            (browse-url-url-at-point)))))
  (declare (special emacspeak-atom-view-xsl))
  (emacspeak-webutils-feed-display feed-url emacspeak-atom-view-xsl))

;;}}}
(provide 'emacspeak-webutils)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
