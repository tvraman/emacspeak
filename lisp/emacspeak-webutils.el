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

;;{{{  Introduction:

;;; Commentary:
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
(require 'browse-url)
;;}}}
;;{{{ keymap: web-prefix
(define-prefix-command 'emacspeak-web-prefix)

(declaim (special emacspeak-web-prefix))
(loop for k in
      '(
        ("b" browse-url-of-buffer)
        ("r" browse-url-of-region)
        ("R" emacspeak-xslt-view-region)
        )
      do
      (emacspeak-keymap-update  emacspeak-web-prefix k))

;;}}}
;;{{{ Helpers:

;;;###autoload
(defcustom emacspeak-webutils-charent-alist
  '(("&lt;" . "<")
    ("&gt;" . ">")
    ("&quot;" . "\"")
    ("&apos;" . "'")
    ("&amp;" . "&"))
  "Entities to unescape when treating badly escaped XML."
  :type '(repeat  :tag "Char Entity"
                  (cons :tag "Entry"
                        (string :tag "CharEnt")
                        (string :tag "Replacement")))
  :group 'emacspeak-webutils)

(defsubst emacspeak-webutils-unescape-charent (start end)
  "Clean up charents in XML."
  (declare (special emacspeak-webutils-charent-alist))
  (loop for entry in emacspeak-webutils-charent-alist
        do
        (let ((entity (car  entry))
              (replacement (cdr entry )))
          (goto-char start)
          (while (search-forward entity end t)
            (replace-match replacement )))))

(defsubst emacspeak-webutils-supported-p ()
  "Check if this is a supported browser."
  (or   (eq browse-url-browser-function 'w3-fetch)
        (eq browse-url-browser-function 'browse-url-w3)
        (eq browse-url-browser-function 'w3m-browse-url)))

(defsubst emacspeak-webutils-autospeak()
  "Setup post process hook to speak the Web page when rendered."
  (add-hook 'emacspeak-w3-post-process-hook
            #'(lambda nil
                (emacspeak-speak-buffer)
                (emacspeak-auditory-icon 'open-object))))

(defsubst emacspeak-webutils-browser-check ()
  "Check to see if functions are called from a browser buffer"
  (declare (special major-mode))
  (unless (or (eq major-mode 'w3-mode)
              (eq major-mode 'w3m-mode))
    (error "This command cannot be used outside browser buffers.")))

(defsubst emacspeak-webutils-read-url ( )
  "Return URL of current page,
or URL read from minibuffer."
  (declare (special emacspeak-webutils-current-url))
  (if (functionp  emacspeak-webutils-current-url)
      (funcall emacspeak-webutils-current-url)
    (read-from-minibuffer "URL: "
                          (or (browse-url-url-at-point)
                              "http://"))))

(defsubst emacspeak-webutils-read-this-url ( )
  "Return URL under point
or URL read from minibuffer."
  (declare (special emacspeak-webutils-url-at-point))
  (if (functionp  emacspeak-webutils-url-at-point)
      (funcall emacspeak-webutils-url-at-point)
    (read-from-minibuffer "URL: "
                          (or (browse-url-url-at-point)
                              "http://"))))

;;;  Helper: rename result buffer
(defsubst emacspeak-webutils-rename-buffer (key)
  "Setup emacspeak-w3-post-process-hook  to rename result buffer"
  (add-hook
   'emacspeak-w3-post-process-hook
   (eval
    `(function
      (lambda nil
        (rename-buffer
         (format "%s %s"
                 ,key (buffer-name))
         'unique))))))

;;;###autoload
(defun emacspeak-webutils-post-process (locator speaker &rest args)
  "Set up post processing steps on a result page.
LOCATOR is a string to search for in the results page.
SPEAKER is a function to call to speak relevant information.
ARGS specifies additional arguments to SPEAKER if any."
  (declare (special emacspeak-w3-post-process-hook))
  (when (emacspeak-webutils-supported-p)
    (add-hook  'emacspeak-w3-post-process-hook
               (eval
                `(function
                  (lambda nil
                    (cond
                     ((search-forward ,locator nil t)
                      (recenter 0)
                      (apply(quote ,speaker) ,args))
                     (t (message "Your search appears to have failed.")))))))))

;;}}}
;;{{{ helper macros:

;;; tVR: moving these from emacspeak-w3 to this module.

(defmacro emacspeak-webutils-without-xsl (&rest body)
  "Execute body with XSL turned off."
  (`
   (progn
     (declare (special emacspeak-we-xsl-p))
     (when emacspeak-we-xsl-p
       (setq emacspeak-we-xsl-p nil)
       (add-hook 'emacspeak-w3-post-process-hook
                 #'(lambda ()
                     (declare (special emacspeak-we-xsl-p))
                     (setq emacspeak-we-xsl-p t))))
     (,@ body))))

(defmacro emacspeak-webutils-with-xsl (&rest body)
  "Execute body with XSL turned on."
  (`
   (progn
     (declare (special emacspeak-we-xsl-p))
     (unless emacspeak-we-xsl-p
       (setq emacspeak-we-xsl-p t)
       (add-hook 'emacspeak-w3-post-process-hook
                 #'(lambda ()
                     (declare (special emacspeak-we-xsl-p))
                     (setq emacspeak-we-xsl-p nil))))
     (,@ body))))

(defmacro emacspeak-webutils-with-xsl-environment (style params options  &rest body)
  "Execute body with XSL turned on
and xsl environment specified by style, params and options."
  `(let ((save-flag ,emacspeak-we-xsl-p)
         (save-options ,emacspeak-xslt-options)
         (save-style ,emacspeak-we-xsl-transform)
         (save-params ,emacspeak-we-xsl-params))
     (setq emacspeak-we-xsl-p t
           emacspeak-xslt-options ,options
           emacspeak-we-xsl-transform ,style
           emacspeak-we-xsl-params ,params)
     (add-hook
      'emacspeak-w3-post-process-hook
      (eval
       `(function
         (lambda ()
           (declare (special emacspeak-we-xsl-p
                             emacspeak-we-xsl-transform
                             emacspeak-xslt-options
                             emacspeak-we-xsl-params))
           (setq emacspeak-we-xsl-p ,save-flag
                 emacspeak-xslt-options ,save-options
                 emacspeak-we-xsl-transform ,save-style
                 emacspeak-we-xsl-params ,save-params)))))
     ,@body))

;;}}}
;;{{{ variables

(defvar emacspeak-webutils-document-title nil
  "Function variable returning the current document title.")

(defvar emacspeak-webutils-url-at-point nil
  "Function variable returning the value of the url under point
  in a Web page.")

(defvar emacspeak-webutils-current-url nil
  "Function variable returning the value of the current document
  url in a Web page.")

(make-variable-buffer-local 'emacspeak-webutils-document-title)
(make-variable-buffer-local 'emacspeak-webutils-url-at-point)
(make-variable-buffer-local 'emacspeak-webutils-current-url)

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
  (emacspeak-webutils-post-process "Similar"
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
     ((null feed) (error "No url under point."))
     (t (emacspeak-webutils-atom-display
         (format
          "http://www.google.com/reader/public/atom/feed/%s?n=20"
          (emacspeak-url-encode feed)))))))
;;;###autoload
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

(defun emacspeak-webutils-feed-display(feed-url style &optional speak)
  "Fetch feed via Emacs and display using xsltproc."
  (let ((buffer (url-retrieve-synchronously feed-url))
        (emacspeak-xslt-options nil))
    (when speak (emacspeak-webutils-autospeak))
    (cond
     ((null buffer)
      (message "Nothing to display."))
     (t
      (emacspeak-webutils-without-xsl
       (save-excursion
         (set-buffer buffer)
         (goto-char (point-min))
         (search-forward "\n\n")
         (delete-region (point-min) (point))
		 (decode-coding-region (point-min) (point-max) 'utf-8)
         (emacspeak-xslt-region style
                                (point-min) (point-max))
         (browse-url-of-buffer)))))))

;;;###autoload
(defun emacspeak-webutils-rss-display (feed-url )
  "Display RSS feed."
  (interactive
   (list
    (emacspeak-webutils-read-this-url)))
  (emacspeak-auditory-icon 'select-object)
  (emacspeak-webutils-autospeak)
  (emacspeak-webutils-feed-display feed-url
                                   (emacspeak-xslt-get "rss.xsl")))

;;;###autoload
(defun emacspeak-webutils-atom-display (feed-url )
  "Display ATOM feed."
  (interactive (list (emacspeak-webutils-read-this-url)))
  (declare (special emacspeak-atom-view-xsl))
  (emacspeak-auditory-icon 'select-object)
  (emacspeak-webutils-autospeak)
  (emacspeak-webutils-feed-display feed-url
                                   emacspeak-atom-view-xsl))

;;;###autoload
(defun emacspeak-webutils-fv (feed-url )
  "Display RSS or ATOM feed."
  (interactive (list (emacspeak-webutils-read-this-url)))
  (emacspeak-auditory-icon 'select-object)
  (emacspeak-webutils-autospeak)
  (emacspeak-webutils-feed-display feed-url
                                   (emacspeak-xslt-get "fv.xsl")))

;;}}}
;;{{{ RSS:
;;{{{ RSS feed cache

;;;###autoload
(defgroup emacspeak-rss nil
  "RSS Feeds for the Emacspeak desktop."
  :group 'emacspeak)

(defcustom emacspeak-rss-feeds
  '(
    ("Wired News" "http://www.wired.com/news_drop/netcenter/netcenter.rdf")
    ("BBC News"  "http://www.bbc.co.uk/syndication/feeds/news/ukfs_news/front_page/rss091.xml")
    ("CNet Tech News"  "http://rss.com.com/2547-12-0-5.xml")
    ("XML.COM"  "http://www.xml.com/xml/news.rss")
    )
  "Table of RSS feeds."
  :type '(repeat
          (list :tag "RSS Feed"
                (string :tag "Title")
                (string :tag "URI")))
  :group 'emacspeak-rss)

;;}}}
;;{{{  view feed
(defcustom emacspeak-rss-unescape-html t
  "Fix malformed  XML that results from sites attempting to
unescape HTML tags."
  :type 'boolean
  :group 'emacspeak-rss)

;;;###autoload

;;;###autoload
(defun emacspeak-opml-display (opml-url &optional speak)
  "Retrieve and display OPML  URL."
  (interactive
   (list
    (car (browse-url-interactive-arg "OPML  URL: "))
    (or (interactive-p)
        current-prefix-arg)))
  (emacspeak-webutils-feed-display
   (emacspeak-xslt-get "opml.xsl")
   opml-url
   speak))

;;;###autoload
(defun emacspeak-rss-browse (feed)
  "Browse specified RSS feed."
  (interactive
   (list
    (let ((completion-ignore-case t))
      (completing-read "Feed:"
                       emacspeak-rss-feeds))))
  (let ((uri (cadr
              (assoc feed emacspeak-rss-feeds))))
    (emacspeak-webutils-rss-display uri )))

;;}}}
;;}}}
;;{{{ ATOM:
;;{{{ ATOM feed cache

;;;###autoload
(defgroup emacspeak-atom nil
  "ATOM Feeds for the Emacspeak desktop."
  :group 'emacspeak)

;;;###autoload
(defcustom emacspeak-atom-feeds
  nil
  "Table of ATOM feeds."
  :type '(repeat
          (list :tag "ATOM Feed"
                (string :tag "Title")
                (string :tag "URI")))
  :group 'emacspeak-atom)

;;}}}
;;{{{  view feed

(defvar emacspeak-atom-legacy
  (expand-file-name "legacy-atom.xsl" emacspeak-xslt-directory)
  "Legacy Atom support.")

(defvar emacspeak-atom-modern
  (expand-file-name "atom-view.xsl" emacspeak-xslt-directory)
  "Modern Atom support.")

(defcustom emacspeak-atom-view-xsl
  emacspeak-atom-legacy
  "XSL stylesheet used for viewing Atom Feeds."
  :type '(choice
          (string :tag "Legacy"  emacspeak-atom-legacy)
          (string :tag "Modern" emacspeak-atom-modern))
  :group 'emacspeak-xsl)

;;;###autoload

;;;###autoload
(defun emacspeak-atom-browse (feed)
  "Browse specified ATOM feed."
  (interactive
   (list
    (let ((completion-ignore-case t))
      (completing-read "Feed:"
                       emacspeak-atom-feeds))))
  (let ((uri (cadr (assoc feed emacspeak-atom-feeds))))
    (emacspeak-webutils-atom-display uri)))

;;}}}

;;}}}
(provide 'emacspeak-webutils)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
