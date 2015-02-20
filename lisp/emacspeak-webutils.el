;;; emacspeak-webutils.el --- Common Web Utilities For Emacspeak
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak Webutils
;;; Keywords: Emacspeak, web
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2008-08-14 11:23:31 -0700 (Thu, 14 Aug 2008) $ |
;;;  $Revision: 4634 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1999, 2011 T. V. Raman <raman@cs.cornell.edu>
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
(require 'emacspeak-preamble)
(require 'url)
(require 'gfeeds)
(require 'browse-url)
;;}}}
;;{{{ Utility: Render HTML To String
;;;###autoload
(defsubst emacspeak-webutils-html-string (html-string)
  "Return formatted string."
  (or (require 'shr) (error "Need  emacs 24.4"))
  (with-temp-buffer 
    (insert html-string)
    (shr-render-region  (point-min) (point-max))
    (buffer-string)))

;;}}}
;;{{{ Fix bug in url-cookie

(defadvice url-cookie-write-file (around fix-write-bug pre act comp)
  "Fix bug in url-cookie-write-file."
  (let ((print-length nil)
        (print-level nil))
    ad-do-it))

;;}}}
;;{{{ keymap: web-prefix
;;;###autoload
(define-prefix-command 'emacspeak-web-prefix)

(declaim (special emacspeak-web-prefix))
(loop for k in
      '(
        ("R" emacspeak-xslt-view-region)
        ("b" browse-url-of-buffer)
        ("m" emacspeak-wizards-eww-buffer-list)
        ("r" browse-url-of-region)
        )
      do
      (emacspeak-keymap-update  emacspeak-web-prefix k))

;;}}}
;;{{{ web-post-process

;;;###autoload
(defvar emacspeak-web-post-process-hook nil
  "Set locally to a  site specific post processor.
Note that the Web browser should reset this hook after using it.")
(defsubst emacspeak-webutils-run-post-process-hook (&rest ignore)
  "Use web post process hook."
  (declare (special emacspeak-web-post-process-hook))
  (when     emacspeak-web-post-process-hook
    (condition-case nil
        (let ((inhibit-read-only t))
          (run-hooks  'emacspeak-web-post-process-hook))
      ((debug error)  (message "Caught error  in post-process hook.")
       (setq emacspeak-web-post-process-hook nil)))
    (setq emacspeak-web-post-process-hook nil)))

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
  "Setup post process hook to speak the Web page when rendered.
Forward punctuation and rate  settings to resulting buffer."
  (lexical-let
      ((p dtk-punctuation-mode)
       (r dtk-speech-rate))
    (add-hook 'emacspeak-web-post-process-hook
              #'(lambda nil
                  (declare (special emacspeak-we-xpath-filter))
                  (let ((inhibit-read-only t))
                    (dtk-set-punctuations p)
                    (dtk-set-rate r)
                    (emacspeak-dtk-sync)
                    (setq emacspeak-we-xpath-filter
                          emacspeak-we-paragraphs-xpath-filter)
                    (emacspeak-speak-buffer)))
              'at-end)))

(defsubst emacspeak-webutils-cache-google-query(query)
  "Setup post process hook to cache google query when rendered."
  (declare (special emacspeak-google-query))
  (let ((cache
         (eval `(function
                 (lambda nil
                   (setq emacspeak-google-query ,query))))))
    (add-hook 'emacspeak-web-post-process-hook cache 'at-end)))

(defsubst emacspeak-webutils-cache-google-toolbelt(belt)
  "Setup post process hook to cache google toolbelt when rendered."
  (declare (special emacspeak-google-toolbelt))
  (let ((cache
         (eval `(function
                 (lambda nil
                   (setq emacspeak-google-toolbelt' ,belt))))))
    (add-hook 'emacspeak-web-post-process-hook cache 'at-end)))

(defsubst emacspeak-webutils-browser-check ()
  "Check to see if functions are called from a browser buffer"
  (declare (special major-mode))
  (unless (or (eq major-mode 'w3-mode)
              (eq major-mode 'w3m-mode)
              (eq major-mode 'eww-mode))
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
    (car (browse-url-interactive-arg "URL: "))))

;;;  Helper: rename result buffer
(defsubst emacspeak-webutils-rename-buffer (key)
  "Setup emacspeak-web-post-process-hook  to rename result buffer"
  (add-hook
   'emacspeak-web-post-process-hook
   (eval
    `(function
      (lambda nil
        (rename-buffer
         (format "%s %s"
                 (buffer-name) ,key)
         'unique))))))

;;;###autoload
(defun emacspeak-webutils-post-process (locator speaker &rest args)
  "Set up post processing steps on a result page.
LOCATOR is a string to search for in the results page.
SPEAKER is a function to call to speak relevant information.
ARGS specifies additional arguments to SPEAKER if any."
  (declare (special emacspeak-web-post-process-hook))
  (when (emacspeak-webutils-supported-p)
    (add-hook
     'emacspeak-web-post-process-hook
     (eval
      `(function
        (lambda nil
          (let ((inhibit-read-only t))
            (condition-case nil
                (cond
                 ((search-forward ,locator nil t)
                  (recenter 0)
                  (apply(quote ,speaker) ,args))
                 (t (message "Your search appears to have failed.")))
              (error nil))))))
     'at-end)))

;;}}}
;;{{{ helper macros:

;;; tVR: moving these from emacspeak-w3 to this module.

(defmacro emacspeak-webutils-without-xsl (&rest body)
  "Execute body with XSL turned off."
  `(progn
     (declare (special emacspeak-we-xsl-p))
     (when emacspeak-we-xsl-p
       (setq emacspeak-we-xsl-p nil)
       (add-hook 'emacspeak-web-post-process-hook
                 #'(lambda ()
                     (declare (special emacspeak-we-xsl-p))
                     (setq emacspeak-we-xsl-p t))
                 'append))
     ,@body))

(defmacro emacspeak-webutils-with-xsl-environment (style params options  &rest body)
  "Execute body with XSL turned on
and xsl environment specified by style, params and options."
  `(progn
     (add-hook
      'emacspeak-web-post-process-hook
      (eval
       `(function
         (lambda ()
           (declare (special emacspeak-we-xsl-p emacspeak-we-xsl-transform
                             emacspeak-xslt-options emacspeak-we-xsl-params))
           (setq emacspeak-we-xsl-p ,emacspeak-we-xsl-p
                 emacspeak-xslt-options ,emacspeak-xslt-options
                 emacspeak-we-xsl-transform ,emacspeak-we-xsl-transform
                 emacspeak-we-xsl-params (quote ,emacspeak-we-xsl-params)))))
      'append)
     (setq emacspeak-we-xsl-p t
           emacspeak-xslt-options ,options
           emacspeak-we-xsl-transform ,style
           emacspeak-we-xsl-params ,params)
     (condition-case nil
         (progn ,@body)
       (error (setq emacspeak-we-xsl-p ,emacspeak-we-xsl-p
                    emacspeak-xslt-options ,emacspeak-xslt-options
                    emacspeak-we-xsl-transform ,emacspeak-we-xsl-transform
                    emacspeak-we-xsl-params ,emacspeak-we-xsl-params)))))

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
;;{{{ Properties from HTML stack:

(defsubst emacspeak-webutils-property-names-from-html-stack (html-stack)
  "Returns list of attributes from HTML stack."
  (delete nil
          (loop for e in html-stack
                append
                (mapcar 'car (rest e)))))

(defun emacspeak-webutils-get-property-from-html-stack (html-stack prop)
  "Extract and return list of prop values from HTML  stack.
Stack is a list of the form ((element-name (attribute-alist)))."
  (let ((props nil))
    (loop for element in html-stack
          do
          (push (cdr (assoc prop (rest element)))
                props))
    (nreverse (delq nil props))))

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
  (browse-url
   (format "http://webcache.googleusercontent.com/search?q=cache:%s"
           (cond
            ((null prefix) (funcall emacspeak-webutils-current-url))
            (t (funcall emacspeak-webutils-url-at-point))))))

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
(defsubst emacspeak-webutils-transcode-this-url-via-google (url)
  "Transcode specified url via Google."
  (declare (special emacspeak-webutils-google-transcoder-url))
  (browse-url
   (format emacspeak-webutils-google-transcoder-url
           (emacspeak-url-encode url))))

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
      (emacspeak-webutils-transcode-this-url-via-google (funcall emacspeak-webutils-url-at-point)))
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
    (emacspeak-webutils-transcode-this-url-via-google
     (funcall emacspeak-webutils-current-url)))
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
(defun emacspeak-webutils-play-media-at-point (&optional  playlist-p)
  "Play media url under point.
Optional interactive prefix arg `playlist-p' says to treat the link as a playlist.
 A second interactive prefix arg adds mplayer option -allow-dangerous-playlist-parsing"
  (interactive "P" )
  (let ((url
         (if emacspeak-webutils-url-at-point
             (funcall emacspeak-webutils-url-at-point)
           (browse-url-url-at-point))))
    (message "Playing media  URL under point")
    (funcall  emacspeak-media-player  url  playlist-p)))

;;;###autoload
(defun emacspeak-webutils-open-in-other-browser ()
  "Opens link in alternate browser."
  (interactive)
  (declare (special major-mode
                    w3-mode
                    eww-mode))
  (emacspeak-webutils-browser-check)
  (if (eq major-mode 'w3-mode)
      (eww-browse-url  (funcall emacspeak-webutils-url-at-point))
    (browse-url-w3 (funcall emacspeak-webutils-url-at-point))))

;;}}}
(provide 'emacspeak-webutils)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
