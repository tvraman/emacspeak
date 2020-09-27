;;; emacspeak-webutils.el --- Common Web Utilities For Emacspeak  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak Webutils
;;; Keywords: Emacspeak, web
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2008-08-14 11:23:31 -0700 (Thu, 14 Aug 2008) $ |
;;;  $Revision: 4634 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1995 -- 2018, T. V. Raman
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
;;; Code:

;;}}}
;;{{{ required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'url-http)
(require 'url)
(require 'browse-url)
(require 'shr)

;;}}}
;;{{{ web-pre-process

;;;###autoload
(defvar emacspeak-web-pre-process-hook nil
  "Pre-process hook -- to be used for XSL preprocessing etc.")

(defun emacspeak-webutils-run-pre-process-hook (&rest _ignore)
  "Run web pre process hook."
  (cl-declare (special emacspeak-web-pre-process-hook))
  (when     emacspeak-web-pre-process-hook
    (condition-case nil
        (let ((inhibit-read-only t))
          (run-hooks  'emacspeak-web-pre-process-hook))
      ((debug error)  (message "Caught error  in pre-process hook.")
       (setq emacspeak-web-pre-process-hook nil)))
    (setq emacspeak-web-pre-process-hook nil)))

;;}}}
;;{{{ web-post-process

;;;###autoload
(defvar emacspeak-web-post-process-hook nil
  "Set locally to a  site specific post processor.
Note that the Web browser should reset this hook after using it.")

(defun emacspeak-webutils-run-post-process-hook (&rest _ignore)
  "Use web post process hook."
  (cl-declare (special emacspeak-web-post-process-hook))
  (when     emacspeak-web-post-process-hook
    (condition-case nil
        (let ((inhibit-read-only t))
          (run-hooks  'emacspeak-web-post-process-hook))
      ((debug error)  (message "Caught error  in post-process hook.")
       (setq emacspeak-web-post-process-hook nil)))
    (setq emacspeak-web-post-process-hook nil)))

;;}}}
;;{{{ Helpers:

(defun emacspeak-webutils-make-xsl-transformer  (xsl &optional params)
  "Return a function that can be attached to emacspeak-web-pre-process-hook to apply required xslt transform."
  (cond
   ((null params)
    (eval
     `#'(lambda ()
          (emacspeak-xslt-region ,xsl (point) (point-max)))))
   (t
    (eval
     `#'(lambda ()
          (emacspeak-xslt-region ,xsl (point) (point-max) ',params))))))

(defun emacspeak-webutils-make-xsl-transformer-pipeline   (specs url)
  "Return a function that can be attached to
emacspeak-web-pre-process-hook to apply required xslt transformation
pipeline. Argument `specs' is a list of elements of the form `(xsl params)'."
  (eval
   `#'(lambda ()
        (cl-loop
         for s in ',specs do
         (emacspeak-xslt-region
          (cl-first s)
          (point) (point-max)
          (emacspeak-xslt-params-from-xpath (cl-second s) ,url))))))

(defvar emacspeak-webutils-charent-alist
  '(("&lt;" . "<")
    ("&gt;" . ">")
    ("&quot;" . "\"")
    ("&apos;" . "'")
    ("&amp;" . "&"))
  "Entities to unescape when treating badly escaped XML.")

(defun emacspeak-webutils-unescape-charent (start end)
  "Clean up charents in XML."
  (cl-declare (special emacspeak-webutils-charent-alist))
  (cl-loop for entry in emacspeak-webutils-charent-alist
           do
           (let ((entity (car  entry))
                 (replacement (cdr entry)))
             (goto-char start)
             (while (search-forward entity end t)
               (replace-match replacement nil t)))))

(defsubst emacspeak-eww-autospeak()
  "Setup post process hook to speak the Web page when rendered.
Forward punctuation and rate  settings to resulting buffer."
  (let ((p dtk-punctuation-mode)
       (r dtk-speech-rate))
    (add-hook
     'emacspeak-web-post-process-hook
     #'(lambda nil
         (cl-declare (special emacspeak-we-xpath-filter))
         (let ((inhibit-read-only t))
           (dtk-set-punctuations p)
           (dtk-set-rate r)
           (emacspeak-dtk-sync)
           (setq emacspeak-we-xpath-filter emacspeak-we-paragraphs-xpath-filter)
           (emacspeak-speak-buffer)))
     'at-end)))

(defun emacspeak-webutils-cache-google-query(query)
  "Setup post process hook to cache google query when rendered."
  (cl-declare (special emacspeak-google-query))
  (let ((cache
         (eval
          `#'(lambda nil
              (setq emacspeak-google-query ,query)))))
    (add-hook 'emacspeak-web-post-process-hook cache 'at-end)))

(defun emacspeak-webutils-cache-google-toolbelt(belt)
  "Setup post process hook to cache google toolbelt when rendered."
  (cl-declare (special emacspeak-google-toolbelt))
  (let ((cache
         (eval 
                 `#'(lambda nil
                   (setq emacspeak-google-toolbelt' ,belt)))))
    (add-hook 'emacspeak-web-post-process-hook cache 'at-end)))

(defsubst emacspeak-webutils-browser-check ()
  "Check to see if functions are called from a browser buffer"
  (cl-declare (special major-mode))
  (unless (eq major-mode 'eww-mode)
    (error "This command cannot be used outside browser buffers.")))

(defun emacspeak-webutils-read-url ()
  "Return URL under point
or URL read from minibuffer."
  (let ((url (shr-url-at-point nil)))
    (if url
        url 
      (car (browse-url-interactive-arg "URL: ")))))

;;;  Helper: rename result buffer
(defun emacspeak-webutils-rename-buffer (key)
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
  (cl-declare (special emacspeak-web-post-process-hook))
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
     'at-end))

;;}}}
;;{{{ helper macros:

;;;###autoload

;;}}}
(provide 'emacspeak-webutils)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
