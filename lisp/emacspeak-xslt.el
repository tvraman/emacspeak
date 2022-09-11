;;; emacspeak-xslt.el --- Implements Emacspeak  xslt transform engine  -*- lexical-binding: t; -*-
;;
;; $Author: tv.raman.tv $
;; Description:  xslt transformation routines
;; Keywords: Emacspeak,  Audio Desktop XSLT
;;{{{  LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;; 
;;  $Revision: 4562 $ |
;; Location undetermined
;; 

;;}}}
;;{{{  Copyright:
;; Copyright (C) 1995 -- 2022, T. V. Raman
;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
;; All Rights Reserved.
;; 
;; This file is not part of GNU Emacs, but the same permissions apply.
;; 
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;; libxml and libxsl are XML libraries for GNOME.
;; xsltproc is a  xslt processor using libxsl
;; this module defines routines for applying xsl transformations
;; using xsltproc
;;; Code:

;;}}}
;;{{{  Required modules

(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
;;}}}
;;{{{  xslt Environment:
(defsubst emacspeak-xslt-get (style)
  "Return  stylesheet path."
  (expand-file-name style emacspeak-xslt-directory))

(defconst emacspeak-opml-view-xsl
  (eval-when-compile  (emacspeak-xslt-get "opml.xsl"))
  "XSL stylesheet used for viewing OPML  Feeds.")

(defconst emacspeak-rss-view-xsl
  (eval-when-compile  (emacspeak-xslt-get "rss.xsl"))
  "XSL stylesheet used for viewing RSS Feeds.")

(defconst emacspeak-atom-view-xsl
  (eval-when-compile  (emacspeak-xslt-get "atom.xsl"))
  "XSL stylesheet used for viewing Atom Feeds.")

(defun emacspeak-xslt-params-from-xpath (path base)
  "Return params suitable for passing to  emacspeak-xslt-region"
  (list
   (cons "path"
         (format "\"'%s'\""
                 (shell-quote-argument path)))
   (cons "locator"
         (format "'%s'"
                 path))
   (cons "base"
         (format "\"'%s'\""
                 base))))

(defun emacspeak-xslt-read ()
  "Read XSLT transformation name from minibuffer."
  (cl-declare (special emacspeak-xslt-directory emacspeak-we-xsl-transform))
  (expand-file-name
   (read-file-name "XSL Transformation: "
                   emacspeak-xslt-directory
                   emacspeak-we-xsl-transform)))
(defvar emacspeak-xslt-program
  (executable-find "xsltproc")
  "Name of XSLT transformation engine.")

(defvar emacspeak-xslt-options
  "--html --nonet --novalid --encoding utf-8"
  "Options passed to xsltproc.")

(defvar emacspeak-xslt-keep-errors  nil
  "If non-nil, xslt errors will be preserved in an errors buffer.")

(defvar emacspeak-xslt-nuke-null-char t
  "If T null chars in the region will be nuked.
This is useful when handling bad HTML.")

;;}}}
;;{{{Macro: without-xsl
(defmacro emacspeak-xslt-without-xsl (&rest body)
  "Execute body with XSL turned off."
  (declare (indent 1) (debug t))
  `(progn
     (cl-declare (special emacspeak-we-xsl-p))
     (when emacspeak-we-xsl-p
       (setq emacspeak-we-xsl-p nil)
       (add-hook 'emacspeak-eww-post-process-hook
                 #'(lambda ()
                     (cl-declare (special emacspeak-we-xsl-p))
                     (setq emacspeak-we-xsl-p t))
                 'append))
     ,@body))

;;}}}
;;{{{XSLT Transformer functions:

(defun emacspeak-xslt-make-xsl-transformer  (xsl &optional params)
  "Return a function that can be attached to
emacspeak-eww-pre-process-hook to apply required xslt transform."
  (cond
   ((null params)
    (eval
     `#'(lambda ()
          (emacspeak-xslt-region ,xsl (point) (point-max)))))
   (t
    (eval
     `#'(lambda ()
          (emacspeak-xslt-region ,xsl (point) (point-max) ',params))))))

(defun emacspeak-xslt-make-xsl-transformer-pipeline   (specs url)
  "Return a function that can be attached to
emacspeak-eww-pre-process-hook to apply required xslt transformation
pipeline. Argument `specs' is a list of elements of the form `(xsl params)'."
  (eval
   `#'(lambda ()
        (cl-loop
         for s in ',specs do
         (emacspeak-xslt-region
          (cl-first s)
          (point) (point-max)
          (emacspeak-xslt-params-from-xpath (cl-second s) ,url))))))

;;}}}
;;{{{ Functions:

;;;###autoload
(defun emacspeak-xslt-region (xsl start end &optional params no-comment)
  "Apply XSLT transformation to region and replace it with the result.  "
  (cl-declare (special emacspeak-xslt-program emacspeak-xslt-options
                       emacspeak-xslt-keep-errors modification-flag))
  (save-excursion
    (with-silent-modifications
      (let ((command nil)
            (parameters (when params
                          (mapconcat
                           #'(lambda (pair)
                               (format "--param %s %s "
                                       (car pair)
                                       (cdr pair)))
                           params
                           " ")))
            (coding-system-for-write 'utf-8)
            (coding-system-for-read 'utf-8)
            (buffer-file-coding-system 'utf-8))
        (setq command
              (format
               "%s %s  %s  %s - %s"
               emacspeak-xslt-program
               (or emacspeak-xslt-options "")
               (or parameters "")
               xsl
               (unless  emacspeak-xslt-keep-errors " 2>/dev/null ")))
        (shell-command-on-region
         start end
         command
         (current-buffer)
         'replace
         (when emacspeak-xslt-keep-errors "*xslt errors*"))
        (when (get-buffer  "*xslt errors*")
          (bury-buffer "*xslt errors*"))
        (unless no-comment
          (goto-char (point-max))
          (insert
           (format "<!--\n %s \n-->\n"
                   command)))
        (set-buffer-multibyte t)
        (current-buffer)))))

(defun emacspeak-xslt-run (xsl &optional start end)
  "Run xslt on region, and return output filtered by sort -u.
Region defaults to entire buffer."
  (cl-declare (special emacspeak-xslt-program emacspeak-xslt-options))
  (or start (setq start (point-min)))
  (or end (setq end (point-max)))
  (let ((coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        (buffer-file-coding-system 'utf-8))
    (shell-command-on-region
     start end
     (format "%s %s %s - 2>/dev/null | sort -u"
             emacspeak-xslt-program emacspeak-xslt-options xsl)
     (current-buffer) 'replace)
    (set-buffer-multibyte t)
    (current-buffer)))

;;;###autoload
(defun emacspeak-xslt-url (xsl url &optional params no-comment)
  "Apply XSLT transformation to url
and return the results in a newly created buffer. "
  (cl-declare (special emacspeak-xslt-program
                       emacspeak-xslt-keep-errors))
  (let ((result (get-buffer-create " *xslt result*"))
        (command nil)
        (parameters (when params
                      (mapconcat
                       #'(lambda (pair)
                           (format "--param %s %s "
                                   (car pair)
                                   (cdr pair)))
                       params
                       " "))))
    (setq command
          (format
           "curl --silent %s | %s %s --html --novalid %s - %s"
           url
           emacspeak-xslt-program
           (or parameters "")
           xsl
           (unless emacspeak-xslt-keep-errors " 2>/dev/null ")))
    (with-current-buffer result 
      (kill-all-local-variables)
      (erase-buffer)
      (setq buffer-undo-list t)
      (let ((coding-system-for-write 'utf-8)
            (coding-system-for-read 'utf-8)
            (buffer-file-coding-system 'utf-8))
        (shell-command
         command (current-buffer)
         (when emacspeak-xslt-keep-errors "*xslt errors*"))
        (when emacspeak-xslt-nuke-null-char
          (goto-char (point-min))
          (while (search-forward
                  (format "%c" 0)
                  nil  t)
            (replace-match " "))))
      (when (get-buffer  "*xslt errors*")
        (bury-buffer "*xslt errors*"))
      (goto-char (point-max))
      (unless no-comment
        (insert
         (format "<!--\n %s \n-->\n"
                 command)))
      (setq modification-flag nil)
      (set-buffer-multibyte t)
      (goto-char (point-min))
      result)))

(defun emacspeak-xslt-pipeline-url (specs url &optional  no-comment)
  "Apply XSLT transformation to url
and browse the results.
Argument `specs' is a list of elements of the form
`(xsl xpath)'.
  This uses XSLT processor xsltproc available as
part of the libxslt package."
  (cl-declare (special emacspeak-xslt-program
                       emacspeak-xslt-keep-errors))
  (let ((result (url-retrieve-synchronously url))
        (command ""))
    (setq command
          (apply
           #'concat
           (cl-loop
            for s in specs
            and i from 0 collect 
            (format
             "%s %s %s %s %s - 2>/dev/null  "
             (if (= i 0)  "" "|")
             emacspeak-xslt-program
             (or emacspeak-xslt-options "")
             (mapconcat
              #'(lambda (pair)
                  (format "--param %s %s "
                          (car pair) (cdr pair)))
              (emacspeak-xslt-params-from-xpath (cl-second s) url)
              "")
             (cl-first s)))))
    (with-silent-modifications
      (with-current-buffer result 
        (let ((coding-system-for-write 'utf-8)
              (coding-system-for-read 'utf-8)
              (buffer-file-coding-system 'utf-8))
          (goto-char (point-min))
          (search-forward "\n\n")
          (delete-region (point-min) (point))
          (shell-command-on-region
           (point-min) (point-max)
           command (current-buffer) 'replace
           (when emacspeak-xslt-keep-errors "*xslt errors*")))
        (when (get-buffer  "*xslt errors*")
          (bury-buffer "*xslt errors*"))
        (goto-char (point-max))
        (unless no-comment
          (insert
           (format "<!--\n %s \n-->\n"
                   command)))
        (set-buffer-multibyte t)
        (goto-char (point-min))
        (browse-url-of-buffer)))))

;;;###autoload
(defun emacspeak-xslt-xml-url (xsl url &optional params)
  "Apply XSLT transformation to XML url
and return the results in a newly created buffer. "
  (cl-declare (special emacspeak-xslt-program
                       emacspeak-xslt-keep-errors))
  (let ((result (get-buffer-create " *xslt result*"))
        (command nil)
        (parameters
         (when params
           (mapconcat
            #'(lambda (pair)
                (format "--param %s %s " (car pair) (cdr pair)))
            params " "))))
    (setq command
          (format
           "%s %s --novalid %s '%s' %s"
           emacspeak-xslt-program
           (or parameters "")
           xsl url
           (unless emacspeak-xslt-keep-errors " 2>/dev/null ")))
    (save-current-buffer
      (set-buffer result)
      (kill-all-local-variables)
      (erase-buffer)
      (let ((coding-system-for-write 'utf-8)
            (coding-system-for-read 'utf-8)
            (buffer-file-coding-system 'utf-8))
        (shell-command
         command (current-buffer)
         (when emacspeak-xslt-keep-errors
           "xslt errors*")))
      (when (get-buffer  "*xslt errors*")
        (bury-buffer "*xslt errors*"))
      (goto-char (point-max))
      (insert
       (format "<!--\n %s \n-->\n"
               command))
      (setq modification-flag nil)
      (goto-char (point-min))
      (set-buffer-multibyte t)
      result)))

;;}}}
;;{{{handle charent
(defvar emacspeak-xslt-charent-alist
  '(("&lt;" . "<")
    ("&gt;" . ">")
    ("&quot;" . "\"")
    ("&apos;" . "'")
    ("&amp;" . "&"))
  "Entities to unescape when treating badly escaped XML.")

(defun emacspeak-xslt-unescape-charent (start end)
  "Clean up charents in XML."
  (cl-declare (special emacspeak-xslt-charent-alist))
  (cl-loop for entry in emacspeak-xslt-charent-alist
           do
           (let ((entity (car  entry))
                 (replacement (cdr entry)))
             (goto-char start)
             (while (search-forward entity end t)
               (replace-match replacement nil t)))))

;;}}}
;;{{{ interactive commands:

;;;###autoload
(defun emacspeak-xslt-view-file(style file)
  "Transform `file' using `style' and preview via browse-url."
  (interactive
   (list
    (read-file-name "Style File: " emacspeak-xslt-directory)
    (read-file-name "File:")))
  (cl-declare (special emacspeak-xslt-directory))
  (with-temp-buffer
    (let ((browse-url-browser-function  'eww-browse-url)
          (coding-system-for-read 'utf-8)
          (coding-system-for-write 'utf-8)
          (buffer-file-coding-system 'utf-8))
      (insert-file-contents file)
      (shell-command
       (format "%s   --novalid --nonet --param base %s  %s  \"%s\"  2>/dev/null"
               emacspeak-xslt-program 
               (format "\"'file://%s'\"" file)
               style file)
       (current-buffer) 'replace)
      (browse-url-of-buffer))))

;;;###autoload
(defun emacspeak-xslt-view (style url)
  "Browse URL with specified XSL style."
  (interactive
   (list
    (expand-file-name
     (read-file-name "XSL Transformation: "))
    (emacspeak-eww-read-url)))
  (cl-declare (special emacspeak-xslt-options))
  (add-hook
   'emacspeak-eww-pre-process-hook
   (emacspeak-xslt-make-xsl-transformer style))
  (browse-url url))

(defun emacspeak-xslt-view-xml (style url &optional unescape-charent)
  "Browse XML URL with specified XSL style."
  (interactive
   (list
    (emacspeak-xslt-read)
    (emacspeak-eww-read-url)
    current-prefix-arg))
  (let ((browse-url-browser-function  'eww-browse-url)
        (src-buffer
         (emacspeak-xslt-xml-url
          style
          url
          (list
           (cons "base"
                 (format "\"'%s'\""
                         url))))))
    (when (called-interactively-p 'interactive) (emacspeak-eww-autospeak))
    (save-current-buffer
      (set-buffer src-buffer)
      (when unescape-charent
        (emacspeak-xslt-unescape-charent (point-min) (point-max)))
      (emacspeak-xslt-without-xsl
       (browse-url-of-buffer)))
    (kill-buffer src-buffer)))

(defun emacspeak-xslt-view-region (style start end &optional unescape-charent)
  "Browse XML region with specified XSL style."
  (interactive
   (list
    (emacspeak-xslt-read)
    (point)
    (mark)
    current-prefix-arg))
  (let ((browse-url-browser-function  'eww-browse-url)
        (src-buffer
         (with-silent-modifications
           (emacspeak-xslt-region style start end))))
    (save-current-buffer
      (set-buffer src-buffer)
      (when unescape-charent
        (emacspeak-xslt-unescape-charent (point-min) (point-max)))
      (emacspeak-xslt-without-xsl
       (browse-url-of-buffer)))
    (kill-buffer src-buffer)))

;;}}}
(provide 'emacspeak-xslt)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}
