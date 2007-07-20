;;; emacspeak-webmarks.el --- Web Bookmarks Via Google
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  WebMarks are Web Bookmarks stored at Google
;;; Keywords: Emacspeak,  Audio Desktop Web, Bookmarks
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2007, T. V. Raman
;;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
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

;;{{{  introduction

;;; Commentary:

;;; http://www.google.com/bookmarks provides a simple bookmark
;;; facility
;;; emacspeak-webmarks provides direct minibuffer-level access to
;;; these in the spirit of the Emacspeak Web Command Line.
;;; For now, the emacspeak-webmarks-key needs to be set by hand
;;;  after doing the following:
;;; 0) sign in to google using xml-forms/glogin.xml
;;; 1) search for a bookmark using xml-forms/bookmark-find.xml
;;; will work around this eventually.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'url-parse)
(require 'emacspeak-preamble)
(require 'emacspeak-webutils)
(require 'emacspeak-w3)
;;}}}
;;{{{ Customizations

(defgroup emacspeak-webmarks nil
  "Customization group for WebMarks."
  :group 'emacspeak)
(defcustom emacspeak-webmarks-key nil
  "Magic cookie key to send for bookmark operations.
This gets set the first time we sign in using a browser."
  :type '(choice
          (const :tag "None" nil)
          (string :tag "Key"))
  :group 'emacspeak-webmarks)

(defvar emacspeak-webmarks-list-url-template
  "http://www.google.com/bookmarks/?hl=en&output=rss"
  "URL template for listing all bookmarks.")

(defvar emacspeak-webmarks-find-url-template
  "http://www.google.com/bookmarks/find?q=%s&hl=en&output=rss"
  "URL template for  bookmark searches.")

(defvar emacspeak-webmarks-add-url-template
  "http://www.google.com/bookmarks/mark?op=edit"
  "URL template for adding WebMarks.")

(defvar emacspeak-webmarks-history-search-url-template
  "http://www.google.com/history/find?hl=en&btnSMH=Search+History&output=rss"
  "URL template for search history.")

(defun emacspeak-webmarks-url (template)
  "Return appropriately filled out url."
  (declare (special emacspeak-webmarks-key))
  (concat template
          (format "&zx=%s"
                  emacspeak-webmarks-key)))

;;}}}
;;{{{ keymap:

(declaim(special emacspeak-web-prefix))
(loop for k in
      '(
        ("s" emacspeak-webmarks-search)("h" emacspeak-webmarks-history)
        ("a" emacspeak-webmarks-add)
        ("l" emacspeak-webmarks-list)
        )
      do
      (emacspeak-keymap-update  emacspeak-web-prefix k))

;;}}}
;;{{{ Interactive commands:

;;;###autoload
(defun emacspeak-webmarks-list ()
  "List WebMarks."
  (interactive)
  (declare (special emacspeak-webmarks-key))
  (unless emacspeak-webmarks-key
    (error "WebMarks key not set."))
  (emacspeak-webutils-rss-display
   (emacspeak-webmarks-url
    emacspeak-webmarks-list-url-template)))

;;;###autoload
(defun emacspeak-webmarks-search (query)
  "Search WebMarks."
  (interactive "sQuery: ")
  (declare (special emacspeak-webmarks-key))
  (unless emacspeak-webmarks-key
    (error "WebMarks key not set."))
  (emacspeak-webutils-rss-display
   (format "%s&q=%s"
           (emacspeak-webmarks-url emacspeak-webmarks-find-url-template)
           query)))

;;;###autoload
(defun emacspeak-webmarks-history (query)
  "Search search history."
  (interactive "sQuery: ")
  (declare (special emacspeak-webmarks-key))
  (unless emacspeak-webmarks-key
    (error "WebMarks key not set.
Use form bookmark-add.html, and use the resulting zx param as key"))
  (emacspeak-webutils-rss-display
   (format "%s&q=%s"
           (emacspeak-webmarks-url emacspeak-webmarks-history-search-url-template)
           query)))

;;;###autoload
(defun emacspeak-webmarks-add (url title notes)
  "Add WebMark."
  (interactive
   (list
    (read-from-minibuffer "URL: "
                          (if (or (eq major-mode 'w3-mode)
                                  (eq major-mode 'w3m-mode))
                              (funcall
                               emacspeak-webutils-url-at-point)
                            (browse-url-url-at-point)))
    (read-from-minibuffer "Title: "
                          (if (or (eq major-mode 'w3-mode)
                                  (eq major-mode 'w3m-mode))
                              (funcall
                               emacspeak-webutils-document-title)
                            (buffer-substring
                             (line-beginning-position)
                             (line-end-position))))
    (read-from-minibuffer "Notes: ")))
  (declare (special emacspeak-webmarks-key))
  (unless emacspeak-webmarks-key
    (error "WebMarks key not set."))
  (let ((base-url (format "%s&title=%s&bkmk=%s&annotation=%s"
                          (emacspeak-webmarks-url emacspeak-webmarks-add-url-template)
                          (emacspeak-url-encode title)
                          (emacspeak-url-encode url)
                          (emacspeak-url-encode notes))))
    (emacspeak-webutils-with-xsl-environment
     (expand-file-name "xpath-filter.xsl" emacspeak-xslt-directory)
     (emacspeak-xslt-params-from-xpath
      "//form[@name=\"add_bkmk_form\"]" base-url)
     emacspeak-xslt-options
     (browse-url base-url))))

;;}}}
(provide 'emacspeak-webmarks)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
