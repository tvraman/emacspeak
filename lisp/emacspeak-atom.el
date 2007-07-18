;;; emacspeak-atom.el --- Emacspeak ATOM Wizard
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  ATOM Wizard for the emacspeak desktop
;;; Keywords: Emacspeak,  Audio Desktop ATOM
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-05-25 13:56:11 -0700 (Fri, 25 May 2007) $ |
;;;  $Revision: 4598 $ |
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

;;; Simple ATOM wizard for Emacspeak

;;}}}
;;{{{  Required modules

(require 'emacspeak-preamble)
(require 'emacspeak-webutils)
(require 'browse-url)

;;}}}
;;{{{ ATOM feed cache

;;;###autoload
(defgroup emacspeak-atom nil
  "ATOM Feeds for the Emacspeak desktop."
  :group 'emacspeak)

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

(defcustom emacspeak-atom-unescape-html t
  "Fix malformed  XML that results from sites attempting to
unescape HTML tags."
  :type 'boolean
  :group 'emacspeak-atom)

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
(defun emacspeak-atom-display (atom-url &optional speak)
  "Retrieve and display ATOM URL."
  (interactive
   (list
    (read-from-minibuffer "URL: "
                          (if (or (eq major-mode 'w3-mode)
                                  (eq major-mode 'w3m-mode))
                              (or (funcall emacspeak-webutils-url-at-point)
                                  (funcall emacspeak-webutils-current-url))))
    (or (interactive-p) current-prefix-arg)))
  (declare (special emacspeak-atom-view-xsl))
  (when speak
    (add-hook 'emacspeak-w3-post-process-hook
              'emacspeak-speak-buffer))
  (when speak
    (add-hook 'emacspeak-w3-post-process-hook
              'emacspeak-speak-buffer))
  (emacspeak-w3-browse-xml-url-with-style
   emacspeak-atom-view-xsl
   rss-url
   'unescape))

;;;###autoload
(defun emacspeak-atom-browse (feed)
  "Browse specified ATOM feed."
  (interactive
   (list
    (let ((completion-ignore-case t))
      (completing-read "Feed:"
                       emacspeak-atom-feeds)))
  (let ((uri (cadr
              (assoc feed emacspeak-atom-feeds))))))
    (emacspeak-atom-display uri 'speak))

;;}}}
(provide 'emacspeak-atom)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
