;;; emacspeak-atom.el --- Emacspeak ATOM Wizard
;;; $Id$
;;; $Author$
;;; Description:  ATOM Wizard for the emacspeak desktop
;;; Keywords: Emacspeak,  Audio Desktop ATOM
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
;;;Copyright (C) 1995 -- 2004, T. V. Raman 
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

;;;###autoload
(defun emacspeak-atom-display (atom-url &optional speak)
  "Retrieve and display ATOM URL."
  (interactive
   (list
    (car
     (browse-url-interactive-arg "ATOM URL: "))))
  (declare (special emacspeak-atom-unescape-html
   emacspeak-xslt-directory))
  (when (or (interactive-p)speak)
    (add-hook 'emacspeak-w3-post-process-hook
	      'emacspeak-speak-buffer))
  (emacspeak-w3-browse-xml-url-with-style
   (expand-file-name "atom.xsl" emacspeak-xslt-directory)
   atom-url
					(and emacspeak-atom-unescape-html 'unescape-charent)
   ))

;;;###autoload
(defun emacspeak-atom-browse (feed)
  "Browse specified ATOM feed."
  (interactive
   (list
    (let ((completion-ignore-case t))
      (completing-read "Feed:"
		       emacspeak-atom-feeds))))
  (let ((uri (cadr
              (assoc feed emacspeak-atom-feeds))))
    (emacspeak-atom-display uri 'speak)))

;;}}}
(provide 'emacspeak-atom)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
