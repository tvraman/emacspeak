;;; emacspeak-nero.el --- Speech-Enable nero (interface to lynx)
;;; $Id$
;;; $Author$
;;; Description: speech-enable nero (light-weight lynx rapper).
;;; Keywords: Emacspeak, nero
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

;;; Copyright (c) 1995 -- 2004, T. V. Raman
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

;;{{{ Introduction:

;;; Commentary:
;;; nero.el (posted to gnu.emacs.sources) is a light-weight elisp
;;; wrapper for lynx. It's a very efficient wa of reading HTML
;;; content this module advices interactive commands from nero.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'emacspeak-preamble)

;;}}}
;;{{{ Advice interactive commands:

(defadvice nero-revisionism (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (message "Cleared context")))

(defadvice nero-reload (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Reloaded document.")))
(loop for  f in
      (list 'nero-back 'nero-forward)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
	  "Provide auditory feedback."
	  (when (interactive-p)
	    (emacspeak-auditory-icon 'select-object)
	    (emacspeak-speak-mode-line)))))

(loop for f in
      (list
       'nero-follow-link
       'nero-follow-current-link)
      do
      (eval
       `(defadvice ,f (around emacspeak pre act comp)
	  "Provide auditory feedback."
	  (cond
	   ((interactive-p)
	    (emacspeak-auditory-icon 'select-object)
	    ad-do-it
	    (emacspeak-speak-mode-line)
	    (emacspeak-auditory-icon 'open-object))
	   (t ad-do-it))
	  ad-return-value)))

(defadvice nero-move-to-next-link (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(loop for f in
      (list 'nero-hide 'nero-finished)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
	  "Provide auditory feedback."
	  (when (interactive-p)
	    (emacspeak-auditory-icon 'close-object)
	    (emacspeak-speak-mode-line)))))

(defadvice nero-kill-ring-save-current-url (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (message "Copied URL to kill ring")))

(defadvice nero-toggle-display-of-links (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'button)
    (message "Turned %s display of links"
             (if nero-links-visible " on " " off "))))

(defadvice nero-browse-url (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

;;}}}
(provide 'emacspeak-nero)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
