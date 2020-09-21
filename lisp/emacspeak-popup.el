;;; emacspeak-popup.el --- Speech-enable POPUP  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable POPUP An Emacs Interface to popup
;;; Keywords: Emacspeak,  Audio Desktop popup
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
;;;Copyright (C) 1995 -- 2007, 2011, T. V. Raman
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
;;; MERCHANTABILITY or FITNPOPUP FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; POPUP ==  popup.el from MELPA

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'popup "popup" 'no-error)
;;}}}
;;{{{ Map Faces:

(voice-setup-add-map
 '(
   (popup-face voice-bolden)
   (popup-isearch-match voice-animate)
   (popup-menu-face voice-monotone)
   (popup-menu-mouse-face voice-monotone)
   (popup-menu-selection-face voice-lighten)
   (popup-menu-summary-face voice-smoothen)
   (popup-summary-face voice-smoothen)
   (popup-tip-face voice-lighten)))

;;}}}
;;{{{ Interactive Commands:

(defun emacspeak-popup-speak-item (popup)
  "Speak current item."
  (let ((msg (elt (popup-list popup) (popup-cursor popup))))
    (message msg)))

(defadvice popup-menu-event-loop (around emacspeak pre act comp)
  "Provide auditory feedback."
  (emacspeak-auditory-icon 'open-object)
  (emacspeak-popup-speak-item (ad-get-arg 0))
  ad-do-it
  (emacspeak-auditory-icon 'close-object))

(defadvice popup-menu-read-key-sequence (before emacspeak pre act comp)
  "Speak our prompt."
  (when (sit-for 2)
    (dtk-speak (or (ad-get-arg 1)
                   "Menu:"))))

(cl-loop
 for f in
 '(popup-next popup-previous)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     (emacspeak-auditory-icon 'select-object)
     (emacspeak-popup-speak-item (ad-get-arg 0)))))

(cl-loop
 for f in
 '(popup-page-next popup-page-previous)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     (emacspeak-auditory-icon 'scroll)
     (emacspeak-popup-speak-item (ad-get-arg 0)))))
(defadvice popup-menu-show-help (after emacspeak pre act comp)
  "Speak help if available."
  (let ((doc (popup-item-documentation item)))
    (emacspeak-auditory-icon 'help)
    (if doc
        (dtk-speak doc)
      (dtk-speak "helpless"))))

;;}}}
;;{{{ Augment popup keymap:

(eval-after-load
    "popup"
  `(define-key popup-menu-keymap   emacspeak-prefix 'emacspeak-prefix-command))

;;}}}

(provide 'emacspeak-popup)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
