;;; emacspeak-selectrum.el --- Speech-enable SELECTRUM  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable SELECTRUM An Emacs Interface to selectrum
;;; Keywords: Emacspeak,  Audio Desktop selectrum
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
;;;Copyright (C) 1995 -- 2007, 2019, T. V. Raman
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
;;; MERCHANTABILITY or FITNSELECTRUM FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; SELECTRUM ==  Flexibly select from lists.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Map Faces:

(voice-setup-add-map
 '(
   (selectrum-completion-annotation voice-annotate)
   (selectrum-completion-docsig voice-monotone)
   (selectrum-current-candidate voice-bolden)
   (selectrum-primary-highlight voice-animate)
   (selectrum-secondary-highlight voice-lighten)))

;;}}}
;;{{{Fix interactive commands:

'(
  selectrum-kill-ring-save
  selectrum-prescient-toggle-anchored
  selectrum-prescient-toggle-fuzzy
  selectrum-prescient-toggle-initialism
  selectrum-prescient-toggle-literal
  selectrum-prescient-toggle-prefix
  selectrum-prescient-toggle-regexp
  selectrum-repeat
  
  selectrum-select-from-history
  
  )



(defadvice selectrum-select-current-candidate (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (when (and ad-return-value (stringp ad-return-value))
      (dtk-speak ad-return-value))
    (emacspeak-auditory-icon 'close-object)))

(defadvice selectrum-submit-exact-input (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)))

(defadvice selectrum-insert-current-candidate (around emacspeak pre act comp)
  "Provide auditory feedback."
  (cond
   ((ems-interactive-p)
    (let ((orig (point)))
      ad-do-it
      (emacspeak-auditory-icon 'complete)
      (emacspeak-speak-region orig (point))))
   (t ad-do-it))
  ad-return-value)

(cl-loop
 for f in 
 '(
   selectrum-next-page selectrum-previous-page
   selectrum-goto-beginning selectrum-goto-end)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'large-movement)
       (emacspeak-speak-line)))))

(cl-loop
 for f in 
 '(selectrum-previous-candidate selectrum-next-candidate)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'select-object)
       (emacspeak-speak-line)))))


;;}}}
(provide 'emacspeak-selectrum)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
