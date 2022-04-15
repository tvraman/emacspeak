;; emacspeak-iedit.el --- Speech-enable IEDIT  -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Description:  Speech-enable IEDIT An Emacs Interface to iedit
;; Keywords: Emacspeak,  Audio Desktop iedit
;;{{{  LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;  $Revision: 4532 $ |
;; Location undetermined
;; 

;;}}}
;;{{{  Copyright:
;; Copyright (C) 1995 -- 2007, 2011, T. V. Raman
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
;; MERCHANTABILITY or FITNIEDIT FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;; Commentary:
;; IEDIT ==  Edit multiple regions
;; This module speech-enables iedit.

;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Map Faces:

(voice-setup-add-map
 '(
   (iedit-occurrence voice-overlay-1)
   (iedit-read-only-occurrence voice-monotone-extra)))

;;}}}
;;{{{ Interactive Commands:

'(
  iedit-apply-global-modification
  iedit-execute-last-modification
  iedit-expand-down-a-line
  iedit-expand-down-to-occurrence
  iedit-expand-up-a-line
  iedit-expand-up-to-occurrence
  iedit-number-occurrences
  iedit-replace-occurrences
  iedit-restrict-current-line
  iedit-restrict-function

  )

(defadvice iedit-mode (after emacspeak pre act comp)
  "speak."
  (cl-declare (special iedit-mode))
  (when (ems-interactive-p)
    (emacspeak-auditory-icon (if iedit-mode 'on 'off))))

(defadvice iedit-done (after emacspeak pre act comp)
  "speak."
  (emacspeak-auditory-icon 'close-object)
  (message "IEdit done"))

(cl-loop
 for f in
 '(
   iedit-prev-occurrence iedit-next-occurrence
   iedit-goto-last-occurrence iedit-goto-first-occurrence
   iedit-goto-last-occurrence)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'large-movement)
       (emacspeak-speak-line)))))
(cl-loop
 for f in
 '(iedit-describe-bindings iedit-describe-key iedit-describe-mode)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'help)))))

(cl-loop
 for f in
 '(
   iedit-upcase-occurrences iedit-downcase-occurrences
   iedit-blank-occurrences iedit-delete-occurrences)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'task-done)
       (message "%s"  ,(symbol-name f))))))

(defadvice iedit-show/hide-unmatched-lines (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon
     (if iedit-unmatched-lines-invisible 'on 'off))))

;;}}}
(provide 'emacspeak-iedit)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}
