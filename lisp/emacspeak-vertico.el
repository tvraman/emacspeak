;;; emacspeak-vertico.el --- Speech-enable Vertico  -*- lexical-binding: t; -*-
;;; Author: Krzysztof Drewniak <krzysdrewniak@gmail.com>
;;; Description:  Speech-enable Vertico, a modern Emacs completion interface
;;; Keywords: Emacspeak, Audio Desktop, Vertico, completion

;;{{{  Copyright:

;;; Copyright (C) 2021 Krzysztof Drewniak <krzysdrewniak@gmail.com>
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
;;; MERCHANTABILITY or FITNMARKDOWN FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; Vertico is a modern completion UI that uses Emacs's native completion engine
;;; This module speech-enables Vertico's UI

;;}}}
;;; Code:
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'vertico nil 'noerror)
;;}}}
;;{{{ Map faces to voices:

(voice-setup-add-map
 '((vertico-group-title voice-smoothen)
   (vertico-group-separator voice-overlay-0)))

;;}}}
;;{{{ Define bookkeeping variables for UI state

(defvar-local emacspeak-vertico--prev-candidate nil
  "Previously spoken candidate")

(defvar-local emacspeak-vertico--prev-index nil
  "Index of previously spoken candidate")

;;}}}
;;{{{
(declare-function 'vertico--candidate "vertico.el" (&optional hl))
(declare-function 'vertico--allow-prompt-selection-p "vertico.el" ())
;;}}}
;;{{{ Advice interactive commands

(defadvice vertico-insert (around emacspeak pre act comp)
  "speak."
  (let* ((orig-point (point)))
    ad-do-it
    (emacspeak-auditory-icon 'complete)
    (emacspeak-speak-region orig-point (point)))
  ad-return-value)

(defadvice vertico--exhibit (after emacspeak pre act comp)
  "speak."
  (cl-declare (special vertico--index vertico--base))
  (let ((new-cand
         (substring (vertico--candidate)
                    (if (>= vertico--index 0) (length vertico--base) 0)))
        (to-speak nil))
    (unless (equal emacspeak-vertico--prev-candidate new-cand)
      (push new-cand to-speak)
      (when (or (equal vertico--index emacspeak-vertico--prev-index)
                (and (not (equal vertico--index -1))
                     (equal emacspeak-vertico--prev-index -1)))
        (push "candidate" to-speak)))
    (when (and (not (vertico--allow-prompt-selection-p))
               (equal emacspeak-vertico--prev-candidate nil))
      (push "first candidate" to-speak))
    (when to-speak
      (dtk-speak (mapconcat 'identity to-speak " ")))
    (setq-local
     emacspeak-vertico--prev-candidate new-cand
     emacspeak-vertico--prev-index vertico--index)))

(cl-loop
 for (f icon) in
 '((vertico-scroll-up scroll)
   (vertico-scroll-down scroll)
   (vertico-first large-movement)
   (vertico-last large-movement)
   (vertico-next select-object)
   (vertico-previous select-object)
   (vertico-exit close-object)
   (vertico-kill delete-object))
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon ',icon)))))

;;}}}
(provide 'emacspeak-vertico)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
