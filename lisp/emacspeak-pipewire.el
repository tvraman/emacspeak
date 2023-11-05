;;; emacspeak-pipewire.el --- Speech-enable PIPEWIRE  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable PIPEWIRE An Emacs Interface to pipewire
;;; Keywords: Emacspeak,  Audio Desktop pipewire
;;  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;  Copyright:

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  introduction

;;; Commentary:
;; PIPEWIRE ==  Pipewire Interaction from Emacs.

;;; Code:

;;  Required modules

(eval-when-compile  (require 'cl-lib))
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(eval-when-compile (require 'pipewire "pipewire" 'no-error ))

;;; Map Faces:




(voice-setup-add-map 
 '(
   (pipewire-default-object voice-smoothen)
   (pipewire-label voice-lighten)
   (pipewire-muted voice-brighten)
   (pipewire-volume voice-bolden)))

;;; Interactive Commands:

(defadvice pipewire (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-toggle-audio-indentation)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(cl-loop
 for f in 
 '(
   pipewire-decrease-volume pipewire-decrease-volume-single
   pipewire-set-volume pipewire-set-profile
   pipewire-increase-volume pipewire-increase-volume-single)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'button)))))

(provide 'emacspeak-pipewire)
;; end of file

;;; local variables:
;;; folded-file: t
;;; end:

