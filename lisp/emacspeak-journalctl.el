;;; emacspeak-journalctl.el --- JOURNALCTL  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable JOURNALCTL An Emacs Interface to journalctl
;;; Keywords: Emacspeak,  Audio Desktop journalctl
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
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
;;; MERCHANTABILITY or FITNJOURNALCTL FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; JOURNALCTL ==  SystemD Journal From emacs
;; See https://github.com/SebastianMeisel/journalctl-mode 

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Map Faces:

(voice-setup-add-map
 '
(
 (journalctl-error-face voice-animate)
 (journalctl-finished-face voice-lighten)
 (journalctl-host-face voice-smoothen)
 (journalctl-process-face voice-monotone)
 (journalctl-starting-face voice-lighten)
 (journalctl-timestamp-face voice-monotone)
 (journalctl-warning-face voice-animate)))

;;}}}
;;{{{ Interactive Commands:

(cl-loop
 for f in 
 '(journalctl-boot journalctl
journalctl-unit journalctl-user-unit)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))))




(cl-loop
 for f in 
 '(
   journalctl-scroll-up journalctl-scroll-down
   journalctl-previous-chunk journalctl-next-chunkfunctions)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'scroll)
       (emacspeak-speak-line)))))

;;}}}
(provide 'emacspeak-journalctl)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
