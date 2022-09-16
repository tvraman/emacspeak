;;; emacspeak-ffap.el --- Speech-enable FFAP  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable FFAP An Emacs Interface to ffap
;;; Keywords: Emacspeak,  Audio Desktop ffap
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
;;; MERCHANTABILITY or FITNFFAP FOR A PARTICULAR PURPOSE.  See the
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
;;; FFAP ==  Find file at point and friends

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(eval-when-compile (require 'ffap))

;;}}}
;;{{{ Map Faces:

(voice-setup-add-map 
 '((ffap voice-bolden)))

;;}}}
;;{{{ Interactive Commands:

(cl-loop
 for f in 
 '(
   ffap ffap-alternate-file ffap-alternate-file-other-window ffap-at-mouse
   ffap-dired-other-frame ffap-dired-other-window
   ffap-list-directory ffap-literally
   ffap-next ffap-next-url
   ffap-other-frame ffap-other-tab ffap-other-window
   ffap-read-only ffap-read-only-other-frame
   ffap-read-only-other-tab ffap-read-only-other-window)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'open-object)
       (emacspeak-speak-mode-line)))))

;;}}}
(provide 'emacspeak-ffap)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
