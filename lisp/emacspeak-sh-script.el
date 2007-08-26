;;; emacspeak-sh-script.el --- Speech enable  sh-script mode
;;; $Id$
;;; $Author$
;;; Description:   extension to speech enable sh-script 
;;; Keywords: Emacspeak, Audio Desktop
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1995 -- 2007, T. V. Raman<raman@cs.cornell.edu>
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

;;{{{ required modules

(require 'emacspeak-preamble)
;;}}}
;;{{{  Introduction:

;;; Commentary:

;;; This module speech-enables sh-script.el 

;;; Code:

;;}}}
;;{{{  advice interactive commands

(defadvice sh-mode (after emacspeak pre act comp)
  "Speech-enable sh-script editting."
  (voice-lock-mode 1)
  (dtk-set-punctuations 'all)
  (unless emacspeak-audio-indentation
    (emacspeak-toggle-audio-indentation))
  (emacspeak-speak-mode-line))

(defun emacspeak-sh-script-voice-lock-setup()
  "Setup voice locking."
  (voice-lock-mode 1))

(defadvice sh-indent-line (after emacspeak pre act comp)
  "Provide auditory feedback to indicate indentation."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-current-column)))
(defadvice sh-assignment (after emacspeak pre act comp)
  "Speak assignment as it is inserted."
  (when (interactive-p)
    (emacspeak-speak-this-char (preceding-char))))

(defadvice sh-maybe-here-document(around emacspeak pre act comp)
  "Spoken feedback based on what we insert."
  (cond
   ((interactive-p)
    (let ((start (point)))
      ad-do-it
      (if (= (point) (1+ start))
          (emacspeak-speak-this-char last-input-char)
        (message "Started a shell here  document."))))
   (t ad-do-it))
  ad-return-value)
(defadvice sh-newline-and-indent (after emacspeak pre act comp)
  "Provide auditory feedback to indicate indentation."
  (when (interactive-p)
    (emacspeak-speak-line)))
(defadvice sh-beginning-of-command(after emacspeak pre act
                                         comp)
  "Speak point moved to."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))
(defadvice sh-end-of-command(after emacspeak pre act
                                   comp)
  "Speak point moved to."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

;;}}}
;;{{{ advice skeleton insertion 
(defadvice skeleton-pair-insert-maybe(around emacspeak pre
                                             act comp)
  "Speak what you inserted."
  (cond
   ((interactive-p)
    (let ((orig (point)))
      ad-do-it
      (emacspeak-speak-region orig (point))))
   (t ad-do-it))
  ad-return-value)

;;}}}
(provide 'emacspeak-sh-script)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
