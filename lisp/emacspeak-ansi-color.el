;;; emacspeak-ansi-color.el --- Speech-enable ansi-color terminal
;;; $Id$
;;; $Author$
;;; Description:  Emacspeak module for ansi-color
;;; Keywords: Emacspeak, ansi-color
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision: 4502 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1999 T. V. Raman <raman@cs.cornell.edu>
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

;;{{{  Introduction:

;;; Commentary:

;;; Module ansi-color (bundled with Emacs 21)
;;; handles ansi escape sequences and turns them into
;;; appropriate faces.
;;;This is useful in things like shell buffers.
;;; This module maps ansi codes to the appropriate voices.

;;}}}
;;{{{ required modules

;;; Code:
(require 'emacspeak-preamble)
;;}}}
;;{{{ color to voice

(defun emacspeak-ansi-color-to-voice (face-spec)
  "Return a voice corresponding to specified face-spec."
  (declare (special ansi-color-names-vector
                    ansi-color-faces-vector))
  (let* ((voice-name nil)
         (style (cadr face-spec))
         (style-index (position style ansi-color-faces-vector))
         (color (cdr (assq 'foreground-color  face-spec)))
         (color-index
          (when color
            (position  color ansi-color-names-vector
                       :test #'string-equal)))
         (style nil)
         (color-parameter nil)
         (style-parameter nil))
    (setq voice-name
          (intern (format "emacspeak-ansi-color-%s-%s"
                          (if color color "default")
                          (if style style "default"))))
    (unless (tts-voice-defined-p voice-name)
      (setq style (make-acss ))
      (setq style-parameter
            (if style-index
                (+ 1 style-index)
              1))
      (setq color-parameter
            (if color-index
                (+ 1 color-index)
              1))
      (setf (acss-average-pitch style) color-parameter)
      (setf (acss-pitch-range style) color-parameter)
      (setf (acss-richness style) color-parameter)
      (setf (acss-stress style) color-parameter)
      (tts-define-voice-from-speech-style voice-name style))
    voice-name))

(defadvice ansi-color-set-extent-face (after emacspeak pre act comp)
  "Apply aural properties."
  (let* ((extent (ad-get-arg 0))
         (face (ad-get-arg 1))
         (start (overlay-start extent))
         (end (overlay-end extent))
         (voice (when (listp face)
                  (emacspeak-ansi-color-to-voice face))))
    (when voice
      (ems-modify-buffer-safely
       (put-text-property start end
                          'personality voice)))))

;;}}}
;;{{{ advice interactive commands

(defadvice ansi-color-for-comint-mode-on (after emacspeak
                                                pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'on)
    (message "Ansi escape sequences will be processed.")))

(defadvice ansi-color-for-comint-mode-off (after emacspeak
                                                 pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'off)
    (message "Ansi escape sequences will not be processed.")))

;;}}}
(provide 'emacspeak-ansi-color)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
