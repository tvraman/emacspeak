;;; emacspeak-wdired.el --- Speech-enable wdired
;;; $Id$
;;; $Author$
;;; Description:  Emacspeak extension to speech-enable WDIRED
;;; Keywords: Emacspeak, Multimedia
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

;;; Copyright (C) 1995--2004 T. V. Raman <raman@cs.cornell.edu>
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

;;{{{  Introduction

;;; Commentary:
;;; Speech-enable wdired to permit in-place renaming of groups of files.

;;}}}
;;{{{ required modules

(require 'emacspeak-preamble)
(require 'emacspeak-dired)

;;}}}
;;{{{ Advice interactive commands.

(loop for c in
      '(wdired-next-line wdired-previous-line)
      do
      (eval
       `(defadvice ,c (after emacspeak pre act comp)
          "Provide spoken feedback."
          (when (interactive-p)
            (emacspeak-auditory-icon 'select-object)
            (emacspeak-dired-speak-line)))))

(defadvice wdired-upcase-word (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (interactive-p)
    (tts-with-punctuations 'some
                           (dtk-speak "upper cased file name. "))))
(defadvice wdired-capitalize-word (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (interactive-p)
    (tts-with-punctuations 'some
                           (dtk-speak "Capitalized file name. "))))
(defadvice wdired-downcase-word (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (interactive-p)
    (tts-with-punctuations 'some
                           (dtk-speak "Down cased file
  name. "))))

(defadvice wdired-toggle-bit (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'button)
    (dtk-speak "Toggled permission bit.")))

(defadvice wdired-abort-changes (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (tts-with-punctuations 'some
                           (dtk-speak "Cancelling  changes. "))))

(defadvice wdired-finish-edit (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'save-object)
    (tts-with-punctuations 'some
                           (dtk-speak "Committed changes. "))))

(defadvice wdired-change-to-wdired-mode (after emacspeak pre act
                                               comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (tts-with-punctuations 'some
                           (dtk-speak "Entering writeable dir ed mode. "))))

;;}}}

(provide 'emacspeak-wdired)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
