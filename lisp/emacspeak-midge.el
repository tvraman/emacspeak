;;; emacspeak-midge.el --- Speech-enable MIDI editor  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak extension to speech-enable MIDGE
;;; Keywords: Emacspeak, MIDI 
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2007-08-25 18:28:19 -0700 (Sat, 25 Aug 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1995 -- 2018, T. V. Raman<tv.raman.tv@gmail.com>
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

(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
;;}}}
;;{{{  Introduction:

;;; Commentary:

;;; This module speech enables  midge.
;;; Midge is a MIDI composer/editor tool.
;;;From the package README file:
                                        ; Midge, for midi generator, is a text to midi translator.
                                        ; It creates type 1 (ie multitrack) midi files from text
                                        ; descriptions of music. It is a single perl script, which
                                        ; does not require any additional modules.
;;;The package also provides a convenient emacs mode for
;;;editing and playing  midge files.
;;;Midge's homepage is at:
;;; http://www.dmriley.demon.co.uk/code/midge/ 

;;; Code:

;;}}}
;;{{{ Speech enable interactive commands.

(defadvice midge-indent-line(after emacspeak pre act comp)
  "Speak line after indenting it."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice midge-close-bracket(after emacspeak pre act comp)
  "Speak closing delimiter we inserted"
  (when (ems-interactive-p)
    (emacspeak-speak-this-char last-input-event)))

(defadvice midge-head-block(after emacspeak pre act comp)
  "Announce insertion of head block"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Started head section")))

(defadvice midge-body-block(after emacspeak pre act comp)
  "Announce insertion of body block"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Started body section")))

(defadvice midge-repeat-block(after emacspeak pre act comp)
  "Announce insertion of repeat block"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Started repeat block")))

(defadvice midge-choose-block(after emacspeak pre act comp)
  "Announce insertion of choose block"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Started choose block")))

(defadvice midge-bend-block(after emacspeak pre act comp)
  "Announce insertion of bend block"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Started bend block")))

(defadvice midge-define-block(after emacspeak pre act comp)
  "Announce insertion of define block"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Started define block")))

(defadvice midge-repeat-line(after emacspeak pre act comp)
  "Announce insertion of repeat block"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice midge-bend-line(after emacspeak pre act comp)
  "Announce insertion of bend block"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice midge-define-line(after emacspeak pre act comp)
  "Announce insertion of define block"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice midge-choose-line(after emacspeak pre act comp)
  "Announce insertion of choose block"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice midge-compile(after emacspeak pre act comp)
  "Produce auditory icon."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-message-again)))

(defadvice midge-compile-debug(after emacspeak pre act comp)
  "Produce auditory icon."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice midge-compile-verbose(after emacspeak pre act comp)
  "Produce auditory icon."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice midge-compile-ask(after emacspeak pre act comp)
  "Produce auditory icon."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)))

;;}}}
;;{{{ midge-mode-hook

(defvar midge-mode-hook nil
  "midge setup hook")

(defadvice midge-mode (after emacspeak pre act comp)
  "Run midge-mode-hook"
  (run-hooks 'midge-mode-hook))

;;}}}

(provide 'emacspeak-midge)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
