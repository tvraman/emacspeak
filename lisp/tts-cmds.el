;;; tts-cmds.el --- Engine-specific TTS Commands  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  TTS Commands Implements Using Module tts.el
;;; Keywords: Emacspeak,  Audio Desktop tts
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
;;;Copyright (C) 1995 -- 2007, 2011, T. V. Raman
;;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
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
;;; MERCHANTABILITY or FITNTTS-ENV FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary: 

;;; End-user TTS Commands implemented using tts-state. If this works
;;; out, these will eventually replace the commands implemented in
;;; dtk-speak.el

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'tts)
;;}}}
;;{{{ Setters:

(defun tts-cmd-set-rate (rate    &optional prefix)
  "Set speech rate."
  (interactive "nRate:\nP")
  (cl-declare (special  dtk-speaker-process dtk-speak-server-initialized))
  (when prefix
    (setf (tts-state-rate tts-state-prototype) rate))
  (when dtk-speak-server-initialized
    (tts-set-rate rate)
    (setq dtk-speech-rate rate)
    (dtk-interp-set-rate rate)
    (when (called-interactively-p t)
      (message "Set speech rate to %s %s"
               rate
               (if prefix "" "locally")))))

;;}}}
(provide 'tts-cmds)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
