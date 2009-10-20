;;; emacspeak-twit.el --- Speech-enable Twitter
;;; $Id: emacspeak-twit.el 6133 2009-03-17 02:36:43Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable twit.el and twitter.el --- Twitter from Emacs
;;; Keywords: Emacspeak,  Audio Desktop Twiter
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
;;;Copyright (C) 1995 -- 2009, T. V. Raman
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
;;; MERCHANTABILITY or FITNTWIT FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; Locate modules twit.el and twitter.el from emacs Wiki.
;;; This module speech-enables these for Emacs
;;; Primarily defines face->voice mappings.

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Map->Voice Mappings:
(voice-setup-add-map
 '(
   (twit-message-face voice-lighten)
   (twit-author-face voice-brighten)
   (twit-info-face voice-smoothen)
   (twit-title-face voice-bolden)
   (twit-zebra-1-face voice-smoothen-extra)
   (twit-zebra-2-face voice-lighten)
   (twit-error-face voice-lighten-extra)
   (twit-fail-whale-face voice-lighten-extra)
   ))
;;}}}
;;{{{ Advice interactive commands: twit.el Version 3 

(loop for command in
      '(twit-next-tweet twit-previous-tweet)
      do
      (eval
       `(defadvice ,command (after emacspeak pre act comp)
          "Provide spoken feedback."
          (when (interactive-p)
            (emacspeak-auditory-icon 'select-object)
            (emacspeak-speak-line)))))

;;}}}
;;{{{ turn on voice lock:
;;; no minor mode hook for now alas:

(defadvice twit-write-recent-tweets (after emacspeak pre act comp)
  "Turn on voice lock."
  (voice-lock-mode 1))

;;}}}
(provide 'emacspeak-twit)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
