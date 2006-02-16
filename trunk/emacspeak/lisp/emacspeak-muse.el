;;; emacspeak-muse.el --- Speech-enable Muse
;;; $Id$
;;; $Author$
;;; Description:  Speech-enable Muse
;;; Keywords: Emacspeak,  Audio Desktop Muse
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
;;;Copyright (C) 1995 -- 2004, T. V. Raman 
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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; Speech enable Muse

;;}}}
;;{{{  Required modules

;;; Code:

(require 'emacspeak-preamble)
(require 'browse-url)
(require 'emacspeak-outline)

;;}}}
;;{{{ Voice definitions:

(def-voice-font emacspeak-muse-emphasis-1-personality voice-bolden
  'muse-emphasis-1 
  "Personality used for Muse emphasis.")

(def-voice-font emacspeak-muse-emphasis-2-personality voice-bolden-medium
  'muse-emphasis-2
  "Personality used for Muse emphasis.")

(def-voice-font emacspeak-muse-emphasis-3-personality voice-bolden-extra
  'muse-emphasis-3
  "Personality used for Muse emphasis.")

(def-voice-font emacspeak-muse-link-personality voice-brighten
  'muse-link-face
  "Personality used for Muse links.")

(def-voice-font emacspeak-muse-bad-link-personality voice-animate
  'muse-bad-link-face
  "Personality for bad muse links.")

(def-voice-font  emacspeak-muse-verbatim-personality
  voice-monotone
  'muse-verbatim-face
  "Personality for verbatim text in Muse.")
(def-voice-font emacspeak-muse-header-1 emacspeak-outline-1
  'muse-header-1
  "Header personality in Muse.")

(def-voice-font emacspeak-muse-header-2 emacspeak-outline-2
  'muse-header-2
  "Header personality in Muse.")
(def-voice-font emacspeak-muse-header-3 emacspeak-outline-3
  'muse-header-3
  "Header personality in Muse.")
      
(def-voice-font emacspeak-muse-header-4 emacspeak-outline-4
  'muse-header-4
  "Header personality in Muse.")

(def-voice-font emacspeak-muse-header-5 emacspeak-outline-5
  'muse-header-5
  "Header personality in Muse.")

;;}}}
;;{{{ advice interactive commands
(loop for f in
      '(muse-follow-name-at-point
        muse-follow-name-at-point-other-window
        muse-next-reference
        muse-previous-reference)
      do
      (eval
       `(defadvice   ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (interactive-p)
            (emacspeak-auditory-icon 'large-movement)
            (emacspeak-speak-line)))))

;;}}}
(provide 'emacspeak-muse)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
