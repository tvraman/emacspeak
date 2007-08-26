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
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2007, T. V. Raman 
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
(voice-setup-add-map
 '(
   (muse-bad-link-face voice-bolden-and-animate)
   (muse-emphasis-1 voice-lighten)
   (muse-emphasis-2 voice-lighten-medium)
   (muse-emphasis-3 voice-lighten-extra)
   (muse-header-1 voice-bolden)
   (muse-header-2 voice-bolden-medium)
   (muse-header-3 voice-bolden-extra)
   (muse-header-4 voice-bolden-extra)
   (muse-header-5 voice-bolden-extra)
   (muse-link-face voice-brighten)
   (muse-verbatim-face voice-monotone)
   ))

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
