;;; emacspeak-tab-bar.el --- Speech-enable tab-bar  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable tab-bar An Emacs Interface to tab-bar
;;; Keywords: Emacspeak,  Audio Desktop tab-bar
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
;;; MERCHANTABILITY or FITNtab-bar FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; tab-bar ==  tabs for window configuration.
;;; Speech-enable tab-bar interaction.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Map Faces:

(voice-setup-add-map 
'(
  (tab-bar voice-bolden)
  (tab-bar-tab voice-highlight)
  (tab-bar-tab-inactive voice-smoothen)
  (tab-line voice-lighten)
  )
)

;;}}}
;;{{{ Interactive Commands:

'(
tab-bar-list
tab-bar-list-backup-unmark
tab-bar-list-delete
tab-bar-list-delete-backwards
tab-bar-list-execute
tab-bar-list-mode
tab-bar-list-mouse-select
tab-bar-list-next-line
tab-bar-list-prev-line
tab-bar-list-select
tab-bar-list-unmark
tab-bar-mode
tab-bar-new-tab
tab-bar-select-tab
tab-bar-switch-to-next-tab
tab-bar-switch-to-prev-tab
tab-bar-switch-to-tab

tab-list
tab-new
tab-next
tab-previous
tab-select
)

(cl-loop
 for f in 
 '(
   tab-bar-close-other-tabs tab-bar-close-tab
   tab-close tab-close-other)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'close-object)
       (emacspeak-speak-mode-line)))))


(defadvice tab-bar-close-tab-by-name (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (dtk-speak (message "Closed tab %s" (ad-get-arg  0)))
    (emacspeak-auditory-icon 'close-object)))

;;}}}
(provide 'emacspeak-tab-bar)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
