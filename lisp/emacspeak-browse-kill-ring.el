;;; emacspeak-browse-kill-ring.el --- browse-kill-ring  for emacspeak desktop
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak front-end for BROWSE-KILL-RING
;;; Keywords: Emacspeak, browse-kill-ring
;;{{{  LCD Archive entry:
;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2006-08-19 10:48:45 -0700 (Sat, 19 Aug 2006) $ |
;;;  $Revision: 4074 $ |
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

;;; Browse the kill ring using 
;;;browse-kill-ring.el - interactively insert items from kill-ring (by Colin Walters)
;;;http://www.cis.ohio-state.edu/~walters/browse-kill-ring.el

;;}}}
;;{{{ required modules

;;; Code:
(require 'emacspeak-preamble)
;;}}}
;;{{{ speech-enable interactive commands

(defadvice browse-kill-ring-undo-other-window (after
                                               emacspeak pre
                                               act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'unmodified-object)))

(defadvice browse-kill-ring-insert (after
                                    emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'yank-object)))

(defadvice browse-kill-ring-delete (after
                                    emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)))
(defadvice browse-kill-ring-forward (after
                                     emacspeak pre
                                     act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defadvice browse-kill-ring-previous (after
                                      emacspeak pre
                                      act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defadvice browse-kill-ring-quit (after
                                  emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice browse-kill-ring-edit (after
                                  emacspeak pre
                                  act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)))
(defadvice browse-kill-ring-edit-finish (after
                                         emacspeak pre
                                         act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)))

(defadvice browse-kill-ring-occur (after
                                   emacspeak pre
                                   act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)))

(defadvice browse-kill-ring-update (after
                                    emacspeak pre
                                    act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)))
(defadvice browse-kill-ring (after
                             emacspeak pre
                             act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{ add keybinding on emacspeak desktop
(eval-when (load)
  (define-key emacspeak-keymap "\C-k" 'browse-kill-ring))
;;}}}
(provide 'emacspeak-browse-kill-ring)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

(defadvice browse-kill-ring-search-forward (after
                                            emacspeak pre
                                            act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defadvice browse-kill-ring-search-backward (after
                                             emacspeak pre
                                             act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

;;}}}
