;;; emacspeak-bookmark.el --- Speech enable Emacs' builtin bookmarks
;;; $Id$
;;; $Author$ 
;;; Description: Auditory interface to bookmark
;;; Keywords: Emacspeak, Speak, Spoken Output, bookmark
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

;;; Copyright (c) 1995 -- 2003, T. V. Raman
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

;;{{{  Required modules

(require 'emacspeak-preamble)

;;}}}
;;{{{  Introduction 

;;; Speech enable bookmarks

;;}}}
;;{{{  bookmarks
(defadvice bookmark-set (after emacspeak pre act)
  "Announce yourself."
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (message "Set bookmark ")))

(defadvice bookmark-yank-word (after emacspeak pre act )
  "Speak what has been yanked so far"
  (when (interactive-p)
    (emacspeak-auditory-icon 'yank-object )
    (emacspeak-speak-line )))

(defadvice bookmark-insert-current-bookmark (after emacspeak pre act )
  "Speak what has been yanked so far"
  (when (interactive-p)
    (emacspeak-auditory-icon 'yank-object )
    (emacspeak-speak-line )))

(defadvice bookmark-insert-current-file-name (after emacspeak pre act )
  "Speak what has been yanked so far"
  (when (interactive-p)
    (emacspeak-auditory-icon 'yank-object )
    (emacspeak-speak-line )))

(defadvice  bookmark-jump (after emacspeak pre act)
  "Announce what happened."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice bookmark-bmenu-list (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (switch-to-buffer "*Bookmark List*")
    (emacspeak-speak-line)))

(defadvice bookmark-bmenu-this-window (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'open-object)))
(defadvice bookmark-bmenu-select (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))
(defadvice bookmark-bmenu-delete-backwards (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-speak-line)))

(defadvice bookmark-bmenu-1-window (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice bookmark-bmenu-2-window (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice bookmark-bmenu-switch-other-window (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice bookmark-bmenu-edit-annotation (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice bookmark-bmenu-delete (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-speak-line)))

(defadvice bookmark-bmenu-unmark (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'deselect-object)
    (emacspeak-speak-line)
    ))

(defadvice bookmark-bmenu-edit-annotation (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))
(defadvice bookmark-send-edited-annotation (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (emacspeak-speak-line)))

(defadvice bookmark-bmenu-show-annotation (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-other-window 1)))

(defadvice bookmark-bmenu-show-all-annotations (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Displayed all annotations in other window")))

(defadvice bookmark-bmenu-mark (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-speak-line)))

(defadvice bookmark-bmenu-quit  (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice bookmark-bmenu-backup-unmark (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'deselect-object)
    (emacspeak-speak-line)))

;;}}}
(provide 'emacspeak-bookmark)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
