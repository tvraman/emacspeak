;;; emacspeak-org.el --- Speech-enable org 
;;; $Id$
;;; $Author$
;;; Description:  Emacspeak front-end for ORG 
;;; Keywords: Emacspeak, org 
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

;;; Commentary:
;;{{{  Introduction:

;;; Speech-enable org ---
;;;  Org allows you to keep organized notes and todo lists.
;;; Homepage: http://www.astro.uva.nl/~dominik/Tools/org/

;;}}}
;;{{{ required modules

;;; Code:
(require 'emacspeak-preamble)
(require 'emacspeak-redefine)

;;}}}
;;{{{ Structure Navigation:

(loop for f in
      '(org-cycle
        org-goto  org-goto-ret
        org-goto-left org-goto-right
        org-goto-quit
        )
      do
      (eval
       `(defadvice ,f(after emacspeak pre act comp)
          "Provide spoken feedback."
          (when (interactive-p)
            (emacspeak-speak-line)
            (emacspeak-auditory-icon 'large-movement)))))

;;}}}
;;{{{ Header insertion and relocation

(loop for f in
      '(org-insert-heading org-insert-todo-heading
                           org-promote-subtree org-demote-subtree
                           org-do-promote org-do-demote
                           org-move-subtree-up org-move-subtree-down
                           )
      do
      (eval
       `(defadvice ,f(after emacspeak pre act comp)
          "Provide spoken feedback."
          (when (interactive-p)
            (emacspeak-speak-line)
            (emacspeak-auditory-icon 'open-object)))))

;;}}}
;;{{{ cut and paste:

(loop for f in
      '(org-copy-subtree org-paste-subtree
                         org-archive-subtree)
      do
      (eval
       `(defadvice ,f(after emacspeak pre act comp)
          "Provide spoken feedback."
          (when (interactive-p)
            (emacspeak-speak-line)
            (emacspeak-auditory-icon 'yank-object)))))

;;}}}
;;{{{ completion:

(defadvice org-complete (around emacspeak pre act)
  "Say what you completed."
  (let ((prior (save-excursion
                 (backward-word 1)
                 (point )))
        (dtk-stop-immediately t))
    ad-do-it
    (let ((completions-buffer (get-buffer "*Completions*")))
      (if (> (point) prior)
          (tts-with-punctuations 'all
                                 (dtk-speak (buffer-substring prior (point ))))
        (when (and completions-buffer
                   (window-live-p (get-buffer-window completions-buffer )))
          (save-excursion
            (set-buffer completions-buffer )
            (emacspeak-prepare-completions-buffer)
            (dtk-speak (buffer-string ))))))
    ad-return-value))

;;}}}
;;{{{ toggles:

;;}}}
;;{{{ ToDo:

;;}}}
;;{{{ timestamps and calendar:

;;}}}
;;{{{ Agenda:

;;}}}
;;{{{ misc file commands:

;;}}}
;;{{{ Links:

;;}}}
;;{{{ tables:

;;}}}
;;{{{ table minor mode:

;;}}}
;;{{{ import/export:

;;}}}
;;{{{ Meta Navigators:

;;}}}
(provide 'emacspeak-org)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
