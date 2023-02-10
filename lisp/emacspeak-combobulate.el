;;; emacspeak-combobulate.el --- Speech-enable COMBOBULATE  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable COMBOBULATE An Emacs Interface to combobulate
;;; Keywords: Emacspeak,  Audio Desktop combobulate
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2007, 2019, T. V. Raman
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
;;; MERCHANTABILITY or FITNCOMBOBULATE FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; COMBOBULATE: Navigate, Manipulate code with  tree-sitter concrete-tree;

;;; Code:

;;}}}
;;{{{  Required modules

(eval-when-compile  (require 'cl-lib))
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Map Faces:

(voice-setup-add-map 
'(
(combobulate-active-indicator-face voice-animategs)
(combobulate-dimmed-indicator-face voice-bolden)
(combobulate-refactor-highlight-face voice-annotate)
(combobulate-tree-branch-face voice-lighten)
(combobulate-tree-highlighted-node-face voice-brighten)
(combobulate-tree-pulse-node-face voice-smoothen)))

;;}}}
;;{{{ Interactive Commands:

'(
combobulate-clone-node-dwim
combobulate-drag-down
combobulate-drag-up
combobulate-edit-cluster-dwim
combobulate-envelop
combobulate-envelop-node
combobulate-envelop-python-ts-mode-decorate
combobulate-envelop-python-ts-mode-nest-for
combobulate-envelop-python-ts-mode-nest-if
combobulate-envelop-python-ts-mode-nest-while
combobulate-envelop-python-ts-mode-wrap-parentheses
combobulate-kill-node-dwim
combobulate-mark-defun
combobulate-mark-node-dwim
combobulate-maybe-auto-close-tag
combobulate-maybe-close-tag-or-self-insert
combobulate-maybe-insert-attribute
combobulate-python-indent-for-tab-command
combobulate-splice-down
combobulate-splice-up
combobulate-transpose-sexps
combobulate-vanish-node
)


(defun emacspeak-combobulate-speak-line ()
  "Speak"
  (let ((emacspeak-show-point  t))
    (emacspeak-speak-line)))

(cl-loop
 for f in 
 '(
   combobulate-navigate-backward combobulate-navigate-beginning-of-defun
   combobulate-navigate-down combobulate-navigate-down-list-maybe
   combobulate-navigate-end-of-defun combobulate-navigate-forward
   combobulate-navigate-logical-next combobulate-navigate-logical-previous
   combobulate-navigate-next combobulate-navigate-previous
   combobulate-navigate-up combobulate-navigate-up-list-maybe)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'large-movement)
       (emacspeak-combobulate-speak-line)))))

;;}}}
(provide 'emacspeak-combobulate)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
