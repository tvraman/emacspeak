;;; emacspeak-kotl.el --- Speech enable KOtl -- Hyperbole's outlining editor  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author$ 
;;; Description:  Emacspeak extensions for Bob Weiner's excellent Outliner
;;; Keywords: Emacspeak, Speech Access, Hyperbole, Outliner
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

;;{{{  Introduction:

;;; Provide Emacspeak  advice to
;;; the outliner that comes with the hyperbole system

;;; We first advice the redefined functions e.g.
;;; kotl-mode:next-line etc.
;;; We take care to avoid an advice/elisp bug
;;; by taking care to set the advice info for
;;; the new functions before advising them.
;;; Then we advice the  new functions that are specific to kotl, e.g. moving
;;; cells etc.

;;}}}
;;{{{ requires
(require 'emacspeak-preamble)

;;}}}
;;{{{  Advice the various redefined functions

;;{{{  advice cursor movement commands to speak

;;; This section contains the same advice present in emacspeak-advice.el
;;; kotl does not redefine all the functions advised there.


(defadvice kotl-mode:next-line (after emacspeak pre act)
  "Speak line that you just moved to. "
  (when (ems-interactive-p)
    (emacspeak-speak-line  )))

(defadvice kotl-mode:previous-line (after emacspeak pre act)
  "Speak line that you just moved to. "
  (when (ems-interactive-p) (emacspeak-speak-line  )))

(defadvice kotl-mode:forward-word (after emacspeak pre act)
  "Speak the word you just moved to. "
  (when (ems-interactive-p)
    (skip-syntax-forward  " ")
    (emacspeak-speak-word )))

(defadvice kotl-mode:backward-word (after emacspeak pre act)
  "Speak the word you just moved to. "
  (when (ems-interactive-p) (emacspeak-speak-word )))


(defadvice kotl-mode:beginning-of-buffer (after emacspeak pre act)
  "Speak the line. "
  (when (ems-interactive-p) (emacspeak-speak-line  )))

(defadvice kotl-mode:end-of-buffer (after emacspeak pre act)
  "Speak the line. "
  (when (ems-interactive-p) (emacspeak-speak-line   )))

(defadvice kotl-mode:back-to-indentation (after emacspeak pre act)
  "Speak the entire line. "
  (when (ems-interactive-p) (emacspeak-speak-line  )))

(defadvice kotl-mode:forward-sentence (after emacspeak pre act)
  "Speak  sentence  after moving. "
  (when (ems-interactive-p) (emacspeak-speak-sentence    )))

(defadvice kotl-mode:backward-sentence (after emacspeak pre act)
  "Speak  sentence  after moving. "
  (when (ems-interactive-p) (emacspeak-speak-sentence    )))


(defadvice kotl-mode:forward-paragraph (after emacspeak pre act )
  "Speak the paragraph. "
  (when(ems-interactive-p)
    (emacspeak-speak-paragraph))
  )

(defadvice kotl-mode:backward-paragraph (after emacspeak pre act )
  "Speak the paragraph. "
  (when(ems-interactive-p) 
    (emacspeak-speak-paragraph  nil )))

(defadvice kotl-mode:scroll-up (after emacspeak pre act)
  "Speak the screenful"
  (when (ems-interactive-p)
    (let ((start (point )))
      (emacspeak-auditory-icon 'scroll)
      (save-excursion
        (forward-line (window-height))
        (emacspeak-speak-region start (point ))))))

(defadvice kotl-mode:scroll-down (after emacspeak pre act)
  "Speak the screenful"
  (when (ems-interactive-p)
    (let ((start (point )))
      (emacspeak-auditory-icon 'scroll)
      (save-excursion
        (forward-line (- (window-height )))
        (emacspeak-speak-region start (point ))))))


(defadvice kotl-mode:beginning-of-line (after emacspeak pre act )
  "Stop speech after moving"
  (when (ems-interactive-p)
    (dtk-stop )))


(defadvice kotl-mode:end-of-line (after emacspeak pre act )
  "Stop speech after moving"
  (when (ems-interactive-p)
    (dtk-stop )))

;;}}}
;;{{{  Killing, deleting, and yanking:


(defadvice kotl-mode:kill-line(before emacspeak pre act)
  "Speak line before killing it. " 
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-speak-line 1)
    (dtk-tone 500 30)))

(defadvice kotl-mode:kill-word (before emacspeak pre act )
  "Speak word before killing it. "
  (when (ems-interactive-p )
    (save-excursion
      (skip-syntax-forward " ")
      (emacspeak-speak-word 1 ))
    (dtk-tone 500 30)))

(defadvice kotl-mode:backward-kill-word (before emacspeak pre act)
  "Speak word before killing it. "
  (when (ems-interactive-p )
    (let ((start (point )))
      (save-excursion
        (forward-word -1)
        (emacspeak-speak-region (point) start )
        )
      (dtk-tone 500 30))))


(defadvice kotl-mode:kill-sentence (before emacspeak pre act )
  "Speak the line  you killed. "
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-speak-line 1 )
    (dtk-tone 500 30)))

(defadvice kotl-mode:kill-ring-save (after emacspeak pre act)
  "Indicate that region has been copied to the kill ring.
  Produce an auditory icon if possible."
  (when (ems-interactive-p ) 
    (emacspeak-auditory-icon 'mark-object )
    (message "region copied to kill ring ")))


(defadvice kotl-mode:yank (after emacspeak pre act)
  "Say what you yanked.
   Produce an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'yank-object )
    (emacspeak-speak-region (mark) (point))))


(defadvice kotl-mode:yank-pop (after emacspeak pre act)
  "Say what you yanked.
  Also produce an auditory icon if possible. "
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-speak-region (point) (mark))))


(defadvice kotl-mode:open-line (after emacspeak pre act )
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (let ((count (ad-get-arg 0)))
      (emacspeak-auditory-icon 'open-object)
      (message "Opened %s blank line%s"
               (if (= count 1) "a" count)
               (if (= count 1 ) "" "s")))))

(defadvice kotl-mode:delete-char (before emacspeak pre act)
  "Speak character you're deleting. "
  (when (ems-interactive-p )
    (emacspeak-speak-char t)
    (dtk-tone 500 30)))

(defadvice kotl-mode:delete-backward-char (before emacspeak pre act)
  "Speak character you're deleting. " 
  (when (ems-interactive-p )
    (emacspeak-speak-this-char (preceding-char ))
    (dtk-tone 500 30)))

(defadvice kotl-mode:transpose-chars (after emacspeak pre act )
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-speak-char  t)))

(defadvice kotl-mode:transpose-lines (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'yank-object )
    (emacspeak-speak-line )))

(defadvice kotl-mode:transpose-words (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'yank-object )
    (emacspeak-speak-word )))


(defadvice kotl-mode:transpose-sexps (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'yank-object )
    (emacspeak-speak-sexp )))

(defadvice kotl-mode:open-line (after emacspeak pre act )
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (let ((count (ad-get-arg 0)))
      (emacspeak-auditory-icon 'open-object)
      (message "Opened %s blank line%s"
               (if (= count 1) "a" count)
               (if (= count 1 ) "" "s")))))

;;}}}
;;{{{  misc functions:


(defadvice kotl-mode:mark-whole-buffer (after emacspeak pre act)
  "Produce an auditory icon if possible. "
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'mark-object)))


(defadvice kotl-mode:mark-paragraph(after emacspeak pre act)
  "Produce an auditory icon if possible. "
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'mark-object)))

;;}}}
;;{{{  fix interactive prompts of redefined functions:

(defadvice kotl-mode:copy-to-register (before emacspeak pre act)
  "Acknowledge the copy"
  (when (ems-interactive-p)
    (let ((start (ad-get-arg 1))
          (end (ad-get-arg 2 ))
          (register (ad-get-arg 0)))
      (message "Copied %s characters to register %c"
               (abs (- start end ))

               register ))))

;;}}}
;;{{{ filling and centering


(defadvice kotl-mode:center-line (after emacspeak pre act)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (message"Centered current line")))


(defadvice kotl-mode:center-paragraph (after emacspeak pre act)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (message"Centered current paragraph")))

(defadvice kotl-mode:fill-paragraph (after emacspeak pre act)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'fill-object )
    (message "Filled current paragraph")))

;;}}}

;;}}}
;;{{{  Moving between cells

(defadvice kotl-mode:up-level (after emacspeak pre act)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice kotl-mode:down-level (after emacspeak pre act)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))
(defadvice kotl-mode:forward-cell (after emacspeak pre act)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))
(defadvice kotl-mode:backward-cell (after emacspeak pre act)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))
(defadvice kotl-mode:next-cell (after emacspeak pre act)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice kotl-mode:previous-cell (after emacspeak pre act)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice kotl-mode:goto-cell (after emacspeak pre act)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice kotl-mode:beginning-of-tree(after emacspeak pre act)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice kotl-mode:end-of-tree(after emacspeak pre act)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice kotl-mode:first-sibling(after emacspeak pre act)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice kotl-mode:last-sibling(after emacspeak pre act)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice kotl-mode:beginning-of-cell(after emacspeak pre act)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice kotl-mode:end-of-cell(after emacspeak pre act)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

;;}}}
;;{{{  Manipulating cells and their content

(defadvice kotl-mode:demote-tree (after emacspeak pre act )
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-speak-line )))

(defadvice kotl-mode:promote-tree (after emacspeak pre act )
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-speak-line )))

(defadvice kotl-mode:transpose-cells (after emacspeak pre act )
  "Provie auditory feedback"
  (when (ems-interactive-p )
    (emacspeak-speak-line )))

(defadvice kotl-mode:split-cell (after emacspeak pre act )
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-speak-line)))

(defadvice kotl-mode:kill-contents (after emacspeak pre act)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (message "Killed cell contents")))

(defadvice kotl-mode:kill-tree (after emacspeak pre act)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (message "Killed current tree")))

;;}}}
;;{{{ filling and centering:

(defadvice kotl-mode:fill-cell(after emacspeak pre act)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'fill-object )
    (message "Filled current paragraph")))

(defadvice kotl-mode:fill-tree(after emacspeak pre act)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'fill-object )
    (message "Filled current paragraph")))

;;}}}
;;{{{ Hiding and Exposing cells

;;; Since some of these are rewrites of functions in outline.el
;;; we take the same precaution of first setting advice info to nil

(defadvice kotl-mode:hide-tree (after emacspeak pre act )
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-line)))
(defadvice kotl-mode:show-tree (after emacspeak pre act )
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))


(defadvice kotl-mode:show-all (after emacspeak pre act comp)
  "Produce an auditory icon"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Exposed all text in the buffer")))

(defadvice kotl-mode:hide-sublevels (after emacspeak pre act comp)
  "Produce an auditory icon"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Hid everything except the top  %s levels"
             (ad-get-arg 0))))

(defadvice kotl-mode:hide-subtree (after emacspeak pre act comp)
  "Produce an auditory icon"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Hid everything at deeper levels from current heading")))

(defadvice kotl-mode:top-cells (after emacspeak pre act)
  "provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Showing only top-level headings")))

;;}}}
;;{{{ Adding  and moving cells 

(defadvice kotl-mode:add-parent (after emacspeak pre act )
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon'yank-object)
    (message "Added sibling to parent of current cell")))

(defadvice kotl-mode:add-cell (after emacspeak pre act )
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-speak-line )))

(defadvice kotl-mode:add-child (after emacspeak pre act )
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-speak-line )))

(defadvice kotl-mode:move-before (after emacspeak pre act)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-speak-line )))

(defadvice kotl-mode:move-after (after emacspeak pre act)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-speak-line )))

(defadvice kotl-mode:exchange-cells (after emacspeak pre act )
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-speak-line )))
(defadvice kotl-mode:copy-before (after emacspeak pre act)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-speak-line )))

(defadvice kotl-mode:copy-after (after emacspeak pre act)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-speak-line )))

(defadvice kotl-mode:mail-tree(after emacspeak pre act )
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line )))

(defadvice kotl-mode:copy-to-buffer (after emacspeak pre act )
  "provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'yank-object)
    (message "Copied  tree %s to buffer %s"
             (ad-get-arg 0)
             (ad-get-arg 1 ))))

;;}}}
;;{{{  emacspeak commands:

(defun emacspeak-kotl-speak-cell (arg)
  "Speak cell contents from point to end of cell.
  With prefix arg, speaks entire cell contents"
  (interactive "P")
  (emacspeak-speak-region
   (if arg (kcell-view:start) (point))
   (kcell-view:end-contents)))

;;}}}
;;{{{ Extra keybindings:

(defun emacspeak-kotl-setup-keys ()
  "Setup additional keybindings"
  (interactive)
  (declare (special kotl-mode-map))
  (define-key kotl-mode-map "\C-c " 'emacspeak-kotl-speak-cell)
  (define-key kotl-mode-map '[003  left] 'kotl-mode:backward-cell)
  (define-key kotl-mode-map '[003 right] 'kotl-mode:forward-cell)
  (define-key kotl-mode-map '[003 up] 'kotl-mode:previous-cell)
  (define-key kotl-mode-map '[003 down] 'kotl-mode:next-cell)
  )

(defadvice kotl-mode (after emacspeak pre act )
  "Setup emacspeak keys"
  (emacspeak-kotl-setup-keys))

;;}}}
(provide  'emacspeak-kotl)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
