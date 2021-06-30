;;; emacspeak-org-drill.el --- Speech-enable org-drill  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: Bart Bunting $
;;; Description:  Emacspeak front-end for ORG drill
;;; Keywords: Emacspeak, org org-drill
;;{{{  Copyright:

;;; Copyright (C) 2015, Bart Bunting
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
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak-org-drill| Bart Bunting |bart@bunting.net.au
;;; A speech interface to org-drill |
;;; $Date: 2015-03-17 09:26:40 -0700 (Sat, 22 Mar 2008) $ |
;;;  $Revision:  $ |
;;; Location undetermined
;;;

;;}}}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  Introduction:

;;; Commentary:
;;; Speech-enable org-drill ---

;;; Org-Drill is an extension for Org
;;; mode(http://orgmode.org/). Org-Drill uses a [spaced
;;; repetition](http://en.wikipedia.org/wiki/Spaced_repetition)
;;; algorithm to conduct interactive "drill sessions", using org files
;;; as sources of facts to be memorised. Each topic is treated as a
;;; "flash card". The material to be remembered is presented to the
;;; student in random order. The student rates his or her recall of
;;; each item, and this information is used to schedule the item for
;;; later revision.
;;;
;;; Code:

;;}}}
;;{{{ required modules

(require 'org-drill)

;;}}}

(defadvice org-drill-presentation-prompt (around emacspeak pre act comp)
  "Read out the previously saved question and Silence messages while this function executes.  Otherwise emacspeak will read out the timer as it ticks up."
  (emacspeak-auditory-icon 'ask-question)
  (let ((emacspeak-speak-messages nil))
    (emacspeak-org-drill-log "org-drill-presentation-prompt start")
    ; Speak the current question collected earlier prior to calling org-drill-presentation-prompt.
    (dtk-speak emacspeak-org-drill-current-question)
    ad-do-it
    (emacspeak-org-drill-log "org-drill-presentation-prompt end")))

(defadvice org-drill-present-simple-card (around emacspeak pre act comp)
  "Save the question away in a buffer and variable for use later"
  (let ((emacspeak-speak-messages nil))
    (emacspeak-org-drill-log "org-drill-present-simple-card start")
    (save-excursion
      (emacspeak-org-drill-log (org-drill-get-entry-text))
      (emacspeak-org-drill-save-question (org-drill-get-entry-text))))
  ad-do-it
  (emacspeak-org-drill-log "org-drill-present-simple-card end"))


					; This currently is broken.
(defadvice org-drill-present-two-sided-card (around emacspeak pre act comp)
  (let ((emacspeak-speak-messages nil)
	)
    (emacspeak-org-drill-log "org-drill-present-twosided-card start")
    (save-excursion
      (with-hidden-comments
       (with-hidden-cloze-hints
	(with-hidden-cloze-text
	 (let ((drill-sections (org-drill-hide-all-subheadings-except nil)))
	   (when drill-sections
	     (save-excursion
	       (goto-char (nth (random* (min 2 (length drill-sections)))
			       drill-sections))
	       (org-show-subtree)))
	   (org-drill--show-latex-fragments)
	   (ignore-errors
	     (org-display-inline-images t))
	   (org-cycle-hide-drawers 'all)
	   ))))
      (emacspeak-org-drill-log (buffer-substring (point-min) (point-max)))
      (emacspeak-org-drill-save-question (buffer-substring (point-min) (point-max)))
      )
    )
  ad-do-it
  (emacspeak-org-drill-log "org-drill-present-twosided-card end"))


(defadvice org-drill-present-default-answer (around emacspeak pre act comp)
  "Present the answer and provide ability to review answer.  To review answer press e at the answer prompt, this places you in a recursive edit buffer where you can review the answer.  Hit c-m-c to exit."
  (emacspeak-auditory-icon 'help)
  (emacspeak-org-drill-log "org-drill-present-default-answer start")
  (save-excursion
    (let ((emacspeak-speak-messages nil))
      (cond
       ; This is a special case that we currently don't use.
       (drill-answer
	(emacspeak-org-drill-log ("in drill-answer clause of cond"))
	(with-replaced-entry-text
	 (emacspeak-org-drill-log (     (format "\nAnswer:\n\n  %s\n" drill-answer)))
	 (format "\nAnswer:\n\n  %s\n" drill-answer)
	 (prog1
	     (funcall reschedule-fn)
	   (setq drill-answer nil))))
       ; This is the code path that is used currently
       (t
	(emacspeak-org-drill-log "in t clause of cond")
	(org-drill-hide-subheadings-if 'org-drill-entry-p)
	(org-drill-unhide-clozed-text)
	(org-cycle-hide-drawers 'all)
	(with-hidden-cloze-hints
	 ;Stash the answer away
	 (emacspeak-org-drill-save-answer (buffer-substring (point-min) (point-max))))))
      ; Present the answer and allow for review of the answer.
      (emacspeak-org-drill-answer-prompt)
      ad-do-it)
    (emacspeak-org-drill-log "after call ad-do-it emacspeak-org-drill-current-answer")
    (emacspeak-org-drill-log emacspeak-org-drill-current-answer))
  (emacspeak-org-drill-log "org-drill-present-default-answer end"))



(defun emacspeak-org-drill-log (msg)
  "Log msg to *drill-scratch-buffer*.  Used for debugging."
  (let ((scratch-buffer (get-buffer-create "*drill-scratch-buffer*"))
        (line nil))
    (with-current-buffer scratch-buffer
      (insert (concat msg "\n")))))


(defun emacspeak-org-drill-save-question (question)
  "put the question text into a temp buffer for debug"
  (let ((question-buffer (get-buffer-create "*emacspeak-drill-question-buffer*"))
        (line nil))
    (setq emacspeak-org-drill-current-question question)
    (with-current-buffer question-buffer
      (erase-buffer)
      (insert (concat question "\n")))))


(defun emacspeak-org-drill-save-answer (answer)
  "put the answer text into a temp buffer for debug"
  (let ((answer-buffer (get-buffer-create "*emacspeak-drill-answer-buffer*")))
    (with-current-buffer answer-buffer
      (org-mode)
      (erase-buffer)
      (insert (concat answer "\n"))
      (beginning-of-buffer)

(emacspeak-org-drill-log "before point")
(beginning-of-buffer)
(org-next-visible-heading 1)
(search-forward ":END:" nil t)
(delete-region (point-min) (point))
    (setq emacspeak-org-drill-current-answer (buffer-substring (point-min) (point-max)))
(emacspeak-org-drill-log "after point")
)
))




(defun    emacspeak-org-drill-answer-prompt ()
  "present the answer and allow the user to review if desired"
  (emacspeak-org-drill-log "emacspeak-org-drill-answer-prompt start")
  (emacspeak-org-drill-log emacspeak-org-drill-current-answer)
  (dtk-speak emacspeak-org-drill-current-answer)
  (sit-for 1)
  (setq input (read-key-sequence nil))
  (if (stringp input) (setq ch (elt input 0)))
  (if (eql ch ?e)
      (progn
	(dtk-speak "edit.  Use c-m-c to exit")
	(save-excursion
	  (switch-to-buffer "*emacspeak-drill-answer-buffer*")
	  (recursive-edit))))
  (emacspeak-org-drill-log "emacspeak-org-drill-answer-prompt end"))



