;;; emacspeak-rmail.el --- Speech enable RMail -- Emacs' default mail agent
;;; $Id$
;;; $Author$ 
;;; Description: Emacspeak extension for rmail
;;; Keywords:emacspeak, audio interface to emacs mail
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
;;;Copyright (C) 1995 -- 2006, T. V. Raman 
;;; Copyright (c) 1995 by T. V. Raman  
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

;;{{{  Introduction

;;; emacspeak extensions to rmail

;;}}}
;;{{{ requires
(require 'emacspeak-preamble)
(require 'desktop)
(require 'rmailsort)
(require 'emacspeak-desktop)

;;}}}
;;{{{  customizations:

(declaim (special rmail-ignored-headers))
(setq rmail-ignored-headers
      (concat "^X-\\|"
              "^Content-\\|"
              "^Mime-\\|"
              rmail-ignored-headers))

;;}}}
;;{{{  helper functions:

(defun emacspeak-rmail-summarize-message (message)
  "Summarize message in rmail identified by message number message"
  (let ((subject (rmail-fetch-field message "Subject"))
        (to (rmail-fetch-field message "To"))
        (from (rmail-fetch-field message "From"))
        (lines (count-lines (rmail-msgbeg message)
                            (rmail-msgend message)))
        (labels (let ((rmail-current-message message ))
                  (rmail-display-labels ))))
    (dtk-speak
     (format "%s %s   %s %s labelled %s "
             (or from "")
             (if (and to (< (length to) 80))
                 (format "to %s" to) "")
             (if subject (format "on %s" subject) "")
             (if lines (format "%s lines" lines) "")
             labels))))

;;}}}
;;{{{  Advice some commands.
;;{{{  buffer selection

(defadvice rmail-quit(after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice rmail-bury(after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))
(defadvice rmail (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-rmail-summarize-current-message)))

(defadvice rmail-expunge-and-save (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'save-object)))

;;}}}
;;{{{  message navigation

(defadvice rmail-beginning-of-message (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

;;}}}
;;{{{  folder navigation

(defadvice rmail-first-message (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-rmail-summarize-message rmail-current-message)))

(defadvice rmail-first-unseen-message (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-rmail-summarize-message rmail-current-message)))

(defadvice rmail-last-message (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-rmail-summarize-message rmail-current-message)))

(defadvice rmail-next-undeleted-message (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-rmail-summarize-message rmail-current-message)))

(defadvice rmail-next-message (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-rmail-summarize-message rmail-current-message)))

(defadvice rmail-previous-undeleted-message (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-rmail-summarize-message rmail-current-message)))
(defadvice rmail-previous-message (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-rmail-summarize-message rmail-current-message)))
(defadvice rmail-next-labeled-message (around emacspeak pre act comp)
  "Provide auditory feedback"
  (cond
   ((interactive-p)
    (let ((original rmail-current-message))
      ad-do-it
      (cond
       ((not (= original rmail-current-message))
        (emacspeak-auditory-icon 'select-object)
        (emacspeak-rmail-summarize-message rmail-current-message))
       (t (emacspeak-auditory-icon 'search-miss)))))
   (t ad-do-it))
  ad-return-value)
        

(defadvice rmail-previous-labeled-message (around emacspeak pre act comp)
  "Provide auditory feedback"
  (cond
   ((interactive-p)
    (let ((original rmail-current-message))
      ad-do-it
      (cond
       ((not (= original rmail-current-message))
        (emacspeak-auditory-icon 'select-object)
        (emacspeak-rmail-summarize-message rmail-current-message))
       (t (emacspeak-auditory-icon 'search-miss)))))
   (t ad-do-it))
  ad-return-value)

(defadvice rmail-show-message (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-rmail-summarize-message rmail-current-message)))  

;;}}}
;;{{{ delete and undelete messages

(defadvice rmail-undelete-previous-message (after emacspeak pre act
                                                  comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-rmail-summarize-current-message)))
(defadvice rmail-delete-message (after emacspeak pre act)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (message "Message discarded.")))

(defadvice rmail-delete-forward (after emacspeak pre act comp)
  "provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-rmail-summarize-current-message)))

(defadvice rmail-delete-backward (after emacspeak pre act comp)
  "provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-rmail-summarize-current-message))) ;;}}}

;;}}}
;;{{{  Additional interactive commands

(defun emacspeak-rmail-summarize-current-message ()
  "Summarize current message"
  (interactive)
  (declare (special rmail-current-message))
  (emacspeak-rmail-summarize-message rmail-current-message))
(defun  emacspeak-rmail-speak-current-message-labels ()
  "Speak labels of current message"
  (interactive)
  (dtk-speak
   (format "Labels are %s"
           (rmail-display-labels))))

;;}}}
;;{{{  key bindings

(declaim (special rmail-mode-map))
(define-key rmail-mode-map "\C-m" 'emacspeak-rmail-summarize-current-message)
(define-key rmail-mode-map "L" 'emacspeak-rmail-speak-current-message-labels)

;;}}}
(provide  'emacspeak-rmail)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
