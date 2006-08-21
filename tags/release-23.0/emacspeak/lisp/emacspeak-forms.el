;;; emacspeak-forms.el --- Speech enable Emacs' forms mode  -- provides  a convenient database interface
;;; $Id$
;;; $Author$ 
;;; DescriptionEmacspeak extensions for forms-mode 
;;; Keywords:emacspeak, audio interface to emacs forms 
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
;;; Copyright (c) 1996 by T. V. Raman 
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

;;{{{ requires

(require 'forms)(require 'emacspeak-preamble)

;;}}}
;;{{{  Introduction:

;;; Provide additional advice to forms-mode 

;;}}}
;;{{{  custom
(defgroup emacspeak-forms nil
  "Emacspeak support for forms mode."
  :group 'emacspeak
  :group 'forms
  :prefix "emacspeak-forms-")

;;}}}
;;{{{ Helper functions

(defvar emacspeak-forms-current-record-summarizer
  'emacspeak-forms-speak-field
  "Summarizer function for summarizing a record. Default is to
speak the first field")
(make-variable-buffer-local
 emacspeak-forms-current-record-summarizer)

(defun emacspeak-forms-summarize-current-record ()
  "Summarize current record"
  (interactive)
  (declare (special emacspeak-forms-current-record-summarizer))
  (funcall emacspeak-forms-current-record-summarizer))

(defun emacspeak-forms-summarize-current-position ()
  "Summarize current position in list of records"
  (interactive)
  (declare (special forms--current-record forms--total-records
                    forms-file))
  (dtk-speak
   (format "Record %s of %s from %s"
           forms--current-record forms--total-records forms-file)))

(defcustom emacspeak-forms-rw-voice 'paul
  "Personality for read-write fields. "
  :type 'symbol
  :group 'emacspeak-forms)

(defcustom emacspeak-forms-ro-voice voice-annotate
  "Personality for read-only fields. "
  :type 'symbol
  :group 'emacspeak-forms)

(defun emacspeak-forms-speak-field ()
  "Speak current form field name and value.
Assumes that point is at the front of a field value."
  (interactive)
  (let ((voice-lock-mode t)
        (name nil)
        (value nil)
        (n-start nil))
    (save-excursion
      (backward-char 1)
      (setq n-start (point)))
    (setq name (buffer-substring n-start (point)))
    (setq value
          (buffer-substring
           (point)
           (or
            (next-single-property-change (point) 'read-only)
            (point))))
    (put-text-property 0 (length name)
                       'personality
                       emacspeak-forms-ro-voice name)
    (put-text-property 0 (length value )
                       'personality emacspeak-forms-rw-voice value)
    (dtk-speak (concat name " " value ))))

;;}}}
;;{{{ Advise interactive  commands

(defadvice forms-next-record (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (goto-char (next-single-property-change 
                (point)
                'read-only
                (current-buffer)
                (point-max)))
    (emacspeak-forms-summarize-current-record)))

(defadvice forms-prev-record (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (goto-char (next-single-property-change 
                (point)
                'read-only
                (current-buffer)
                (point-max)))
    (emacspeak-forms-summarize-current-record)))

(defadvice forms-first-record (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-forms-summarize-current-record)))

(defadvice forms-last-record (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-forms-summarize-current-record)))

(defadvice forms-jump-record (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-forms-summarize-current-record)))

(defadvice forms-search (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'search-hit)
    (emacspeak-forms-summarize-current-record)))

(defadvice forms-exit (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice forms-next-field (around emacspeak pre act comp)
  "Provide auditory feedback."
  (cond
   ((interactive-p)
    ad-do-it
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-forms-speak-field))
   (t ad-do-it))
  ad-return-value)

(defadvice forms-prev-field (after emacspeak pre act comp)
  "Provide auditory feedback."
  (cond
   ((interactive-p)
    ad-do-it
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-forms-speak-field))
   (t ad-do-it))
  ad-return-value)

(defadvice forms-kill-record (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    ))

(defadvice forms-insert-record (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    ))

(defadvice forms-save-buffer (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when  (interactive-p)
    (emacspeak-auditory-icon 'save-object)))

;;}}}
;;{{{ smart filters

(defun emacspeak-forms-flush-unwanted-records ()
  "Prompt for pattern and flush matching lines"
  (interactive)
  (let ((pattern (read-from-minibuffer
                  "Specify filter pattern")))
    (when (> (length pattern) 0)
      (flush-lines
       pattern))))

(defun emacspeak-forms-rerun-filter ()
  "Rerun  filter --allows us to nuke more matching records"
  (interactive)
  (declare (special forms--file-buffer
                    forms--total-records forms-read-only))
  (save-excursion
    (set-buffer forms--file-buffer)
    (let ((inhibit-read-only t)
          (file-modified (buffer-modified-p)))
      (emacspeak-forms-flush-unwanted-records)
      (if (not file-modified) (set-buffer-modified-p
                               nil))))
  (let (ro)
    (setq forms--total-records
	  (save-excursion
	    (prog1
		(progn
		  ;;(message "forms: counting records...")
		  (set-buffer forms--file-buffer)
		  (bury-buffer (current-buffer))
		  (setq ro buffer-read-only)
		  (count-lines (point-min) (point-max))))))
    (if ro
	(setq forms-read-only t)))
  (message "%s records after filtering"
           forms--total-records))

;;}}}
;;{{{ emacspeak forms find file
;;;###autoload
(defun emacspeak-forms-find-file (filename)
  "Visit a forms file"
  (interactive
   (list
    (read-file-name "Forms file: "
                    (expand-file-name "forms/" emacspeak-etc-directory))))
  (forms-find-file filename))

;;}}}
;;{{{ bind smart filters
(declaim (special forms-mode-map forms-mode-ro-map
                  forms-mode-edit-map))
(add-hook 'forms-mode-hooks
          (function
           (lambda nil 
             (mapcar 
              (function
               (lambda (map)
                 (define-key map "\C-m" 'emacspeak-forms-rerun-filter)
                 (define-key map "."
                   'emacspeak-forms-summarize-current-position)
                 (define-key map "," 'emacspeak-forms-summarize-current-record)))
              (list forms-mode-ro-map 
                    forms-mode-map))
;;; move to first field
             (forms-next-field 1))))
  

;;}}}
(provide  'emacspeak-forms)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
