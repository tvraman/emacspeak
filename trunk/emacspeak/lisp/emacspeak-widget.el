;;; emacspeak-widget.el --- Speech enable Emacs' native GUI widget library
;;; $Id$
;;; $Author$ 
;;; Description: Emacspeak extensions to widgets
;;; Keywords:emacspeak, audio interface to emacs customized widgets
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
;;;Copyright (C) 1995, 1996, 1997, 1998, 1999   T. V. Raman  
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

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(eval-when (compile)
(condition-case nil
    (progn (require 'widget)
           (require 'wid-edit)
           (message "Compiling against widget libraries %s %s"
                    (locate-library "widget")
                    (locate-library "wid-edit")))
  (error
   (message  "Widget libraries not found, widget support may not work correctly."))))
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
;;{{{  Introduction

;;; This module implements the necessary extensions to provide talking
;;; widgets.

;;}}}
;;{{{  Customize global behavior
(declaim (special widget-menu-minibuffer-flag))
(setq  widget-menu-minibuffer-flag t)

;;}}}
;;{{{ Helper functions
(define-widget-keywords :emacspeak-help)

(defsubst emacspeak-widget-type (widget) (car widget ))

;;}}}
;;{{{  define summarizer

(defsubst emacspeak-widget-summarize(widget)
  (when widget
    (let ((emacspeak-help (widget-get widget ':emacspeak-help))
          (emacspeak-speak-messages nil))
      (cond
       ((and emacspeak-help
             (fboundp emacspeak-help))
        (dtk-speak  (funcall emacspeak-help widget)))
       (t (emacspeak-widget-default-summarize widget))))))

(defun emacspeak-widget-default-summarize (widget)
  "Fall back summarizer for all widgets"
  (let ((emacspeak-lazy-message-time 0)
        (tag (widget-get widget ':tag ))
        (value (widget-value widget ))
        (emacspeak-speak-messages nil))
    (if (or tag value                                )
       (dtk-speak (message "%s"
               (or tag value  "")))
(dtk-speak (current-message)))))

;;}}}
;;{{{  widget-voice --as per Per's suggestion

(declaim (special :emacspeak-help))
;;{{{ editable-list

(defun emacspeak-widget-help-editable-list (widget)
  "Summarize a choice item"
  (let ((value (widget-value widget))
        (tag (widget-get widget ':tag))
        (type (emacspeak-widget-type  widget)))
    (format "%s %s   is %s"
            (if (eq type 'editable-list)
                "editable list"
              (or type ""))
            (or tag  "")
            (or value ""))))

(widget-put (get 'editable-list 'widget-type)
		:emacspeak-help 'emacspeak-widget-help-editable-list)

;;}}}
;;{{{ choice-item

(defun emacspeak-widget-help-choice-item (widget)
  "Summarize a choice item"
  (let ((value (widget-value widget))
        (tag (widget-get widget ':tag))
        (parent-type (emacspeak-widget-type  (widget-get widget ':parent))))
     (format "%s %s   is %s"
             (cond
              ((eq parent-type 'radio-button) "radio button ")
              ((eq parent-type 'menu-choice) "menu choice ")
              ((eq parent-type 'checkbox) " check box ")
              (t parent-type))
             (or tag  "")
             (cond
              ((eq parent-type 'checkbox)
               (if value "checked" "unchecked"))
              ((eq parent-type 'radio-button)
               (if value " pressed " "not pressed "))
              (t (if value " on " " off "))))))

(widget-put (get 'choice-item 'widget-type)
		:emacspeak-help 'emacspeak-widget-help-choice-item)

;;}}}
;;{{{ checkbox

(defun emacspeak-widget-help-checkbox (widget)
  "Summarize a checkbox"
  (let ((value (widget-value widget))
        (tag (widget-get widget ':tag)))
    (format "%s %s"
            (or tag "")
            (if value "checked" "unchecked"))))
(widget-put (get 'checkbox 'widget-type)
		:emacspeak-help 'emacspeak-widget-help-checkbox)

;;}}}
;;{{{  menu choice 

(defun emacspeak-widget-help-menu-choice  (widget)
  "Summarize a pull down list"
  (let (
        (tag (widget-get widget ':tag))
        (value (widget-get widget ':value))
        (type (emacspeak-widget-type widget ))
        (emacspeak-speak-messages nil))
    (format "%s %s   is %s"
            (if (eq type 'menu-choice)
                "menu choice"
              (or type ""))
            (or tag "")
            (cond
             ((stringp value) value)
             ((numberp value) value)
             (t  (if value "on" "off" ))))))
      

    (widget-put (get 'menu-choice 'widget-type)
		:emacspeak-help 'emacspeak-widget-help-menu-choice)

;;}}}
;;{{{ editable field

(defun emacspeak-widget-help-editable-field (widget)
  "Summarize an editable field"
  (let ((format (widget-get widget ':format))
        (value (widget-value widget))
        (scratch-buffer (get-buffer-create " *dtk-scratch-buffer* ")))
    (setq format
          (when format 
            (save-excursion
              (set-buffer scratch-buffer)
              (unwind-protect 
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (insert "edit field")
                 (insert format)
                 (put-text-property (point-min) (point-max) 'read-only nil)
                 (goto-char (point-min))
                 (while (search-forward  "%v" nil t)
                   (replace-match  value))
                 (buffer-string ))
               (setq inhibit-read-only nil)))))
    (format "%s"
            (or format value))))

(widget-put (get 'editable-field 'widget-type)
		:emacspeak-help 'emacspeak-widget-help-editable-field)

;;}}}
;;{{{  link 

(defun emacspeak-widget-help-link (widget)
  "Summarize a link"
    (let ((value (widget-get widget ':value)))
        (format "link to %s"
                (or value ""))))

(widget-put (get 'link 'widget-type)
		:emacspeak-help 'emacspeak-widget-help-link)

;;}}}
;;{{{ push button 

(defun emacspeak-widget-help-push-button (widget)
  "Summarize a push button"
  (let ((type (emacspeak-widget-type widget))
        (value (widget-value widget))
        (tag (widget-get widget ':tag)))
     (format "%s  %s"
             (cond
              ((eq type 'push-button) "push button")
              ((eq type 'insert-button) " insert button ")
              ((eq type 'delete-button) " delete button ")
               (t (or  type "")))
             (or tag value ""))))

(widget-put (get 'push-button 'widget-type)
		:emacspeak-help 'emacspeak-widget-help-push-button)

;;}}}
;;{{{ radio-button-choice

(defun emacspeak-widget-help-radio-button-choice (widget)
  "Summarize a choice item"
  (let ((value (widget-value widget))
        (tag (widget-get widget ':tag))
        (type (emacspeak-widget-type  widget)))
    (format "%s %s   is %s"
            (if (eq type 'radio-button-choice )
                "radio button choice "
              (or type ""))
            (or tag  "")
            (or value ""))))

(widget-put (get 'radio-button-choice 'widget-type)
		:emacspeak-help 'emacspeak-widget-help-radio-button-choice)

;;}}}

;;}}}
;;{{{  Widget motion

;;; avoid redundant message speech output
(defadvice widget-echo-help (around emacspeak pre act comp)
(let ((emacspeak-speak-messages nil))
ad-do-it
ad-return-value))
(defadvice widget-beginning-of-line (after emacspeak pre act comp)
  "Provide auditory feedback"
  (cond
   ((interactive-p)
    (let ((widget (widget-at (point ))))
      ad-do-it
    (emacspeak-auditory-icon 'select-object)
    (message "Moved to start of text field %s"
             (if widget
                 (widget-value widget)
               ""))))
   (t ad-do-it))
  ad-return-value)

(defadvice widget-end-of-line (around emacspeak pre act comp)
  "Provide auditory feedback"
  (cond
   ((interactive-p)
    (let ((widget (widget-at (point ))))
      ad-do-it
    (emacspeak-auditory-icon 'select-object)
    (message "Moved to end of text field %s"
             (if widget
                 (widget-value widget)
               ""))))
   (t ad-do-it))
    ad-return-value)

(defadvice widget-forward (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
  (emacspeak-auditory-icon 'large-movement)
  (emacspeak-widget-summarize (widget-at  (point )))))

(defadvice widget-backward (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
(emacspeak-auditory-icon 'large-movement)
  (emacspeak-widget-summarize (widget-at (point)))))

(defadvice widget-kill-line (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-speak-current-kill 0)
    (dtk-tone 500 30)))

;;}}}
;;{{{  activating widgets:

(defadvice widget-button-press (around emacspeak pre act comp)
  "Provide auditory feedback"
  (let ((inhibit-read-only t)
        (widget (widget-at (ad-get-arg 0))))
    (cond
     (widget                            ; First record some state:
      (let ((pos (ad-get-arg 0))
            (old-position (point)))
        (when (eq major-mode 'w3-mode)
          (emacspeak-auditory-icon 'button))
        ad-do-it
        (cond
         ((= old-position (point ))     ;did not move
          (emacspeak-auditory-icon 'button)
          (emacspeak-widget-summarize (widget-at pos)))
         (t  (emacspeak-auditory-icon 'large-movement)
             (or (emacspeak-widget-summarize (widget-at (point)))
                 (emacspeak-speak-line))))))
     (t ad-do-it))
    ad-return-value))

;;}}}
;;{{{ voice lock widget buffers:

(defvar emacspeak-widget-field-personality  'paul-animated
  "Personality for edit fields")

(defvar emacspeak-widget-button-personality 'paul-smooth
  "Personality for buttons")

(defvar emacspeak-widget-documentation-personality 'paul-monotone
  "Personality for documentation")

(defadvice widget-specify-field-update (after emacspeak pre act comp)
  "Voiceify the field"
(put-text-property (ad-get-arg 1) (ad-get-arg 2)
                   'personality emacspeak-widget-field-personality))

(defadvice widget-specify-button (after emacspeak pre act  comp)
  "Voiceify the button"
(put-text-property (ad-get-arg 1) (ad-get-arg 2)
                   'personality emacspeak-widget-button-personality))  

(defadvice widget-specify-doc (after emacspeak pre act comp)
"Voiceify the documentation of a widget"
(put-text-property (ad-get-arg 1) (ad-get-arg 2)
                   'personality emacspeak-widget-documentation-personality))

;;}}}
;;{{{  Interactively summarize a widget and its parents.

(defun emacspeak-widget-summarize-widget-under-point (&optional level)
  "Summarize a widget if any under point.
Optional interactive prefix specifies how many levels to go up from current
widget before summarizing."
  (interactive "P")
  (let ((widget (widget-at (point ))))
    (when(and widget  level)
      (loop for i from 1 to level
             do
            (setq widget (widget-get  widget :parent))))
    (cond
     (widget (emacspeak-widget-summarize widget ))
      (t (message "No widget under point")))))

(defun emacspeak-widget-browse-widget-interactively ()
  "Allows you to browse a widget"
  (interactive)
  (let ((level nil )
        (key nil)
        (continue t))
      (emacspeak-widget-summarize-widget-under-point)
    (while  continue
      (setq key (read-event))
      (cond
       ((= key ?q) (setq continue nil)
        (message "exitting widget browser"))
       ((= key ?.) nil)
       ((= key ?u)
        (if (numberp level)
            (incf level)
          (setq level 1)))
       ((= key ?d)
        (if (> level  0)
            (decf level)
          (message "Leaf widget")))
       (t (read-key-sequence "Press any key to continue")))
      (emacspeak-widget-summarize-widget-under-point level))))

;;}}}
;;{{{ work around widget problems
(defadvice widget-convert-text (around emacspeak pre act comp)
  "Protect value of personality if set originally"
  (let ((start (ad-get-arg 1))
        (end (ad-get-arg 2))
        (orig nil ))
    (setq orig (get-text-property start 'personality))
    ad-do-it 
    (and orig 
         (put-text-property start end 
                            'personality orig ))))

;;}}}
(provide  'emacspeak-widget)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
