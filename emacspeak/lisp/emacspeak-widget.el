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
;;;Copyright (C) 1995 -- 2000, T. V. Raman 
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

;;; Commentary:

;;; This module implements the necessary extensions to provide talking
;;; widgets.

;;}}}
;;{{{ required modules 

;;; Code:

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

;;}}}
;;{{{  Customize global behavior

(declaim (special widget-menu-minibuffer-flag))
(setq  widget-menu-minibuffer-flag t)

(define-widget-keywords :emacspeak-help
  :caption)

;;}}}
;;{{{  define summarizer

(defun emacspeak-widget-summarize-parent ()
  "Summarize parent of widget at point."
  (interactive)
  (let* ((w (widget-at (point)))
         (p (when w (widget-get w :parent))))
    (cond
     (p (emacspeak-widget-summarize p))
     (t (message "Widget at point has no parent")))))
;;; Find summarizer for a specific widget type and dispatch.

(defun emacspeak-widget-summarize(widget)
  (when widget
    (let ((emacspeak-help (widget-get widget :emacspeak-help))
          (emacspeak-speak-messages nil)
          (voice-lock-mode t))
      (cond
       ((and emacspeak-help
             (fboundp emacspeak-help))
        (dtk-speak  (funcall emacspeak-help widget)))
       (t (emacspeak-widget-default-summarize widget))))))

;;}}}
;;{{{  widget specific summarizers  --as per Per's suggestion

;;{{{  default

(defun emacspeak-widget-default-summarize (widget)
  "Fall back summarizer for all widgets"
  (let ((emacspeak-lazy-message-time 0)
        (tag (widget-get widget :tag ))
        (value (widget-value widget ))
        (emacspeak-speak-messages nil)
        (voice-lock-mode t))
    (if (or tag value                                )
        (when tag
          (put-text-property 0 (length tag)
                             'personality 'paul-animated  tag))
      (when value 
        (put-text-property 0 (length value)
                           'personality 'paul-smooth value))
      (cond
       ((or tag value)
      (message
       (concat
        (or tag " ")
        (or value " "))))
(t (dtk-speak (current-message)))))))

(widget-put (get 'default 'widget-type)
            :emacspeak-help 'emacspeak-widget-default-summarize)

;;}}}
;;{{{ editable field

(defun emacspeak-widget-help-editable-field (widget)
  "Summarize an editable field"
  (let ((value (widget-value widget))
        (help-echo (widget-get  widget :help-echo)))
    (when help-echo
      (put-text-property 0 (length help-echo)
                         'personality 'paul-animated help-echo))
    (concat 
     (or help-echo " ")
     (or value " blank "))))

(widget-put (get 'editable-field 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-editable-field)

;;}}}
;;{{{ item 

(defun emacspeak-widget-help-item (widget)
  "Summarize a  item"
  (let* ((value (widget-value widget))
         (tag (widget-get widget :tag)))
    (concat
     (or tag value))))

(widget-put (get 'item 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-item)

;;}}}
;;{{{  push button 

(defun emacspeak-widget-help-push-button (widget)
  "Summarize a push button.
Is smart about summarizing the parent where it makes sense,
e.g. for repeat lists."
  (let* ((type (widget-type widget))
         (tag (widget-get widget :tag))
         (parent (widget-get widget :parent))
         (parent-help(when parent
                       (widget-apply parent :emacspeak-help))))
    (concat
      (format " %s " type) 
     tag
     (or parent-help " "))))

(widget-put (get 'push-button 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-push-button)

;;}}}
;;{{{  link 

(defun emacspeak-widget-help-link (widget)
  "Summarize a link"
  (let ((value (widget-get widget :value)))
    (format "link to %s"
            (or value ""))))

(widget-put (get 'link 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-link)

;;}}}
;;{{{  info-link 

(defun emacspeak-widget-help-info-link (widget)
  "Summarize an info  link"
  (let ((value (widget-get widget :value)))
    (format "Online help  %s"
            (or value ""))))

(widget-put (get 'info-link 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-info-link)

;;}}}
;;{{{  url-link 

(defun emacspeak-widget-help-url-link (widget)
  "Summarize a WWW    link"
  (let ((value (widget-get widget :value)))
    (format "WWW link   %s"
            (or value ""))))

(widget-put (get 'url-link 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-url-link)

;;}}}
;;{{{  variable-link 

(defun emacspeak-widget-help-variable-link (widget)
  "Summarize a     link to a variable."
  (let ((value (widget-get widget :value)))
    (format "WWW link   %s"
            (or value ""))))

(widget-put (get 'variable-link 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-variable-link)

;;}}}
;;{{{  function-link 

(defun emacspeak-widget-help-function-link (widget)
  "Summarize a     link to a function."
  (let ((value (widget-get widget :value)))
    (format "Link to function    %s"
            (or value ""))))

(widget-put (get 'function-link 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-function-link)

;;}}}
;;{{{  file-link 

(defun emacspeak-widget-help-file-link (widget)
  "Summarize a     link to a file."
  (let ((value (widget-get widget :value)))
    (format "WWW link   %s"
            (or value ""))))

(widget-put (get 'file-link 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-file-link)

;;}}}
;;{{{  emacs-library-link 

(defun emacspeak-widget-help-emacs-library-link (widget)
  "Summarize a     link to an Emacs Library.."
  (let ((value (widget-get widget :value)))
    (format "WWW link   %s"
            (or value ""))))

(widget-put (get 'emacs-library-link 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-emacs-library-link)

;;}}}
;;{{{  emacs-commentary-link 

(defun emacspeak-widget-help-emacs-commentary-link (widget)
  "Summarize a     link to a emacs commentary section.."
  (let ((value (widget-get widget :value)))
    (format "WWW link   %s"
            (or value ""))))

(widget-put (get 'emacs-commentary-link 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-emacs-commentary-link)

;;}}}
;;{{{  menu choice 

(defun emacspeak-widget-help-menu-choice  (widget)
  "Summarize a pull down list"
  (let* ((tag (widget-get widget :tag))
         (value (widget-get widget :value))
         (child (car (widget-get widget :children)))
         (type (widget-type widget ))
         (emacspeak-speak-messages nil))
    (put-text-property 0 (length tag)
                       'personality 'harry tag)
    (concat
     (or tag (format " %s " type))
     " is "
     (if child
         (widget-apply child :emacspeak-help)
(format " %s " value)))))
      

(widget-put (get 'menu-choice 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-menu-choice)

;;}}}
;;{{{  toggle  

(defun emacspeak-widget-help-toggle (widget)
  "Summarize a toggle."
  (let* ((type (widget-type widget))
         (value (widget-value widget))
         (tag (widget-get widget :tag)))
    (concat
     (or tag 
     (format " %s " type))
     (if value " is on "
       " is off "))))

;;; shared for checkbox 

(widget-put (get 'toggle 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-toggle)

;;}}}
;;{{{  checklist

(defun emacspeak-widget-help-checklist  (widget)
  "Summarize a check list"
  (let* ((tag (widget-get widget :tag))
         (value (widget-value widget))
         (type (widget-type widget ))
         (selections (cond
                      (value 
                       (mapconcat
                        #'(lambda (s)
                            (format " %s " s))
                        value " "))
                      (t " no items  "))))
    (put-text-property 0  (length selections)
                       'personality 'paul-animated selections)
    (put-text-property 0 (length tag)
                       'personality 'harry tag)
    (concat
     (or tag (format " %s " type))
     " has "
     selections 
     " checked ")))
     
      

(widget-put (get 'checklist 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-checklist)

;;}}}
;;{{{ choice-item

(defun emacspeak-widget-help-choice-item (widget)
  "Summarize a choice item"
  (let ((value (widget-value widget))
        (tag (widget-get widget :tag)))
    (concat 
     (or tag  "")
(or value " ")
     " is "
     (widget-apply (widget-get widget :parent)
                   :emacspeak-help))))

(widget-put (get 'choice-item 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-choice-item)

;;}}}
;;{{{ checkbox

(defun emacspeak-widget-help-checkbox (widget)
  "Summarize a checkbox"
  (let* ((value (widget-value widget))
                                        ;sibling has the lable
         (sibling (widget-get-sibling widget))
         (tag (widget-get widget :tag))
         (label
          (if sibling
              (widget-get sibling :tag)
            tag)))
    (put-text-property 0 (length label)
                       'personality 'paul-animated label)
    (concat 
            label 
            (if value "checked" "unchecked"))))

(widget-put (get 'checkbox 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-checkbox)

;;}}}
;;{{{ radio-button

(defun emacspeak-widget-help-radio-button (widget)
  "Summarize a radio button"
  (let* ((value (widget-value widget))
        (tag (widget-get widget :tag))
        ;sibling has the lable
         (sibling (widget-get-sibling widget))
         (label (if sibling
              (widget-get sibling :tag)
            tag)))
    (concat 
     (or label tag )
     " is "
     (if value
         " pressed "
       " not pressed "))))

(widget-put (get 'radio-button 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-radio-button)

;;}}}
;;{{{ radio-button-choice

(defun emacspeak-widget-help-radio-button-choice  (widget)
  "Summarize a radio group "
  (let* ((tag (widget-get widget :tag))
         (value (widget-value widget))
         (choice (widget-get widget :choice))
         (type (widget-type widget ))
         (selected
          (cond
           (choice (widget-get choice :tag))
           (t (or value
                  " no item ")))))
    (put-text-property 0  (length selected)
                       'personality 'paul-animated selected)
    (put-text-property 0 (length tag)
                       'personality 'harry tag)
    (concat
     (or tag (format " %s " type))
     " is "
     selected)))

(widget-put (get 'radio-button-choice 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-radio-button-choice)

;;}}}
;;{{{ editable-list

(defun emacspeak-widget-help-editable-list (widget)
  "Summarize a editable list"
  (let ((value
         (mapconcat 
          (function
           (lambda (x)
             (format "%s" x)))
          (widget-value widget)
          " "))
        (tag (widget-get widget :tag))
        (type(format "%s"
                     (widget-type  widget))))
    (put-text-property 0 (length type)
                       'personality 'harry
                       type)
    (when tag
      (put-text-property 0 (length tag)
                         'personality 'paul-animated tag))
    (when value
      (put-text-property 0 (length value)
                         'personality 'paul-smooth value))
    (concat 
     type
     (or tag  "")
     (or value ""))))

(widget-put (get 'editable-list 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-editable-list)

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

(defvar emacspeak-widget-button-personality 'harry
  "Personality for buttons")

(defvar emacspeak-widget-documentation-personality 'paul-monotone
  "Personality for documentation")
(defvar emacspeak-widget-inactive-personality  'betty
  "Personality for inactive fields")

(defadvice widget-specify-field-update (after emacspeak pre act comp)
  "Voiceify the field"
  (put-text-property (ad-get-arg 1) (ad-get-arg 2)
                     'personality
                     emacspeak-widget-field-personality))

(defadvice  widget-specify-inactive(after emacspeak pre act comp)
  "Voiceify the field"
  (put-text-property (ad-get-arg 1) (ad-get-arg 2)
                     'personality emacspeak-widget-inactive-personality))

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
      (setq key (read-char))
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
;;{{{ update widget related keymaps so we dont loose the
;;emacspeak prefix 


(defadvice widget-setup (after emacspeak pre act comp)
  "Fix widget keymaps so we dont loose the emacspeak
prefix."
  (declare (special emacspeak-prefix))
  (define-key widget-field-keymap  emacspeak-prefix 'emacspeak-prefix-command)
  (define-key widget-field-keymap  "\C-ee"
    'widget-end-of-line)
  (define-key widget-text-keymap  emacspeak-prefix 'emacspeak-prefix-command)
  (define-key widget-text-keymap  "\C-ee" 'widget-end-of-line)
  )
;;}}}
;;{{{ augment widgets 
(defun emacspeak-widget-update-from-minibuffer (point)
  "Sets widget at point by invoking its prompter."
  (interactive "d")
  (let ((w (widget-at (point))))
    (widget-value-set w
                      (widget-apply w
                                    :prompt-value
                                    (widget-get w :tag)
                                    (widget-value w)
                                    nil))
    (widget-setup)
    (widget-apply w :notify)
    (emacspeak-widget-summarize w)))

(declaim (special widget-keymap))
(define-key widget-keymap "\M-p" 'emacspeak-widget-summarize-parent)
(define-key widget-field-keymap "\M-\C-m" 'emacspeak-widget-update-from-minibuffer)

;;}}}
(provide  'emacspeak-widget)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
