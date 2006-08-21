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
;;;Copyright (C) 1995 -- 2002, T. V. Raman 
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

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'custom)
(eval-when-compile (require 'dtk-speak)
                   (condition-case nil
                       (progn (require 'widget)
                              (require 'wid-edit)
                              (message "Compiling against widget libraries %s %s"
                                       (locate-library "widget")
                                       (locate-library "wid-edit")))
                     (error
                      (message  "Widget libraries not found, widget support may not work correctly.")))
                   (require 'emacspeak-speak)
                   (require 'emacspeak-sounds))

;;}}}
;;{{{  Customize global behavior

(defgroup emacspeak-widget nil
  "Widgets on the Emacspeak Desktop."
  :group 'emacspeak
  :group 'widgets
  :prefix "emacspeak-widget-")
(defcustom emacspeak-widget-edit-personality  'paul-smooth
  "Personality for edit fields"
  :group 'emacspeak-widget
  :type 'symbol)

(defcustom emacspeak-widget-value-personality 'paul-smooth
  "Personality for values"
  :group 'emacspeak-widget
  :type 'symbol)

(defcustom emacspeak-widget-button-personality 'harry
  "Personality for buttons"
  :group 'emacspeak-widget
  :type 'symbol)

(defcustom emacspeak-widget-documentation-personality 'paul-monotone
  "Personality for documentation"
  :group 'emacspeak-widget
  :type 'symbol)

(defcustom emacspeak-widget-inactive-personality  'betty
  "Personality for inactive fields"
  :group 'emacspeak-widget
  :type 'symbol)

(declaim (special widget-menu-minibuffer-flag))
(setq  widget-menu-minibuffer-flag t)

(define-widget-keywords :emacspeak-help
  :caption)

;;}}}
;;{{{  helpers 

(defsubst emacspeak-widget-label (w)
  "Construct a label for a widget.
Returns a string with appropriate personality."
  (let ((type   (widget-type w))
        (tag (widget-get w :tag)))
    (unless tag (setq tag (format " %s " type)))
    (put-text-property 0 (length tag)
                       'personality emacspeak-widget-button-personality tag)
    tag))

(defsubst emacspeak-widget-help-echo (w)
  "Return help-echo with appropriate personality."
  (let ((h (widget-get w :help-echo))
        (help nil))
    (setq help
          (cond
           ((and h
                 (symbolp h)
                 (fboundp h))
            (widget-apply w :help-echo))
           ((and h (stringp h)) h)
           (t nil)))
    (when help
      (put-text-property 0 (length help)
                         'personality 'paul-animated help)
      help)))

;;}}}
;;{{{  define summarizer

(defun emacspeak-widget-help ()
  "Speak help for widget under point."
  (interactive)
  (let* ((w (widget-at (point)))
         (tag (widget-get w :tag))
         (type (widget-type w))
         (help-echo (when w (widget-get w :help-echo)))
         (p (widget-get w :parent))
         (parent-help (when  p (widget-get  p :help-echo))))
    (cond
     (help-echo (message help-echo))
     (parent-help (message " %s to %s "
                           (or tag type)
                           parent-help))
     (w (message (format " %s " type)))
     (t (message " Not on a widget. ")))))
        
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
  "Summarize specified widget."
  (let ((emacspeak-help (widget-get widget :emacspeak-help))
        (emacspeak-speak-messages nil)
        (voice-lock-mode t))
    (cond
     ((and emacspeak-help
           (fboundp emacspeak-help))
      (dtk-speak  (funcall emacspeak-help widget)))
     (t (dtk-speak (current-message))))))

;;}}}
;;{{{ advice activators 

;;{{{  widget specific summarizers  --as per Per's suggestion

;;{{{  default

(defun emacspeak-widget-default-summarize (widget)
  "Fall back summarizer for all widgets"
  (let* ((label (emacspeak-widget-label widget))
         (help-echo (emacspeak-widget-help-echo widget))
         (v  (widget-value widget ))
         (value
          (cond
           ((null v) nil)
           (t (prin1-to-string v 'no-escape)))))
    (when  value
      (put-text-property 0 (length value)
                         'personality emacspeak-widget-value-personality value))
    (concat label
            help-echo
            value)))
     
           

(widget-put (get 'default 'widget-type)
            :emacspeak-help 'emacspeak-widget-default-summarize)

;;}}}
;;{{{ editable field

(defun emacspeak-widget-help-editable-field (widget)
  "Summarize an editable field"
  (let* ((v  (widget-value widget))
         (value (when v
                  (format " %s " v)))
         (help-echo (emacspeak-widget-help-echo widget))
         (label (emacspeak-widget-label widget)))
    (concat label
            help-echo
            value)))

(widget-put (get 'editable-field 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-editable-field)

;;}}}
;;{{{ item 

(defun emacspeak-widget-help-item (widget)
  "Summarize a  item"
  (let* ((value  (widget-value widget))
         (label (emacspeak-widget-label widget))
         (help-echo (emacspeak-widget-help-echo widget)))
    (concat label
            help-echo
            (when value (format " %s " value)))))

(widget-put (get 'item 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-item)

;;}}}
;;{{{ visibility 

(defun emacspeak-widget-help-visibility (widget)
  "Summarize visibility widget"
  (let* ((value (widget-value widget))
         (help-echo (emacspeak-widget-help-echo widget))
         (label (emacspeak-widget-label widget)))
    (if value
        (emacspeak-auditory-icon 'open-object)
      (emacspeak-auditory-icon 'close-object))
    (concat
     label
     (or  help-echo
          (if value " hide  " " show  ")))))

(widget-put (get 'visibility 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-visibility)

;;}}}
;;{{{  push button 

(defun emacspeak-widget-help-push-button (widget)
  "Summarize a push button."
  (let* ((label (emacspeak-widget-label widget))
         (help-echo (emacspeak-widget-help-echo widget))
         (context-widget (widget-get widget :widget))
         (context
          (when  context-widget
            (widget-apply context-widget
                          :emacspeak-help))))
    (concat label
            help-echo
            context )))
       

(widget-put (get 'push-button 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-push-button)

;;}}}
;;{{{  link 

(defun emacspeak-widget-help-link (widget)
  "Summarize a link"
  (let ((value   (widget-get widget :value)))
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
  (let ((value (widget-get widget :value))
        (tag (widget-get widget :tag)))
    (format "URL %s %s"
            (or tag "")
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
    (format "Display documentation for %s" value)))

(widget-put (get 'function-link 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-function-link)

;;}}}
;;{{{  file-link 

(defun emacspeak-widget-help-file-link (widget)
  "Summarize a     link to a file."
  (let ((value (widget-get widget :value))
        (tag (widget-get widget :tag)))
    (format "File link %s    %s"
            (or tag "")
            value)))

(widget-put (get 'file-link 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-file-link)

;;}}}
;;{{{  emacs-library-link 

(defun emacspeak-widget-help-emacs-library-link (widget)
  "Summarize a     link to an Emacs Library.."
  (let ((value (widget-get widget :value))
        (tag (widget-get widget :tag)))
    (format "Emacs library  link   %s %s"
            (or tag "")
            value)))

(widget-put (get 'emacs-library-link 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-emacs-library-link)

;;}}}
;;{{{  emacs-commentary-link 

(defun emacspeak-widget-help-emacs-commentary-link (widget)
  "Summarize a     link to a emacs commentary section.."
  (let ((value (widget-get widget :value))
        (tag (widget-get widget :tag)))
    (format "Commentary  link   %s %s"
(or tag "")
            value)))

(widget-put (get 'emacs-commentary-link 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-emacs-commentary-link)

;;}}}
;;{{{  menu choice 

(defun emacspeak-widget-help-menu-choice  (widget)
  "Summarize a pull down list"
  (let* (
         (help-echo (emacspeak-widget-help-echo widget))(label (emacspeak-widget-label widget))
         (value (format " %s " (widget-get widget :value)))
         (child (car (widget-get widget :children))))
    (concat label
            " is "
            (if child
                (widget-apply child :emacspeak-help)
              value))))
      

(widget-put (get 'menu-choice 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-menu-choice)

;;}}}
;;{{{  toggle   

(defun emacspeak-widget-help-toggle (widget)
  "Summarize a toggle."
  (let* (
         (help-echo (emacspeak-widget-help-echo widget))(label (emacspeak-widget-label widget))
         (value (widget-value widget)))
    (concat label
            help-echo
            (if value " is on "
              " is off "))))



(widget-put (get 'toggle 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-toggle)

;;}}}
;;{{{  checklist

(defun emacspeak-widget-help-checklist  (widget)
  "Summarize a check list"
  (let* ((label (emacspeak-widget-label widget))
         (value (widget-value widget))
         (selections (cond
                      (value (prin1-to-string value))
                      (t " no items  "))))
    (put-text-property 0  (length selections)
                       'personality emacspeak-widget-value-personality selections)
    (concat label
            " has "
            selections 
            " checked ")))
     
      

(widget-put (get 'checklist 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-checklist)

;;}}}
;;{{{ choice-item

(defun emacspeak-widget-help-choice-item (widget)
  "Summarize a choice item"
  (let ((value  (widget-value widget))
        (label (emacspeak-widget-label widget))
        (help-echo (emacspeak-widget-help-echo widget)))
    (concat  label
             help-echo
             (when value (format " %s " value))
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
         (label (if sibling
                    (emacspeak-widget-label sibling)
                  (emacspeak-widget-label widget))))
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
         (sibling (widget-get-sibling widget))
         (label (if sibling
                    (emacspeak-widget-label sibling)
                  (emacspeak-widget-label widget))))
    (concat label
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
  (let* ((value (widget-value widget))
         (label (emacspeak-widget-label widget))
         (choice (widget-get widget :choice))
         (selected
          (cond
           (choice (widget-get choice :tag))
           (t (if value 
(prin1-to-string value)
                  " no item ")))))
    (put-text-property 0  (length selected)
                       'personality emacspeak-widget-value-personality selected)
    (concat label
            " is "
            selected)))

(widget-put (get 'radio-button-choice 'widget-type)
            :emacspeak-help 'emacspeak-widget-help-radio-button-choice)

;;}}}
;;{{{ editable-list

(defun emacspeak-widget-help-editable-list (widget)
  "Summarize a editable list"
  (let ((value (prin1-to-string (widget-value widget)))
        (label (emacspeak-widget-label widget))
        (help-echo (emacspeak-widget-help-echo widget)))
    (when value
      (put-text-property 0 (length value)
                         'personality emacspeak-widget-value-personality value))
    (concat label
            help-echo
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


(defadvice widget-specify-field-update (after emacspeak pre act comp)
  "Voiceify the field"
  (put-text-property (ad-get-arg 1) (ad-get-arg 2)
                     'personality
                     emacspeak-widget-edit-personality))

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
  "Update widget keymaps."
  (declare (special emacspeak-prefix
                    widget-keymap widget-field-keymap widget-text-keymap))
  (loop for map in
        (list widget-keymap
              widget-field-keymap
              widget-text-keymap)
        do
        (define-key map  emacspeak-prefix 'emacspeak-prefix-command)
        (define-key map  "\C-ee" 'widget-end-of-line)
        (define-key map "\M-h" 'emacspeak-widget-help)
        (define-key map "\M-p" 'emacspeak-widget-summarize-parent)
        (define-key map "\M-\C-m"
          'emacspeak-widget-update-from-minibuffer)))

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

(declaim (special widget-keymap
                  widget-field-keymap
                  widget-text-keymap))


;;}}}

;;}}}
;;{{{ voice widgets 

(define-widget 'voice  'menu-choice
  :help-echo "Voice selector"
  "Widget for selecting a voice.")

(define-widget 'personality 'item
  "Individual voice in a voice selector.")

(defun emacspeak-widget-create-voice-selector ()
  "Create a suitable voice selector widget."
  (declare (special dtk-voice-table))
  (let ((w
         (widget-create 'voice
                        :tag "voices")))
    (widget-put w :args 
                (loop for key being the hash-keys of dtk-voice-table 
                      collect
                      (list 'personality :value key )))
w))

;;}}}
(provide  'emacspeak-widget)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
