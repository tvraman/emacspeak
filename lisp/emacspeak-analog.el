;; emacspeak-analog.el --- Speech-enable analog -- a log viewer  -*- lexical-binding: t; -*-
;; $Id$
;; $Author: tv.raman.tv $
;; Description:  Emacspeak front-end for ANALOG log analyzer
;; Keywords: Emacspeak, analog
;;{{{  LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;; $Date: 2007-09-01 15:12:15 -0700 (Sat, 01 Sep 2007) $ |
;;  $Revision: 4150 $ |
;; Location undetermined
;; 

;;}}}
;;{{{  Copyright:

;; Copyright (C) 1995 -- 2021, T. V. Raman
;; All Rights Reserved.
;; 
;; This file is not part of GNU Emacs, but the same permissions apply.
;; 
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;{{{  Introduction:
;; Commentary:

;; Speech-enables package analog --convenient log analyzer 

;;}}}
;;{{{ required modules

;; Code:
(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
;;}}}
;;{{{ autoloads to help compiler

(autoload 'analog-get-entry-property "analog")

;;}}}

;;{{{ advice interactive commands
(defadvice analog (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-analog-update-edit-keys)
    (emacspeak-speak-mode-line)))

(defadvice analog-quit (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))
(defadvice analog-bury-buffer (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

(cl-loop for command in
         '(analog-next-group
           analog-previous-group
           analog-next-entry
           analog-previous-entry
           analog-refresh-display-buffer
           analog-toggle-timer-and-redisplay)
         do
         (eval
          `(defadvice ,command (after emacspeak pre act comp)
             "speak."
             (when (ems-interactive-p)
               (emacspeak-speak-line)
               (emacspeak-auditory-icon 'select-object)))))

;;}}}
;;{{{ voice setup 
(voice-setup-add-map
 '(
   (analog-entry-header-face voice-bolden)
   ))
;;}}}
;;{{{ field navigation

;; You can add a fields property that holds a list of field start
;; positions 
;; in analog-entries-list
;; emacspeak will use this to navigate using the arrow keys.

(defun emacspeak-analog-get-field-spec ()
  "Returns field specification if one defined for current entry.
Nil means no field specified."
  (save-excursion
    (let ((start (previous-single-property-change (point)
                                                  'analog-entry-start)))
      (when start
        (analog-get-entry-property
         (get-text-property
          (1- start)
          'analog-entry-start)
         'fields)))))

(defun emacspeak-analog-forward-field-or-char ()
  "Move forward to next field if field specification is available.
Otherwise move to next char.
Speak field or char moved to."
  (interactive)
  (let ((fields (emacspeak-analog-get-field-spec)))
    (cond
     (fields (emacspeak-analog-next-field fields)
             (emacspeak-analog-speak-field fields)
             (emacspeak-auditory-icon 'large-movement))
     (t (call-interactively 'forward-char)))))

(defun emacspeak-analog-backward-field-or-char ()
  "Move back to next field if field specification is available.
Otherwise move to previous char.
Speak field or char moved to."
  (interactive)
  (let ((fields (emacspeak-analog-get-field-spec)))
    (cond
     (fields (emacspeak-analog-previous-field fields)
             (emacspeak-analog-speak-field fields)
             (emacspeak-auditory-icon 'large-movement))
     (t (call-interactively 'backward-char)))))

(defun emacspeak-analog-speak-field (fields)
  "Speak field containing point."
  (save-excursion
    (let ((col (current-column))
          (start nil)
          (end nil)
          (left 0)
          (right  (cl-first fields)))
      (beginning-of-line)
      (while (and fields 
                  (<=  right col))
        (setq left right 
              right (pop fields)))
      (beginning-of-line)
      (forward-char left)
      (setq start (point))
      (cond
       ((or (null right)
            (<= right col))
        (beginning-of-line)
        (forward-char right)
        (setq start (point))
        (end-of-line)
        (setq end (point)))
       (t
        (beginning-of-line)
        (forward-char  right)
        (setq end (point))))
      (emacspeak-speak-region start end))))

(defun emacspeak-analog-speak-this-field ()
  "Speak current field."
  (interactive)
  (emacspeak-analog-speak-field (emacspeak-analog-get-field-spec)))

(defun emacspeak-analog-next-field (fields)
  "Move to next field."
  (let ((col (current-column))
        (end (cl-first fields)))
    (while (and fields 
                (<= end col))
      (setq end (pop fields)))  
    (cond
     ((> end col)
      (beginning-of-line)
      (forward-char end))
     (t (emacspeak-auditory-icon 'warn-user)))))

(defun emacspeak-analog-previous-field (fields)
  "Move to previous field."
  (let ((col (current-column))
        (prev 0)
        (start 0)
        (end (cl-first fields)))
    (while (and fields 
                (< end col))
      (setq prev start
            start end 
            end (pop fields)))
    (beginning-of-line)
    (cond
     ((<= start col)
      (forward-char start))
     (t (forward-char prev)))))

(defun emacspeak-analog-previous-line ()
  "Move up and speak current field."
  (interactive)
  (let ((fields (emacspeak-analog-get-field-spec)))
    (cond (fields
           (emacspeak-auditory-icon 'select-object)
           (forward-line -1)
           (emacspeak-analog-speak-field fields))
          (t (call-interactively 'previous-line)))))

(defun emacspeak-analog-next-line ()
  "Move down and speak current field."
  (interactive)
  (let ((fields (emacspeak-analog-get-field-spec)))
    (cond (fields
           (emacspeak-auditory-icon 'select-object)
           (forward-line 1)
           (emacspeak-analog-speak-field fields))
          (t (call-interactively 'next-line)))))

;;}}}
;;{{{ key bindings
(when (boundp 'analog-mode-map)
  (cl-declaim (special analog-mode-map))
  (define-key analog-mode-map '[left]
    'emacspeak-analog-backward-field-or-char)
  (define-key analog-mode-map '[right] 'emacspeak-analog-forward-field-or-char)
  (define-key analog-mode-map '[up] 'emacspeak-analog-previous-line)
  (define-key analog-mode-map '[down] 'emacspeak-analog-next-line))



;;}}}
(provide 'emacspeak-analog)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}
