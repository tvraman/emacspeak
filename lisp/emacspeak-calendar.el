;;; emacspeak-calendar.el --- Speech enable Emacs Calendar -- maintain a diary and appointments  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak extensions to speech enable the calendar.
;;; Keywords: Emacspeak, Calendar, Spoken Output
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2008-06-21 10:50:41 -0700 (Sat, 21 Jun 2008) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2021, T. V. Raman
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
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;{{{  Introduction:
;;; Commentary:
;;; This module speech enables the Emacs Calendar.
;;; Speech enabling is not the same as speaking the screen:
;;; This is an excellent example of the advantages of speech-enabled  interaction.
;;; Code:
;;}}}
;;{{{ required modules
;;; Code:
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'calendar)
(require 'solar)
(require 'g-utils)
(require 'appt)
;;}}}
;;{{{  personalities
(voice-setup-add-map
 '(
   (calendar-today voice-bolden)
   (holiday voice-brighten-extra)
   (diary voice-bolden)
   ))

(defvar emacspeak-calendar-mark-personality voice-bolden
  "Personality to use when showing marked calendar entries.")

;;}}}
;;{{{  functions:
(defun emacspeak-calendar-sort-diary-entries ()
  "Sort entries in diary entries list."
  (cl-declare (special diary-entries-list))
  (when(and  (boundp 'diary-entries-list)
             diary-entries-list)
    (setq diary-entries-list
          (sort  diary-entries-list
                 #'(lambda (a b)
                     (string-lessp (cadr a) (cadr b)))))))

(defun emacspeak-calendar-entry-marked-p()
  "Check if diary entry is marked. "
  (memq 'diary
          (delq nil
                (mapcar
                 #'(lambda (overlay)
                     (overlay-get overlay 'face))
                 (overlays-at (point))))))

(defun emacspeak-calendar-speak-date()
  "Speak the date under point when called in Calendar Mode. "
  (interactive)
  (let ((date (calendar-date-string (calendar-cursor-to-date t))))
    (tts-with-punctuations 'some
                           (cond
                            ((emacspeak-calendar-entry-marked-p)
                             (dtk-speak-using-voice emacspeak-calendar-mark-personality date))
                            (t (dtk-speak date))))))

;;}}}
;;{{{  Advice:
(defadvice calendar-exchange-point-and-mark (after emacspeak
                                                   pre act comp)
  "Speak date under point"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-calendar-speak-date)))

(defadvice calendar-set-mark (after emacspeak
                                    pre act
                                    comp)
  "Speak date under point"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-calendar-speak-date)))

(add-hook 'calendar-mode-hook
          'emacspeak-calendar-setup)

(cl-loop for f in
         '(fancy-diary-display simple-diary-display
                               diary-list-entries)
         do
         (eval
          `(defadvice ,f (around emacspeak pre act com)
             "Silence messages."
             (let ((emacspeak-speak-messages (not (ems-interactive-p))))
               ad-do-it))))

(defadvice view-diary-entries (after emacspeak pre act comp)
  "Speak the diary entries."
  (when (ems-interactive-p)
    (ems-with-messages-silenced
     (cond
      ((buffer-live-p (get-buffer "*Fancy Diary Entries*"))
       (save-current-buffer
         (set-buffer "*Fancy Diary Entries*")
         (tts-with-punctuations
          "some"
          (emacspeak-speak-buffer))))
      (t (dtk-speak "No diary entries."))))))

(defadvice  mark-visible-calendar-date (after emacspeak pre act comp)
  "Use voice locking to mark date. "
  (let ((date (ad-get-arg 0)))
    (if (calendar-date-is-valid-p date)
        (save-current-buffer
          (set-buffer calendar-buffer)
          (calendar-cursor-to-visible-date date)
          (with-silent-modifications
            (put-text-property  (1-(point)) (1+ (point))
                                'personality   emacspeak-calendar-mark-personality))))))

(defvar emacspeak-calendar-mode-line-format
  '((calendar-date-string (calendar-current-date))  "Calendar")
  "Mode line format for calendar  with Emacspeak.")

(defvar emacspeak-calendar-header-line-format
  '((:eval (calendar-date-string (calendar-cursor-to-date t))))
  "Header line used by Emacspeak in calendar.")

(cl-declaim (special calendar-mode-line-format))
(setq calendar-mode-line-format
      emacspeak-calendar-mode-line-format)

(defadvice calendar (after emacspeak pre act comp)
  "Announce yourself."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (when emacspeak-use-header-line
      (setq header-line-format
            '((:eval (calendar-date-string (calendar-cursor-to-date t))))))
    (setq calendar-mode-line-format
          emacspeak-calendar-mode-line-format)
    (tts-with-punctuations 'some
                           (emacspeak-speak-mode-line))))

(defadvice calendar-goto-date (after emacspeak pre act comp)
  "Speak the date. "
  (when (ems-interactive-p)
    (emacspeak-calendar-speak-date))
  (emacspeak-auditory-icon 'select-object))

(defadvice calendar-goto-today (after emacspeak pre act comp)
  "Speak the date. "
  (when (ems-interactive-p)
    (emacspeak-calendar-speak-date))

  (emacspeak-auditory-icon 'select-object))

(defadvice calendar-backward-day (after emacspeak pre act comp)
  "Speak the date. "
  (when (ems-interactive-p)
    (emacspeak-calendar-speak-date)
    (emacspeak-auditory-icon 'select-object)))

(defadvice calendar-forward-day (after emacspeak pre act comp)
  "Speak the date. "
  (when (ems-interactive-p)
    (emacspeak-calendar-speak-date)
    (emacspeak-auditory-icon 'select-object)))

(defadvice calendar-backward-week (after emacspeak pre act comp)
  "Speak the date. "
  (when (ems-interactive-p)
    (emacspeak-calendar-speak-date)
    (emacspeak-auditory-icon 'paragraph)))

(defadvice calendar-forward-week (after emacspeak pre act comp)
  "Speak the date. "
  (when (ems-interactive-p)
    (emacspeak-calendar-speak-date)
    (emacspeak-auditory-icon 'paragraph)))

(defadvice calendar-backward-month (after emacspeak pre act comp)
  "Speak the date. "
  (when (ems-interactive-p)
    (emacspeak-calendar-speak-date)
    (emacspeak-auditory-icon 'section)))

(defadvice calendar-forward-month (after emacspeak pre act comp)
  "Speak the date. "
  (when (ems-interactive-p)
    (emacspeak-calendar-speak-date)
    (emacspeak-auditory-icon 'section)))

(defadvice calendar-backward-year (after emacspeak pre act comp)
  "Speak the date. "
  (when (ems-interactive-p)
    (emacspeak-calendar-speak-date)
    (emacspeak-auditory-icon 'large-movement)))

(defadvice calendar-forward-year (after emacspeak pre act comp)
  "Speak the date. "
  (when (ems-interactive-p)
    (emacspeak-calendar-speak-date)
    (emacspeak-auditory-icon 'large-movement)))

(defadvice calendar-beginning-of-week (after emacspeak pre act comp)
  "Speak the date. "
  (when (ems-interactive-p)
    (emacspeak-calendar-speak-date)
    (emacspeak-auditory-icon 'paragraph)))

(defadvice calendar-beginning-of-month (after emacspeak pre act comp)
  "Speak the date. "
  (when (ems-interactive-p)
    (emacspeak-calendar-speak-date)
    (emacspeak-auditory-icon 'section)))

(defadvice calendar-beginning-of-year (after emacspeak pre act comp)
  "Speak the date. "
  (when (ems-interactive-p)
    (emacspeak-calendar-speak-date)
    (emacspeak-auditory-icon 'large-movement)))

(defadvice calendar-end-of-week (after emacspeak pre act comp)
  "Speak the date. "
  (when (ems-interactive-p)
    (emacspeak-calendar-speak-date)
    (emacspeak-auditory-icon 'paragraph)))

(defadvice calendar-end-of-month (after emacspeak pre act comp)
  "Speak the date. "
  (when (ems-interactive-p)
    (emacspeak-calendar-speak-date)
    (emacspeak-auditory-icon 'section)))

(defadvice calendar-end-of-year (after emacspeak pre act comp)
  "Speak the date. "
  (when (ems-interactive-p)
    (emacspeak-calendar-speak-date)
    (emacspeak-auditory-icon 'large-movement)))
(cl-loop for f in
         '(exit-calendar calendar-exit calendar-quit)
         do
         (eval
          `(defadvice ,f (after emacspeak pre act comp)
             "Speak modeline. "
             (when (ems-interactive-p)
               (emacspeak-auditory-icon 'close-object)
               (emacspeak-speak-mode-line)))))

(defadvice insert-block-diary-entry (before emacspeak pre act comp)
  "Speak the line. "
  (when (ems-interactive-p)
    (let*
        ((cursor (calendar-cursor-to-date t))
         (mark (or (car calendar-mark-ring)
                   (error "No mark set in this buffer")))
         (start)
         (end))
      (if (< (calendar-absolute-from-gregorian mark)
             (calendar-absolute-from-gregorian cursor))
          (setq start mark
                end cursor)
        (setq start cursor
              end mark))
      (emacspeak-auditory-icon 'open-object)
      (message "Block diary entry from  %s to %s"
               (calendar-date-string start nil t)
               (calendar-date-string end nil t)))))

(defvar emacspeak-calendar-user-input nil
  "Records last user input to calendar")

(defadvice calendar-read (around emacspeak pre act comp)
  "Record what was read"
  (cl-declare (special emacspeak-calendar-user-input))
  ad-do-it
  (setq emacspeak-calendar-user-input ad-return-value)
  ad-return-value)
(defadvice insert-anniversary-diary-entry (before emacspeak pre act comp)
  "Speak the line. "
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Anniversary entry for %s"
             (calendar-date-string
              (calendar-cursor-to-date)))))

(defadvice insert-cyclic-diary-entry (after emacspeak pre act comp)
  "Speak the line. "
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Insert cyclic diary entry that repeats every
%s days"
             emacspeak-calendar-user-input)))

(defadvice insert-diary-entry (after emacspeak pre act comp)
  "Speak the line. "
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice insert-weekly-diary-entry (before emacspeak pre act comp)
  "Speak the line. "
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Weekly diary entry for %s"
             (calendar-day-name (calendar-cursor-to-date t)))))

(defadvice insert-yearly-diary-entry (before emacspeak pre act comp)
  "Speak the line. "
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Yearly diary entry for %s %s"
             (calendar-month-name(cl-first (calendar-cursor-to-date t)))
             (cl-second (calendar-cursor-to-date t)))))

(defadvice insert-monthly-diary-entry (before emacspeak pre act comp)
  "Speak the line. "
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Monthly diary entry for %s"
             (cl-second (calendar-cursor-to-date t)))))

(defadvice calendar-cursor-holidays (after emacspeak pre act comp)
  "Speak the displayed holidays"
  (when (ems-interactive-p)
    (emacspeak-speak-message-again)))

(defadvice mark-diary-entries (around emacspeak pre act comp)
  "Silence messages."
  (ems-with-messages-silenced
   ad-do-it
   ad-return-value))

;;}}}
;;{{{ Global sunrise/sunset wizard:


(defun emacspeak-calendar-sunrise-sunset (address &optional arg)
  "Display sunrise/sunset for specified address."
  (interactive
   (list
    (read-from-minibuffer "Address: ")
    current-prefix-arg))
  (cl-declare (special calendar-standard-time-zone-name
                       calendar-longitude calendar-latitude))
  (let* ((geo (gmaps-address-geocode address))
         (calendar-latitude (g-json-get 'lat geo))
         (calendar-longitude (g-json-get 'lng geo))
         (calendar-time-zone
          (solar-get-number
           "Enter difference from Coordinated Universal Time (in minutes): "))
         (calendar-standard-time-zone-name
          (cond ((zerop calendar-time-zone) "UTC")
                ((< calendar-time-zone 0)
                 (format "UTC%dmin" calendar-time-zone))
                (t (format "UTC+%dmin" calendar-time-zone))))
         (date (if arg (calendar-read-date) (calendar-current-date)))
         (date-string (calendar-date-string date t))
         (time-string (solar-sunrise-sunset-string date)))
    (message "%s: %s at %s" date-string time-string address)))



;;}}}
;;{{{  keymap



(defun emacspeak-calendar-setup()
  "Set up appropriate bindings for calendar"
  (cl-declare (special calendar-buffer calendar-mode-map emacspeak-prefix))
  (save-current-buffer
    (set-buffer calendar-buffer)
    (local-unset-key emacspeak-prefix)
    (define-key calendar-mode-map "v" 'view-diary-entries)
    (define-key calendar-mode-map "\M-s" 'emacspeak-calendar-sunrise-sunset)
    (define-key calendar-mode-map  "\C-e." 'emacspeak-calendar-speak-date)
    (define-key calendar-mode-map  "\C-ee"
      'calendar-end-of-week)))

                                        ;(add-hook 'calendar-initial-window-hook 'emacspeak-calendar-setup t)

;;}}}
;;{{{  Appointments:

;;{{{ take over and speak the appointment

;;; For the present, we just take over and speak the appointment.

(cl-declaim (special appt-display-duration))
(setq appt-display-duration 90)

(defun emacspeak-appt-speak-appointment (minutes-left new-time message)
  "Speak the appointment in addition to  displaying it visually."
  (emacspeak-auditory-icon 'alarm)
  (message "You have an appointment in %s minutes. %s"
           minutes-left message)
  (appt-disp-window minutes-left new-time  message))

(defun emacspeak-appt-delete-display ()
  "Function to delete appointment message"
  (cl-declare (special appt-buffer-name))
  (and (get-buffer appt-buffer-name)
       (save-current-buffer
         (set-buffer appt-buffer-name)
         (erase-buffer))))

(cl-declaim (special appt-delete-window
                     appt-disp-window-function))

(setq appt-disp-window-function 'emacspeak-appt-speak-appointment)
(setq appt-delete-window 'emacspeak-appt-delete-display)

(defun emacspeak-appt-repeat-announcement ()
  "Speaks the most recently displayed appointment message if any."
  (interactive)
  (cl-declare (special appt-buffer-name))
  (let  ((appt-buffer (get-buffer appt-buffer-name)))
    (cond
     (appt-buffer
      (save-current-buffer
        (set-buffer  appt-buffer)
        (if (= (point-min) (point-max))
            (message  "No appointments are currently displayed")
          (dtk-speak (buffer-string)))))
     (t (dtk-speak-and-echo "You have no appointments ")))))

;;}}}

(defadvice appt-add (after emacspeak pre act comp)
  "Confirm that the alarm got set."
  (when (ems-interactive-p)
    (let ((time (ad-get-arg 0))
          (message (ad-get-arg 1)))
      (message "Set alarm %s at %s"
               message time))))

;;}}}
;;{{{ Use GWeb if available for configuring sunrise/sunset coords

(defun emacspeak-calendar-setup-sunrise-sunset ()
  "Set up geo-coordinates using Google Maps reverse geocoding.
To use, configure variable gmaps-my-address via M-x customize-variable."
  (interactive)
  (cl-declare (special  gmaps-my-address gmaps-my-location
                        calendar-latitude calendar-longitude))
  (cond
   ((null gmaps-my-location)
    (message "First customize gmaps-my-address."))
   (t
    (setq
     calendar-latitude
     (g-json-get 'lat (gmaps-address-geocode gmaps-my-address))
     calendar-longitude
     (g-json-get 'lng (gmaps-address-geocode gmaps-my-address))))))

(defadvice calendar-sunrise-sunset (around emacspeak pre act comp)
  "Like calendar's sunrise-sunset, but speaks location intelligently."
  (cl-declare (special gmaps-my-address))
  (cond
   ((and (boundp 'gmaps-my-address)
         gmaps-my-address
         (ems-interactive-p))
    (let ((date (calendar-cursor-to-date t)))
      (message "%s at %s"
               (solar-sunrise-sunset-string date 'nolocation)
               gmaps-my-address)))
   (t ad-do-it)))

;;}}}
;;{{{ Lunar Phases

(cl-loop for f in
         '(calendar-lunar-phases lunar-phases phases-of-moon)
         do
         (eval
          `(defadvice ,f (after emacspeak pre act comp)
             "speak."
             (when (ems-interactive-p)
               (with-current-buffer lunar-phases-buffer
                 (emacspeak-auditory-icon 'open-object)
                 (emacspeak-speak-buffer))))))

(cl-loop for f in
         '(holidays calendar-list-holidays)
         do
         (eval
          `(defadvice ,f (after emacspeak pre act comp)
             "speak."
             (when (ems-interactive-p)
               (with-current-buffer holiday-buffer
                 (emacspeak-auditory-icon 'open-object)
                 (emacspeak-speak-buffer))))))

;;}}}
(provide 'emacspeak-calendar)
;;{{{ emacs local variables

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
