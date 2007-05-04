;;; emacspeak-calendar.el --- Speech enable Emacs Calendar -- maintain a diary and appointments
;;; $Id$
;;; $Author$
;;; Description:  Emacspeak extensions to speech enable the calendar.
;;; Keywords: Emacspeak, Calendar, Spoken Output
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
;;;Copyright (C) 1995 -- 2007, T. V. Raman
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

;;; This module speech enables the Emacs Calendar.
;;; Speech enabling is not the same as speaking the screen:
;;; This is an excellent example of this.

;;}}}
;;{{{ required modules
;;; Code:
(require 'emacspeak-preamble)
(require 'calendar)
;;}}}
;;{{{  personalities
(voice-setup-add-map
 '(
   (calendar-today voice-lighten)
   (holiday-face voice-brighten-extra)
   (diary-face voice-bolden)
   ))

(defcustom emacspeak-calendar-mark-personality 'ursula
  "Personality to use when showing marked calendar entries."
  :type 'symbol
  :group 'emacspeak-calendar)

;;}}}
;;{{{  functions:
(defun emacspeak-calendar-sort-diary-entries ()
  "Sort entries in diary entries list."
  (declare (special diary-entries-list))
  (when(and  (boundp 'diary-entries-list)
             diary-entries-list)
    (setq diary-entries-list
          (sort  diary-entries-list
                 #'(lambda (a b )
                     (string-lessp (cadr a) (cadr b )))))))

(defsubst emacspeak-calendar-entry-marked-p()
  "Check if diary entry is marked. "
  (member 'diary
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
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-calendar-speak-date)))

(defadvice calendar-set-mark (after emacspeak
                                    pre act
                                    comp)
  "Speak date under point"
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-calendar-speak-date)))

(declaim (special diary-display-hook))

(unless (memq 'fancy-diary-display diary-display-hook)
  (add-hook 'diary-display-hook 'fancy-diary-display))
(add-hook 'calendar-mode-hook
          'gcal-emacs-calendar-setup)
(defadvice view-diary-entries (after emacspeak pre act)
  "Speak the diary entries."
  (when (interactive-p)
    (let ((voice-lock-mode t)
          (emacspeak-speak-messages nil))
      (cond
       ((buffer-live-p (get-buffer "*Fancy Diary Entries*"))
        (save-excursion
          (set-buffer "*Fancy Diary Entries*")
          (tts-with-punctuations "some"
                                 (emacspeak-speak-buffer))))
       (t (dtk-speak "No diary entries."))))))

(defadvice  mark-visible-calendar-date (after emacspeak pre act )
  "Use voice locking to mark date. "
  (let ((date (ad-get-arg 0 )))
    (if (calendar-date-is-legal-p date)
        (save-excursion
          (set-buffer calendar-buffer)
          (calendar-cursor-to-visible-date date)
          (ems-modify-buffer-safely
           (put-text-property  (1-(point)) (1+ (point))
                               'personality   emacspeak-calendar-mark-personality ))))))

(defvar emacspeak-calendar-mode-line-format
  '((calendar-date-string (calendar-current-date))  "Calendar")
  "Mode line format for calendar  with Emacspeak.")

(declaim (special calendar-mode-line-format))
(setq calendar-mode-line-format
      emacspeak-calendar-mode-line-format)

(defadvice calendar (after emacspeak pre act )
  "Announce yourself."
  (when (interactive-p)
    (let ((emacspeak-lazy-message-time 0))
      (emacspeak-auditory-icon 'open-object)
      (setq calendar-mode-line-format emacspeak-calendar-mode-line-format)
      (message "Welcome to the calendar"))))

(defadvice calendar-goto-date (after emacspeak pre act)
  "Speak the date. "
  (when (interactive-p )
    (emacspeak-calendar-speak-date ))
  (emacspeak-auditory-icon 'select-object))

(defadvice calendar-goto-today (after emacspeak pre act)
  "Speak the date. "
  (when (interactive-p )
    (emacspeak-calendar-speak-date ))

  (emacspeak-auditory-icon 'select-object))

(defadvice calendar-backward-day (after emacspeak pre act)
  "Speak the date. "
  (when (interactive-p)
    (emacspeak-calendar-speak-date )
    (emacspeak-auditory-icon 'select-object)))

(defadvice calendar-forward-day (after emacspeak pre act)
  "Speak the date. "
  (when (interactive-p)
    (emacspeak-calendar-speak-date )
    (emacspeak-auditory-icon 'select-object)))

(defadvice calendar-backward-week (after emacspeak pre act)
  "Speak the date. "
  (when (interactive-p)
    (emacspeak-calendar-speak-date )
    (emacspeak-auditory-icon 'large-movement)))

(defadvice calendar-forward-week (after emacspeak pre act)
  "Speak the date. "
  (when (interactive-p)
    (emacspeak-calendar-speak-date )
    (emacspeak-auditory-icon 'large-movement)))

(defadvice calendar-backward-month (after emacspeak pre act)
  "Speak the date. "
  (when (interactive-p)
    (emacspeak-calendar-speak-date )
    (emacspeak-auditory-icon 'large-movement)))

(defadvice calendar-forward-month (after emacspeak pre act)
  "Speak the date. "
  (when (interactive-p)
    (emacspeak-calendar-speak-date )
    (emacspeak-auditory-icon 'large-movement)))

(defadvice calendar-backward-year (after emacspeak pre act)
  "Speak the date. "
  (when (interactive-p)
    (emacspeak-calendar-speak-date )
    (emacspeak-auditory-icon 'large-movement)))

(defadvice calendar-forward-year (after emacspeak pre act)
  "Speak the date. "
  (when (interactive-p)
    (emacspeak-calendar-speak-date )
    (emacspeak-auditory-icon 'large-movement)))

(defadvice calendar-beginning-of-week (after emacspeak pre act)
  "Speak the date. "
  (when (interactive-p)
    (emacspeak-calendar-speak-date )
    (emacspeak-auditory-icon 'large-movement)))

(defadvice calendar-beginning-of-month (after emacspeak pre act)
  "Speak the date. "
  (when (interactive-p)
    (emacspeak-calendar-speak-date )
    (emacspeak-auditory-icon 'large-movement)))

(defadvice calendar-beginning-of-year (after emacspeak pre act)
  "Speak the date. "
  (when (interactive-p)
    (emacspeak-calendar-speak-date )
    (emacspeak-auditory-icon 'large-movement)))

(defadvice calendar-end-of-week (after emacspeak pre act)
  "Speak the date. "
  (when (interactive-p)
    (emacspeak-calendar-speak-date )
    (emacspeak-auditory-icon 'large-movement)))

(defadvice calendar-end-of-month (after emacspeak pre act)
  "Speak the date. "
  (when (interactive-p)
    (emacspeak-calendar-speak-date )
    (emacspeak-auditory-icon 'select-object)))

(defadvice calendar-end-of-year (after emacspeak pre act)
  "Speak the date. "
  (when (interactive-p)
    (emacspeak-calendar-speak-date )
    (emacspeak-auditory-icon 'select-object)))

(defadvice exit-calendar (after emacspeak pre act)
  "Speak modeline. "
  (when (interactive-p )
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice insert-block-diary-entry (before emacspeak pre act)
  "Speak the line. "
  (when (interactive-p)
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
  (declare (special emacspeak-calendar-user-input))
  ad-do-it
  (setq emacspeak-calendar-user-input ad-return-value)
  ad-return-value)
(defadvice insert-anniversary-diary-entry (before emacspeak pre act)
  "Speak the line. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Anniversary entry for %s"
             (calendar-date-string
              (calendar-cursor-to-date)))))

(defadvice insert-cyclic-diary-entry (after emacspeak pre act)
  "Speak the line. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Insert cyclic diary entry that repeats every
%s days"
             emacspeak-calendar-user-input)))

(defadvice insert-diary-entry (after emacspeak pre act)
  "Speak the line. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line )))

(defadvice insert-weekly-diary-entry (before emacspeak pre act)
  "Speak the line. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Weekly diary entry for %s"
             (calendar-day-name (calendar-cursor-to-date t)))))

(defadvice insert-yearly-diary-entry (before emacspeak pre act)
  "Speak the line. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Yearly diary entry for %s %s"
             (calendar-month-name(first (calendar-cursor-to-date t)))
             (second (calendar-cursor-to-date t)))))

(defadvice insert-monthly-diary-entry (before emacspeak pre act)
  "Speak the line. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Monthly diary entry for %s"
             (second (calendar-cursor-to-date t)))))

(defadvice calendar-cursor-holidays (after emacspeak pre act comp)
  "Speak the displayed holidays"
  (when (interactive-p)
    (emacspeak-speak-message-again)))

;;}}}
;;{{{  keymap
(eval-when (load)
  (emacspeak-keymap-remove-emacspeak-edit-commands
   calendar-mode-map))

(defun emacspeak-calendar-setup()
  "Set up appropriate bindings for calendar"
  (declare (special calendar-buffer calendar-mode-map emacspeak-prefix ))
  (save-excursion
    (set-buffer calendar-buffer)
    (local-unset-key emacspeak-prefix)
    (define-key calendar-mode-map  "\C-e." 'emacspeak-calendar-speak-date)
    (define-key calendar-mode-map  "\C-ee"
      'calendar-end-of-week))
  (add-hook 'initial-calendar-window-hook
            (function (lambda ()
                        ))))

(add-hook 'initial-calendar-window-hook 'emacspeak-calendar-setup t)

;;}}}
;;{{{  Appointments:

;;{{{ take over and speak the appointment

;;; For the present, we just take over and speak the appointment.
(eval-when (compile)
  (load-library "appt"))
(declaim (special appt-display-duration ))
(setq appt-display-duration 90)

(defun emacspeak-appt-speak-appointment (minutes-left new-time message )
  "Speak the appointment in addition to  displaying it visually."
  (let ((emacspeak-speak-messages-should-pause-ongoing-speech nil))
    (emacspeak-auditory-icon 'alarm)
    (dtk-pause t)
    (message "You have an appointment in %s minutes. %s"
             minutes-left message )
    (appt-disp-window minutes-left new-time  message)))

(defun emacspeak-appt-delete-display ()
  "Function to delete appointment message"
  (and (get-buffer appt-buffer-name)
       (save-excursion
         (set-buffer appt-buffer-name)
         (erase-buffer))))
(declaim (special appt-delete-window
                  appt-disp-window-function))

(setq appt-disp-window-function 'emacspeak-appt-speak-appointment)
(setq appt-delete-window 'emacspeak-appt-delete-display)
;;;###autoload
(defun emacspeak-appt-repeat-announcement ()
  "Speaks the most recently displayed appointment message if any."
  (interactive)
  (declare (special appt-buffer-name))
  (let  ((appt-buffer (get-buffer appt-buffer-name)))
    (cond
     ( appt-buffer
       (save-excursion
         (set-buffer  appt-buffer)
         (emacspeak-dtk-sync)
         (if (= (point-min) (point-max))
             (message  "No appointments are currently displayed")
           (dtk-speak (buffer-string )))))
     (t (message "You have no appointments "))))
  (emacspeak-dtk-sync))

;;}}}

                                        ; (defadvice appt-disp-window (before emacspeak activate compile)
                                        ;   "Speak the appointment."
                                        ;   (let ((emacspeak-speak-messages-should-pause-ongoing-speech nil))
                                        ;     (dtk-pause t)
                                        ;     (emacspeak-auditory-icon 'alarm)
                                        ;     (message "You have an appointment in %s minutes, %s"
                                        ;              (ad-get-arg 0)
                                        ;              (ad-get-arg 2))))

(defadvice appt-add (after emacspeak pre act )
  "Confirm that the alarm got set."
  (when (interactive-p)
    (let ((time (ad-get-arg 0))
          (message (ad-get-arg 1 )))
      (message "Set alarm %s at %s"
               message time ))))

;;}}}

(provide 'emacspeak-calendar)
;;{{{ emacs local variables

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
