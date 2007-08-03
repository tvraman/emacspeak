;;; emacspeak-advice.el --- Advice all core Emacs functionality to speak intelligently
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Core advice forms that make emacspeak work
;;; Keywords: Emacspeak, Speech, Advice, Spoken  output
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-05-09 10:50:12 -0600 (Wed, 09 May 2007) $ |
;;;  $Revision: 4550 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;;Copyright (C) 1995 -- 2007, T. V. Raman
;;; Copyright (c) 1995, 1996,  1997 by T. V. Raman
;;; All Rights Reserved.
;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  Introduction:

;;; Commentary:

;;; This module defines the advice forms for making the core of Emacs speak
;;; Advice forms that are specific to Emacs subsystems do not belong here!
;;; I violate this at present by advicing completion comint and
;;; shell here.
;;
;;; Code:

;;}}}
;;{{{ Required modules

(require 'advice)
(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'voice-setup)
(require 'dtk-speak)
(require 'emacspeak-speak)
(require 'emacspeak-sounds)

;;}}}
;;{{{  advice cursor movement commands to speak

(defadvice next-line (before emacspeak pre act com)
  "Produce auditory icon  if we cant move."
  (when (and (interactive-p)
             (save-excursion
               (end-of-line)
               (eobp)))
    (emacspeak-auditory-icon 'warn-user)))

(defadvice previous-line (before emacspeak pre act com)
  "Produce auditory icon  if we cant move."
  (when (and (interactive-p)
             (save-excursion
               (beginning-of-line)
               (bobp)))
    (emacspeak-auditory-icon 'warn-user)))

(defadvice next-line (after emacspeak pre act)
  "Speak line that you just moved to."
  (when (interactive-p)
    (emacspeak-speak-line  )))

(defadvice previous-line (after emacspeak pre act)
  "Speak line that you just moved to."
  (when (interactive-p)
    (emacspeak-speak-line  )))

(defadvice forward-word (after emacspeak pre act)
  "Speak the word you just moved to."
  (when (interactive-p)
    (skip-syntax-forward " ")
    (emacspeak-speak-word )))

(defadvice backward-word (after emacspeak pre act)
  "Speak the word you just moved to."
  (when (interactive-p) (emacspeak-speak-word )))

(defadvice next-buffer (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (dtk-speak (buffer-name))))

(defadvice previous-buffer (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (dtk-speak (buffer-name))))

(defadvice beginning-of-buffer (after emacspeak pre act)
  "Speak the line."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line  )))

(defadvice end-of-buffer (after emacspeak pre act)
  "Speak the line."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line   )))

(defadvice back-to-indentation (after emacspeak pre act)
  "Speak the entire line."
  (when (interactive-p) (emacspeak-speak-line  )))

(defadvice lisp-indent-line (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-speak-line )))

(defadvice tab-to-tab-stop (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-speak-current-column)))

(defadvice forward-sentence (after emacspeak pre act)
  "Speak  sentence  after moving."
  (when (interactive-p) (emacspeak-speak-sentence    )))

(defadvice backward-sentence (after emacspeak pre act)
  "Speak  sentence  after moving."
  (when (interactive-p) (emacspeak-speak-sentence    )))

(defadvice forward-sexp (around emacspeak pre act)
  "Speak sexp after moving."
  (if (interactive-p)
      (let ((start (point))
            (same-line nil))
        ad-do-it
        (emacspeak-auditory-icon 'large-movement)
        (skip-syntax-forward " ")
        (setq same-line (count-lines start (point)))
        (cond
         ((> same-line 1)
          (emacspeak-speak-line))
         (t (emacspeak-speak-sexp))))
    ad-do-it)
  ad-return-value)

(defadvice backward-sexp (around  emacspeak pre act )
  "Speak sexp  after moving.
If you move more than a line,
  only speak the target line."
  (if   (interactive-p)
      (let ((start (point))
            (same-line nil))
        ad-do-it
        (emacspeak-auditory-icon 'large-movement)
        (setq same-line (count-lines (point) start ))
        (cond
         ((> same-line 1) (emacspeak-speak-line))
         (t (emacspeak-speak-region start (point )))))
    ad-do-it)
  ad-return-value)

(defadvice forward-paragraph (after emacspeak pre act )
  "Speak the paragraph."
  (when(interactive-p)
    (emacspeak-speak-paragraph)))

(defadvice backward-paragraph (after emacspeak pre act )
  "Speak the paragraph."
  (when(interactive-p)
    (emacspeak-speak-paragraph  nil )))

(defadvice forward-list (around  emacspeak pre act)
  "Speak the list.
If you moved more than a line,
  only speak the target line."
  (if   (interactive-p)
      (let ((start (point))
            (same-line nil))
        ad-do-it
        (setq same-line (count-lines (point) start ))
        (cond
         ((> same-line 1) (emacspeak-speak-line))
         (t (emacspeak-speak-region start (point )))))
    ad-do-it)
  ad-return-value)

(defadvice backward-list (around  emacspeak pre act)
  "Speak the list.
If you moved more than a line,
  just speak the target line."
  (if   (interactive-p)
      (let ((start (point))
            (same-line nil))
        ad-do-it
        (setq same-line (count-lines (point) start ))
        (cond
         ((> same-line 1) (emacspeak-speak-line))
         (t (emacspeak-speak-region start (point )))))
    ad-do-it)
  ad-return-value)

(defadvice up-list (around  emacspeak pre act)
  "Speak the list.
If you moved more than a line,
  only speak the target line"
  (if   (interactive-p)
      (let ((start (point))
            (same-line nil))
        ad-do-it
        (setq same-line (count-lines (point) start ))
        (cond
         ((> same-line 1) (emacspeak-speak-line))
         (t (emacspeak-speak-region start (point )))))
    ad-do-it)
  ad-return-value)

(defadvice backward-up-list (around  emacspeak pre act)
  "Speak the list.
If you moved more than a line,
  only speak the target line"
  (if   (interactive-p)
      (let ((start (point))
            (same-line nil))
        ad-do-it
        (setq same-line (count-lines (point) start ))
        (cond
         ((> same-line 1) (emacspeak-speak-line))
         (t (emacspeak-speak-region start (point )))))
    ad-do-it)
  ad-return-value)

(defadvice down-list (around  emacspeak pre act)
  "Speak the list.
If you moved more than a line,
  only speak the target line."
  (if   (interactive-p)
      (let ((start (point))
            (same-line nil))
        ad-do-it
        (setq same-line (count-lines (point) start ))
        (cond
         ((> same-line 1) (emacspeak-speak-line))
         (t (emacspeak-speak-region start (point )))))
    ad-do-it)
  ad-return-value)

(defadvice forward-page (after emacspeak pre act)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (emacspeak-speak-page )))

(defadvice backward-page (after emacspeak pre act)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (emacspeak-speak-page )))

(defadvice scroll-up (after emacspeak pre act comp)
  "Speak the next screenful."
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (dtk-speak (emacspeak-get-window-contents))))

(defadvice scroll-down (after emacspeak pre act comp)
  "Speak the screenful."
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (dtk-speak (emacspeak-get-window-contents))))

(defadvice  beginning-of-defun (after emacspeak pre act)
  "Speak the line."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice  end-of-defun (after emacspeak pre act)
  "Speak the line."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

;;}}}
;;{{{ Advise modify case commands to speak

(defadvice upcase-word (around emacspeak pre act comp)
  "Provide a tone to indicate that we upper cased the current word.
Speak the word that point lands on after the action
is done.  If `upcase-word' is called with a negative argument,
then point does not move.  In this case, we speak the words
that were upper cased."
  (cond
   ((interactive-p)
    (when dtk-stop-immediately (dtk-stop))
    (let ((dtk-stop-immediately nil))
      (dtk-tone 800 50 'force)
      (cond
       ((and (numberp current-prefix-arg)
             (minusp current-prefix-arg))
        ad-do-it
        (let ((start (point)))
          (save-excursion
            (forward-word current-prefix-arg)
            (emacspeak-speak-region start (point)))))
       (t ad-do-it
          (save-excursion
            (skip-syntax-forward " ")
            (if(eobp)
                (message "Upper cased final word in buffer")
              (emacspeak-speak-word)))))))
   (t ad-do-it))
  ad-return-value)

(defadvice downcase-word (around emacspeak pre act comp)
  "Provide a tone to indicate that we down cased the current word.
Speak the word that point lands on after the action
is done.  If `downcase-word' is called with a negative
argument, then point does not move.  In this case, we speak
the words that were down cased."

  (cond
   ((interactive-p)
    (when dtk-stop-immediately (dtk-stop))
    (let ((dtk-stop-immediately nil))
      (dtk-tone 600 50 'force)
      (cond
       ((and (numberp current-prefix-arg)
             (minusp current-prefix-arg))
        ad-do-it
        (let ((start (point)))
          (save-excursion
            (forward-word current-prefix-arg)
            (emacspeak-speak-region start (point)))))
       (t ad-do-it
          (save-excursion
            (skip-syntax-forward " ")
            (if(eobp)
                (message "Lower cased final word in buffer")
              (emacspeak-speak-word)))))))
   (t ad-do-it))
  ad-return-value)

(defadvice capitalize-word (around emacspeak pre act comp)
  "Provide a tone to indicate that we capitalized the current word.
Speak the word that point lands on after the action
is done.  If `capitalize-word' is called with a negative
argument, then point does not move.  In this case, we speak
the words that were capitalized."
  (cond
   ((interactive-p)
    (when dtk-stop-immediately (dtk-stop))
    (let ((dtk-stop-immediately nil))
      (dtk-tone 700 50 'force)
      (cond
       ((and (numberp current-prefix-arg)
             (minusp current-prefix-arg))
        ad-do-it
        (let ((start (point)))
          (save-excursion
            (forward-word current-prefix-arg)
            (emacspeak-speak-region start (point)))))
       (t ad-do-it
          (save-excursion
            (skip-syntax-forward " ")
            (if(eobp)
                (message "Capitalized final word in buffer")
              (emacspeak-speak-word)))))))
   (t ad-do-it))
  ad-return-value)

;;}}}
;;{{{  Advice deletion commands:
;;;###autoload
(defcustom emacspeak-delete-char-speak-deleted-char t
  "*T means `delete-char' speaks char that was deleted."
  :group 'emacspeak-speak
  :type 'boolean)
;;;###autoload
(defcustom emacspeak-backward-delete-char-speak-current-char nil
  "*T means `backward-delete-char' speaks char that becomes
current after deletion."
  :group 'emacspeak-speak
  :type 'boolean)

(defadvice delete-backward-char (around emacspeak pre act)
  "Speak character you're deleting."
  (cond
   ((interactive-p )
    (dtk-tone 500 30 'force)
    (emacspeak-speak-this-char (preceding-char ))
    ad-do-it)
   (t ad-do-it))
  ad-return-value)

(defadvice delete-char (around emacspeak pre act)
  "Speak character you're deleting."
  (cond
   ((interactive-p )
    (dtk-tone 500 30 'force)
    (and emacspeak-delete-char-speak-deleted-char
         (emacspeak-speak-char t))
    ad-do-it)
   (t ad-do-it))
  ad-return-value)

(defadvice backward-delete-char-untabify (around emacspeak pre act)
  "Speak character you're deleting."
  (cond
   ((interactive-p )
    (dtk-tone 500 30 'force)
    (emacspeak-speak-this-char (preceding-char ))
    ad-do-it)
   (t ad-do-it))
  ad-return-value)

(defadvice backward-delete-char (around emacspeak pre act)
  "Speak character you're deleting."
  (cond
   ((interactive-p )
    (dtk-tone 500 30 'force)
    (emacspeak-speak-this-char (preceding-char ))
    ad-do-it)
   (t ad-do-it))
  ad-return-value)

(defadvice kill-word (before emacspeak pre act )
  "Speak word before killing it."
  (when (interactive-p )
    (save-excursion
      (skip-syntax-forward " ")
      (when dtk-stop-immediately (dtk-stop))
      (let ((dtk-stop-immediately nil))
        (dtk-tone 500 30)
        (emacspeak-speak-word 1 )))))

(defadvice backward-kill-word (before emacspeak pre act)
  "Speak word before killing it."
  (when (interactive-p )
    (when dtk-stop-immediately (dtk-stop))
    (let ((start (point ))
          (dtk-stop-immediately nil))
      (save-excursion
        (forward-word -1)
        (dtk-tone 500 30)
        (emacspeak-speak-region (point) start )))))

;;; Large deletions also produce auditory icons if possible

(defadvice kill-line(before emacspeak pre act)
  "Speak line before killing it. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (when dtk-stop-immediately (dtk-stop))
    (let ((dtk-stop-immediately nil))
      (dtk-tone 500 30)
      (emacspeak-speak-line 1))))

(defadvice kill-sexp (before emacspeak pre act )
  "Speak the sexp you killed."
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (when dtk-stop-immediately (dtk-stop))
    (let ((dtk-stop-immediately nil))
      (dtk-tone 500 30)
      (emacspeak-speak-sexp 1 ))))

(defadvice kill-sentence (before emacspeak pre act )
  "Speak the line  you killed."
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (when dtk-stop-immediately (dtk-stop))
    (let ((dtk-stop-immediately nil))
      (dtk-tone 500 30)
      (emacspeak-speak-line 1 ))))

(defadvice delete-blank-lines (before   emacspeak  pre act )
  "Provide auditory feedback."
  (when (interactive-p)
    (let (thisblank singleblank)
      (save-match-data
        (save-excursion
          (beginning-of-line)
          (setq thisblank (looking-at "[ \t]*$"))
          ;; Set singleblank if there is just one blank line here.
          (setq singleblank
                (and thisblank
                     (not (looking-at "[ \t]*\n[ \t]*$"))
                     (or (bobp)
                         (progn (forward-line -1)
                                (not (looking-at "[ \t]*$"))))))))
      (cond
       ((and thisblank singleblank )
        (message "Deleting current blank line"))
       (  thisblank (message "Deleting surrounding  blank lines"))
       (t (message "Deleting possible subsequent blank lines"))))))

;;}}}
;;{{{  advice insertion commands to speak.

;;; there appears to be a bug in newer emacsuns e.g. 19.30 when using
;;; completion.el
;;;We'll fix it hear for the general good.

;; (defadvice completion-separator-self-insert-command (around
;;                                                      fix-bug
;;                                                      pre act
;;                                                      comp)
;;   "This fixes a bug in completion under Emacs 19.34."
;;   (condition-case nil
;;       ad-do-it
;;     (error (set-syntax-table cmpl-saved-syntax)
;;            (emacspeak-self-insert-command last-input-char ))))

(defadvice completion-separator-self-insert-autofilling
  (around fix-bug pre act comp)
  "This fixes a bug in completion under Emacs 19.34."
  (condition-case nil
      ad-do-it
    (error (set-syntax-table cmpl-saved-syntax)
           (emacspeak-self-insert-command last-input-char ))))

(defadvice completion-separator-self-insert-autofilling (after emacspeak pre act)
  "Speak what wascompleted followed by the next completion."
  (declare (special emacspeak-word-echo))
  (when (and emacspeak-word-echo  (interactive-p ))
    (condition-case nil
        (save-excursion
          (skip-syntax-backward " ")
          (backward-char 1)
          (emacspeak-speak-word))
      (error nil ))))

(defadvice completion-separator-self-insert-command (after emacspeak act comp)
  "Speak char after inserting it."
  (declare (special emacspeak-character-echo))
  (when (and emacspeak-character-echo  (interactive-p))
    (emacspeak-speak-this-char
     (preceding-char ))))

(defadvice quoted-insert  (after emacspeak pre act )
  "Speak the character that was inserted."
  (when (interactive-p)
    (emacspeak-speak-this-char
     (preceding-char ))))

;;}}}
;;{{{  advice minibuffer to speak

(defadvice read-event (before emacspeak pre act comp)
  "Speak the prompt."
  (if (ad-get-arg 0)
      (tts-with-punctuations 'all
                             (dtk-speak (ad-get-arg 0)))))

(defadvice previous-history-element (after emacspeak pre act)
  "Speak the history element just inserted."
  (when (interactive-p)
    (when dtk-stop-immediately (dtk-stop))
    (emacspeak-auditory-icon 'select-object)
    (tts-with-punctuations 'all
                           (emacspeak-speak-current-field ))))

(defadvice next-history-element (after emacspeak  pre act)
  "Speak the history element just inserted."
  (when (interactive-p)
    (when dtk-stop-immediately (dtk-stop))
    (emacspeak-auditory-icon 'select-object)
    (tts-with-punctuations 'all
                           (emacspeak-speak-current-field ))))

(defadvice previous-matching-history-element (after emacspeak pre act)
  "Speak the history element just inserted."
  (when (interactive-p)
    (when dtk-stop-immediately (dtk-stop))
    (emacspeak-auditory-icon 'select-object)
    (tts-with-punctuations 'all
                           (emacspeak-speak-current-field ))))

(defadvice next-matching-history-element (after emacspeak pre act)
  "Speak the history element just inserted."
  (when (interactive-p)
    (when dtk-stop-immediately (dtk-stop))
    (emacspeak-auditory-icon 'select-object)
    (tts-with-punctuations 'all
                           (emacspeak-speak-current-field ))))

(defvar emacspeak-last-message nil
  "Holds the last output generated by the Emacs 'message function.")

(defvar emacspeak-lazy-message-time 0
  "Records when we last spoke a message.")

;;;###autoload
(defcustom emacspeak-speak-messages-pause
  nil
  "* Option to make messages pause speech.
If t then all messages will pause ongoing speech if any
before the message is spoken."
  :group 'emacspeak-speak
  :type 'boolean)

(defadvice momentary-string-display (around emacspeak pre act
                                            comp)
  "Provide spoken feedback."
  (let ((emacspeak-speak-messages nil)
        (msg (ad-get-arg 0))
        (exit (ad-get-arg 2)))
    (dtk-speak
     (format "%s %s"
             msg
             (format "Press %s to exit "
                     (if exit
                         (format "%c" exit)
                       "space"))))
    ad-do-it))

(defadvice message (around  emacspeak pre act)
  "Speak the message."
  (declare (special emacspeak-last-message
                    emacspeak-speak-messages-pause
                    emacspeak-speak-messages emacspeak-lazy-message-time))
  (let ((dtk-stop-immediately t )
        (inhibit-read-only t))
    ad-do-it
    (setq emacspeak-last-message ad-return-value )
    (when (and
           emacspeak-speak-messages  ; speaking messages
           ad-return-value           ;we really do have a message
           (/= emacspeak-lazy-message-time ;; previous message not recent
               (setq emacspeak-lazy-message-time (nth 1  (current-time)))))
      ;; so we really need to speak it
      (when emacspeak-speak-messages-pause (dtk-pause))
      (tts-with-punctuations 'all
                             (dtk-speak ad-return-value)))
    ad-return-value))

(defvar emacspeak-ange-ftp-last-percent nil
  "Cache the last percentage that emacspeak spoke.")

(defadvice ange-ftp-process-handle-hash (around emacspeak pre act )
  "Jibber intelligently."
  (declare (special emacspeak-ange-ftp-last-percent
                    ange-ftp-last-percent ))
  (let ((emacspeak-speak-messages nil ))
    ad-do-it
    (when (or (null emacspeak-ange-ftp-last-percent)
              (>= (abs (- ange-ftp-last-percent emacspeak-ange-ftp-last-percent ))
                  5))
      (setq emacspeak-ange-ftp-last-percent ange-ftp-last-percent )
      (emacspeak-auditory-icon 'progress)
      (dtk-speak
       (format " %s percent" ange-ftp-last-percent )))))

;;{{{ advising signal

(defadvice signal (before emacspeak pre act compile)
  "Speak the error message as well."
  (declare (special emacspeak-speak-cue-errors))
  (when emacspeak-speak-cue-errors
    (let ((dtk-stop-immediately t)
          (message (and (not (eq 'error (ad-get-arg 0)))
                        (get (ad-get-arg 0) 'error-message))))
      (when  message
        (dtk-speak message)))))

;;}}}
;;;###autoload
(defcustom emacspeak-speak-cue-errors t
  "Specifies if error messages are cued."
  :type 'boolean
  :group 'emacspeak-spek)
(defadvice error (before emacspeak pre act)
  "Speak the error message.
Also produces an auditory icon if possible."
  (when emacspeak-speak-cue-errors
    (let ((dtk-stop-immediately nil ))
      (emacspeak-auditory-icon 'warn-user)
      (tts-with-punctuations 'all
                             (message
                              (apply #'format
                                     (ad-get-args  0)))))))

(defadvice eval-minibuffer (before emacspeak pre act com)
  "Speak the prompt."
  (tts-with-punctuations 'all
                         (dtk-speak (ad-get-arg 0))))

(defadvice read-passwd (before emacspeak pre act comp)
  "Speak the prompt."
  (emacspeak-auditory-icon 'open-object)
  (dtk-speak (ad-get-arg 0)))

(defadvice read-from-minibuffer (around emacspeak pre act)
  "Prompt using speech as well."
  (let((prompt (ad-get-arg 0))
       (initial (ad-get-arg 1 ))
       (default (ad-get-arg 5)))
    (tts-with-punctuations 'all
                           (dtk-speak
                            (format "%s  %s%s"
                                    prompt
                                    (if  initial
                                        initial
                                      "")
                                    (if default
                                        (format "Default: %s" default)
                                      ""))))
    ad-do-it
    (tts-with-punctuations 'all
                           (dtk-speak ad-return-value ))
    ad-return-value))

(defadvice read-no-blanks-input (around emacspeak pre act)
  "Prompt using speech as well."
  (let ((prompt (ad-get-arg 0))
        (default  (ad-get-arg 1 )))
    (tts-with-punctuations 'all
                           (dtk-speak
                            (format "%s  %s"
                                    prompt
                                    (if  default
                                        (format "Default: %s" default)
                                      ""))))
    ad-do-it
    (tts-with-punctuations 'all
                           (dtk-speak ad-return-value ))
    ad-return-value))

(defadvice read-minibuffer (around emacspeak pre act)
  "Prompt using speech as well."
  (let ((prompt (ad-get-arg 0))
        (default  (ad-get-arg 1 )))
    (tts-with-punctuations 'all
                           (dtk-speak
                            (format "%s %s"
                                    prompt
                                    (if default
                                        (format "Default %s" default)
                                      " "))))
    ad-do-it
    (tts-with-punctuations 'all
                           (dtk-speak ad-return-value ))
    ad-return-value ))

(defadvice y-or-n-p (around emacspeak pre act )
  "Use speech when prompting.
Produce an auditory icon if possible."
  (emacspeak-auditory-icon 'ask-short-question )
  (when emacspeak-speak-messages-pause
    (dtk-pause))
  (tts-with-punctuations 'all
                         (dtk-speak (format "%s  y or n" (ad-get-arg  0 ))))
  ad-do-it
  (cond
   (ad-return-value
    (emacspeak-auditory-icon 'y-answer )
    (dtk-say "y"))
   (t (emacspeak-auditory-icon  'n-answer )
      (dtk-say "n" )))
  ad-return-value )

(defadvice yes-or-no-p (around emacspeak pre act )
  "Use speech when prompting.
Produce an auditory icon as well."
  (emacspeak-auditory-icon 'ask-question)
  (when emacspeak-speak-messages-pause
    (dtk-pause))
  (tts-with-punctuations 'all
                         (dtk-speak (format "%s  yes or no" (ad-get-arg  0 ))))
  ad-do-it
  (cond
   (ad-return-value
    (emacspeak-auditory-icon 'yes-answer )
    (dtk-say "yes"))
   (t (emacspeak-auditory-icon  'no-answer )
      (dtk-say "no" )))
  ad-return-value )

;;}}}
;;{{{  advice various input functions to speak:
(defadvice read-key-sequence(around emacspeak pre act )
  "Prompt using speech as well. "
  (let ((prompt (ad-get-arg 0)))
    (when prompt
      (tts-with-punctuations 'all
                             (dtk-speak prompt)))
    ad-do-it
    (tts-with-punctuations 'all
                           (dtk-speak (format "%s" ad-return-value)))
    ad-return-value))

(defadvice completing-read (around emacspeak pre act )
  "Prompt using speech."
  (let ((dtk-stop-immediately t )
        (prompt (ad-get-arg 0))
        (initial (ad-get-arg 4 ))
        (default (ad-get-arg 6)))
    (dtk-speak
     (format "%s %s"
             (or prompt " ")
             (or initial default " ")))
    ad-do-it
    (tts-with-punctuations 'all
                           (dtk-speak (format "%s" ad-return-value )))
    ad-return-value ))

(defadvice read-buffer(around emacspeak pre act )
  "Prompt using speech as well. "
  (let ((prompt (ad-get-arg 0))
        (default (ad-get-arg 1 )))
    (tts-with-punctuations 'all
                           (dtk-speak
                            (format "%s %s"
                                    prompt
                                    (or default " "))))
    ad-do-it
    (tts-with-punctuations 'all
                           (dtk-speak ad-return-value))
    ad-return-value))

(defadvice read-char (before emacspeak pre act comp)
  "Speak the prompt"
  (tts-with-punctuations 'all
                         (let ((prompt  (ad-get-arg 0)))
                           (and prompt (dtk-speak prompt)))))

(defadvice read-char-exclusive (before emacspeak pre act comp)
  "Speak the prompt"
  (let ((prompt  (ad-get-arg 0)))
    (if  prompt
        (tts-with-punctuations 'all
                               (dtk-speak prompt)))))

(defadvice read-command(around emacspeak pre act )
  "Prompt using speech as well. "
  (let ((prompt (ad-get-arg 0)))
    (when prompt
      (tts-with-punctuations 'all
                             (dtk-speak prompt)))
    ad-do-it
    (tts-with-punctuations 'all
                           (dtk-speak (format "%s" ad-return-value)))
    ad-return-value))

(defadvice read-string(around emacspeak pre act )
  "Prompt using speech as well. "
  (let ((prompt (ad-get-arg 0 ))
        (default (ad-get-arg 1 )))
    (tts-with-punctuations 'all
                           (dtk-speak
                            (format "%s %s"
                                    prompt
                                    (or default " "))))
    ad-do-it
    (tts-with-punctuations 'all
                           (dtk-speak (format "%s" ad-return-value)))
    ad-return-value))

(defadvice read-variable(around emacspeak pre act )
  "Prompt using speech as well. "
  (let ((prompt (ad-get-arg 0)))
    (when prompt
      (tts-with-punctuations 'all
                             (dtk-speak prompt)))
    ad-do-it
    (tts-with-punctuations 'all
                           (dtk-speak (format "%s" ad-return-value)))
    ad-return-value))

(defadvice read-file-name (around emacspeak pre act )
  "Prompt using speech as well."
  (let ((directory (or
                    (ad-get-arg 1)
                    default-directory))
        (default (ad-get-arg 2 )))
    (tts-with-punctuations 'all
                           (dtk-speak
                            (format "%s %s %s"
                                    (ad-get-arg 0 )
                                    (or directory "")
                                    (if default
                                        (format "Default %s" default )
                                      ""))))
    ad-do-it
    (tts-with-punctuations 'all
                           (dtk-speak ad-return-value))
    ad-return-value))

;;}}}
;;{{{  advice completion functions to speak:

(defadvice dabbrev-expand (after emacspeak pre act)
  "Say what you completed."
  (when (interactive-p)
    (tts-with-punctuations 'all
                           (dtk-speak
                            dabbrev--last-expansion))))

(loop for f in
      '(minibuffer-complete-word  minibuffer-complete)
      do
      (eval
       `(defadvice ,f (around emacspeak pre act)
  "Say what you completed."
  (cond
   ((interactive-p)
  (let ((prior (point ))
        (emacspeak-speak-messages nil))
    (emacspeak-kill-buffer-carefully "*Completions*")
    ad-do-it
    (if (> (point) prior)
          (tts-with-punctuations
           'all
               (dtk-speak
                (buffer-substring (point) prior)))
        (emacspeak-speak-completions-if-available))))
   (t ad-do-it))
  ad-return-value)))


(loop for f in
      '(lisp-complete-symbol complete-symbol)
      do
      (eval
       `(defadvice ,f (around emacspeak pre act)
          "Say what you completed."
          (let ((prior (point ))
                (emacspeak-speak-messages nil))
            ad-do-it
            (if (> (point) prior)
                (tts-with-punctuations
                 'all
                 (dtk-speak
                  (buffer-substring prior (point))))
              (emacspeak-speak-completions-if-available))
            ad-return-value))))

(defadvice switch-to-completions(after emacspeak pre act comp)
  "Provide spoken feedback."
  (dtk-stop)
  (emacspeak-auditory-icon 'select-object)
  (dtk-speak (emacspeak-get-current-completion)))

(defadvice complete (around emacspeak pre act)
  "Say what you completed."
  (let ((emacspeak-speak-messages nil)
        (emacspeak-last-message nil))
    ad-do-it
    (when  (interactive-p)
      (dtk-speak
       (format "%s %s"
               (save-excursion (backward-char 1)
                               (sexp-at-point ))
               (or emacspeak-last-message "")))
      ad-return-value)))

(defadvice  next-completion (after emacspeak  pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (tts-with-punctuations 'all
                           (dtk-speak (emacspeak-get-current-completion)))))

(defadvice  previous-completion (after emacspeak  pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (tts-with-punctuations 'all
                           (dtk-speak
                            (emacspeak-get-current-completion )))))

(defadvice choose-completion (before emacspeak pre act )
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)))

(defadvice minibuffer-message (around emacspeak pre act comp)
  "Speak the message if appropriate."
  (declare (special emacspeak-last-message
                    emacspeak-speak-messages emacspeak-lazy-message-time))
  (let ((dtk-stop-immediately t ))
    ad-do-it
    (setq emacspeak-last-message ad-return-value )
    (when (and   emacspeak-speak-messages ; speaking messages
                 ad-return-value          ;we really do have a message
                 (/= emacspeak-lazy-message-time ;; previous message not recent
                     (setq emacspeak-lazy-message-time
                           (nth 1    (current-time)))))
      ;; so we really need to speak it
      (tts-with-punctuations 'all
                             (dtk-speak ad-return-value)))))

;;}}}
;;{{{ tmm support

(defadvice tmm-goto-completions (after emacspeak pre act comp)
  "announce completions "
  (when (interactive-p)
    (emacspeak-auditory-icon 'help)
    (dtk-speak (emacspeak-get-current-completion))))

(defadvice minibuffer-complete-and-exit (before emacspeak pre act comp)
  "Provide an auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'button)))

(defadvice tmm-menubar (before emacspeak pre act comp)
  "Provide an auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object )))

(defadvice tmm-add-prompt (around emacspeak pre act comp)
  "Speaks the list of completions we have available."
  ad-do-it
  (let ((inhibit-read-only t)
        (cap "cap "))
    (put-text-property 0 (length cap)
                       'personality voice-animate  cap)
    (put-text-property 0  (length tmm-mid-prompt)
                       'personality 'inaudible
                       tmm-mid-prompt)
    (tts-with-punctuations 'all
                           (dtk-speak
                            (mapconcat
                             (function
                              (lambda (choice)
                                (declare (special cap))
                                (let ((string (car choice )))
                                  (put-text-property
                                   0 1
                                   'personality voice-animate  string)
                                  (when (string-match "^[A-Z]" string)
                                    (setq string
                                          (concat cap string)))
                                  string)))
                             minibuffer-completion-table " ")
                            )))
  ad-return-value)

(defadvice tmm-shortcut (after emacspeak pre act comp)
  "Provide contextual feedback when exitting minibuffer."
  (emacspeak-auditory-icon 'button))

;;}}}
;;{{{  Advice comint:

(defadvice comint-magic-space (around emacspeak pre act)
  "Speak word or completion."
  (cond
   ((interactive-p)
    (let ((orig (point))
          (emacspeak-speak-messages nil)
          (count (ad-get-arg 0)))
      (setq count (or count 1))
      ad-do-it
      (cond
       ((= (point) (+ count orig))
        (save-excursion
          (forward-word -1)
          (emacspeak-speak-word)))
       (t (emacspeak-auditory-icon 'select-object)
          (emacspeak-speak-region
           (comint-line-beginning-position) (point))))))
   (t ad-do-it))
  ad-return-value)

(defadvice comint-insert-previous-argument (around emacspeak pre
                                                   act comp)
  "Provide auditory feedback."
  (cond
   ((interactive-p)
    (let ((orig (point)))
      ad-do-it
      (emacspeak-speak-region orig (point))
      (emacspeak-auditory-icon 'select-object)))
   (t ad-do-it))
  ad-return-value)

(require 'shell)

;;; Customize comint:

(add-hook 'comint-output-filter-functions
          'comint-truncate-buffer)
(when (locate-library "ansi-color")
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on))
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt )
(voice-setup-add-map
 '(
   (comint-highlight-prompt voice-monotone-medium)
   (comint-highlight-input voice-bolden-medium)))

(add-hook 'shell-mode-hook 'emacspeak-pronounce-refresh-pronunciations)

(loop for f in
      '(shell-command shell-dirstack-message)
      do
      (eval
`(defadvice ,f (around emacspeak pre act comp)
  "Silence messages"
  (let ((emacspeak-speak-messages nil))
    ad-do-it))))

(add-hook 'comint-mode-hook 'emacspeak-comint-speech-setup)

(defadvice comint-delchar-or-maybe-eof (around emacspeak pre act)
  "Speak character you're deleting."
  (cond
   ((interactive-p )
    (cond
     ((= (point) (point-max))
      (message "Sending EOF to comint process"))
     (t (dtk-tone 500 30 'force)
        (and emacspeak-delete-char-speak-deleted-char
             (emacspeak-speak-char t))))
    ad-do-it)
   (t ad-do-it))
  ad-return-value)

(defadvice comint-send-input (before emacspeak pre act comp)
  "Aurally highlight input."
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (emacspeak-personality-append start end 'emacspeak-comint-input-personality)))

(defadvice comint-send-eof (before emacspeak pre act comp)
  "Announce what we are doing."
  (when (interactive-p)
    (message "Sending EOF to subprocess")))

(defadvice comint-accumulate  (before emacspeak pre act comp)
  "Speak the line we are accumulating."
  (when (interactive-p)
    (save-excursion
      (comint-bol-or-process-mark)
      (emacspeak-auditory-icon 'select-object)
      (emacspeak-speak-line 1))))

(defadvice comint-next-matching-input-from-input  (after
                                                   emacspeak
                                                   pre act com)
  "Speak the line showing where point is."
  (when (interactive-p)
    (let ((emacspeak-show-point t)
          (voice-lock-mode t))
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'select-object))))

(defadvice comint-previous-matching-input-from-input  (after
                                                       emacspeak
                                                       pre act com)
  "Speak the line showing where point is."
  (when (interactive-p)
    (let ((emacspeak-show-point t)
          (voice-lock-mode t))
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'select-object))))

(defadvice shell-forward-command (after emacspeak pre act
                                        comp)
  "Speak the line showing where point is."
  (when (interactive-p)
    (let ((emacspeak-show-point t)
          (voice-lock-mode t))
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'select-object))))

(defadvice shell-backward-command (after emacspeak pre act
                                         comp)
  "Speak the line showing where point is."
  (when (interactive-p)
    (let ((emacspeak-show-point t)
          (voice-lock-mode t))
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'select-object))))

(defadvice comint-show-output (after emacspeak pre act
                                     comp)
  "Speak the line showing where point is."
  (when (interactive-p)
    (let ((emacspeak-show-point t)
          (voice-lock-mode t))
      (emacspeak-auditory-icon 'large-movement)
      (emacspeak-speak-region (point) (mark)))))

(defadvice comint-show-maximum-output (after emacspeak pre act
                                             comp)
  "Speak the line showing where point is."
  (when (interactive-p)
    (let ((emacspeak-show-point t)
          (voice-lock-mode t))
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'select-object))))

(defadvice comint-bol-or-process-mark (after emacspeak pre act
                                             comp)
  "Speak the line showing where point is."
  (when (interactive-p)
    (let ((emacspeak-show-point t)
          (voice-lock-mode t))
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'select-object))))

(defadvice comint-copy-old-input (after emacspeak pre act
                                        comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-speak-line)))

(defadvice comint-output-filter (around emacspeak pre act)
  "Make comint speak its output."
  (save-excursion
    (set-buffer (process-buffer (ad-get-arg 0)))
    (let ((prior (point ))
          (monitor emacspeak-comint-output-monitor)
          (dtk-stop-immediately nil))
      ad-do-it
      (when (and (boundp 'comint-last-prompt-overlay)
                 comint-last-prompt-overlay)
        (add-text-properties
         (overlay-start comint-last-prompt-overlay)
         (overlay-end comint-last-prompt-overlay)
         (list
          'personality
          'emacspeak-comint-prompt-personality
          'rear-sticky nil)))
      (when (and (or emacspeak-comint-autospeak emacspeak-speak-comint-output)
                 (or
                  monitor
                  (eq (selected-window)
                      (get-buffer-window (process-buffer (ad-get-arg 0))))))
        (setq emacspeak-speak-comint-output nil)
        (condition-case nil
            (emacspeak-speak-region prior (point ))
          (error (emacspeak-auditory-icon 'scroll)
                 (dtk-stop ))))
      ad-return-value)))

(defadvice comint-dynamic-list-completions(around emacspeak pre act comp)
  "Replacing mouse oriented completer with keyboard friendly equivalent"
  (let ((completions (sort (ad-get-arg 0) 'string-lessp)))
  (with-output-to-temp-buffer "*Completions*"
    (display-completion-list completions))
  (save-excursion
    (set-buffer (get-buffer "*Completions*"))
    (set (make-local-variable 'comint-displayed-dynamic-completions)
         completions))
  (next-completion 1)
  (dtk-speak
   (buffer-substring (point) (point-max)))))

(defadvice  comint-dynamic-complete (around emacspeak pre act)
  "Say what you completed."
  (cond
   ((interactive-p)
    (let ((prior (point ))
          (emacspeak-speak-messages nil))
      (emacspeak-kill-buffer-carefully "*Completions*")
      ad-do-it
      (if (> (point) prior)
          (tts-with-punctuations 'all
                                 (dtk-speak (buffer-substring prior (point ))))
        (emacspeak-speak-completions-if-available))))
   (t ad-do-it))
  ad-return-value)

(defadvice comint-next-input (after emacspeak pre act)
  "Speak the line."
  (when (interactive-p)
    (tts-with-punctuations 'all
                           (emacspeak-speak-line ))
    (emacspeak-auditory-icon 'select-object)))

(defadvice comint-next-matching-input (after emacspeak pre act)
  "Speak the line."
  (when (interactive-p)
    (tts-with-punctuations 'all
                           (emacspeak-speak-line ))
    (emacspeak-auditory-icon 'select-object)))

(defadvice comint-previous-input (after emacspeak pre act)
  "Speak the line."
  (when (interactive-p)
    (tts-with-punctuations 'all
                           (emacspeak-speak-line ))
    (emacspeak-auditory-icon 'select-object)))

(defadvice comint-previous-matching-input (after emacspeak pre act)
  "Speak the line."
  (when (interactive-p)
    (comint-skip-prompt)
    (tts-with-punctuations 'all
                           (emacspeak-speak-line))
    (emacspeak-auditory-icon 'select-object)))

(defadvice comint-send-input (after emacspeak pre act)
  "Flush any ongoing speech."
  (when (interactive-p)
    (dtk-stop)))

(defadvice comint-previous-prompt (after emacspeak pre act )
  "Provide spoken feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (if (eolp)
        (emacspeak-speak-line)
      (emacspeak-speak-line 1))))

(defadvice comint-next-prompt (after emacspeak pre act )
  "Provide spoken feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (if (eolp)
        (emacspeak-speak-line)
      (emacspeak-speak-line 1))))

(defadvice comint-dynamic-list-input-ring (around emacspeak pre act comp)
  "List in help buffer the buffer's input history."
  (cond
   ((interactive-p)
    (if (or (not (ring-p comint-input-ring))
            (ring-empty-p comint-input-ring))
        (message "No history")
      (let ((history nil)
            (history-buffer " *Input History*")
            (index (1- (ring-length comint-input-ring))))
        ;; We have to build up a list ourselves from the ring vector.
        (while (>= index 0)
          (setq history (cons (ring-ref comint-input-ring index) history)
                index (1- index)))
        ;; Change "completion" to "history reference"
        ;; to make the display accurate.
        (with-output-to-temp-buffer history-buffer
          (display-completion-list history)
          (switch-to-buffer history-buffer)
          (forward-line 3)
          (while (search-backward "completion" nil 'move)
            (replace-match "history reference")))
        (emacspeak-auditory-icon 'help)
        (next-completion 1)
        (dtk-speak (emacspeak-get-current-completion)))))
   (t ad-do-it))
  ad-return-value)

(defadvice comint-kill-output (after emacspeak pre act )
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (message "Nuked output of last command ")))

(defadvice comint-quit-subjob (after emacspeak pre act )
  "Provide auditory feedback."
  (when (interactive-p)
    (message "Sent quit signal to subjob ")))

(defadvice comint-stop-subjob (after emacspeak pre act )
  "Provide auditory feedback."
  (when (interactive-p)
    (message "Stopped the subjob")))

(defadvice comint-interrupt-subjob (after emacspeak pre act )
  "Provide auditory feedback."
  (when (interactive-p)
    (message "Interrupted  the subjob")))

(defadvice comint-kill-input (before emacspeak pre act )
  "Provide spoken feedback."
  (when (interactive-p)
    (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
      (when  (> (point) (marker-position pmark))
        (emacspeak-auditory-icon 'delete-object )
        (emacspeak-speak-region  pmark (point))))))

(defadvice comint-dynamic-list-filename-completions (after emacspeak pre act )
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-speak-completions-if-available)))

;;}}}
;;{{{  Advice centering and filling commands:

(defadvice center-line (after emacspeak pre act)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (message"Centered current line")))

(defadvice center-region (after emacspeak pre act)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (message"Centered current region containing %s lines"
            (count-lines
             (region-beginning)
             (region-end)))))

(defadvice center-paragraph (after emacspeak pre act)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (message"Centered current paragraph")))

(defadvice fill-paragraph (after emacspeak pre act)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'fill-object )
    (message "Filled current paragraph")))

(defadvice lisp-fill-paragraph (after emacspeak pre act)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'fill-object )
    (message "Filled current paragraph")))

(defadvice reindent-then-newline-and-indent (after emacspeak pre act comp)
  "Provide auditory feedback to indicate indentation."
  (when (interactive-p)
    (emacspeak-speak-line)))
(defadvice indent-region (after emacspeak pre act comp)
  "Provide auditory feedback to indicate indentation."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (message "Indented region")))

(defadvice indent-relative (after emacspeak pre act comp)
  "Provide auditory feedback to indicate indentation."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-current-column)))

(defadvice indent-pp-sexp  (after emacspeak pre act)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'fill-object )
    (message "Indented current s expression ")))

(defadvice indent-sexp  (after emacspeak pre act)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'fill-object )
    (message "Indented current s expression ")))

(defadvice fill-region (after emacspeak pre act)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'fill-object )
    (message "Filled current region containing %s lines"
             (count-lines (region-beginning)
                          (region-end)))))

;;}}}
;;{{{  vc:

;;; helper function: find out vc version:

;;; guess the vc version number from the variable used in minor mode alist
(defsubst emacspeak-vc-get-version-id ()
  "Return VC version id."
  (declare (special vc-mode ))
  (let ((id vc-mode ))
    (cond
     ((and vc-mode
           (stringp vc-mode))
      (substring id 5  nil ))
     (t " "))))

(defadvice vc-toggle-read-only (around emacspeak pre act)
  "Provide auditory feedback."
  (cond
   ((interactive-p)
    (let ((message (format  "Checking %s version %s "
                            (if buffer-read-only  "out previous " " in new  ")
                            (emacspeak-vc-get-version-id))))
      (if buffer-read-only
          (emacspeak-auditory-icon 'open-object )
        (emacspeak-auditory-icon 'close-object))
      ad-do-it
      (message message )))
   (t ad-do-it ))
  ad-return-value )

(defadvice vc-next-action (around  emacspeak pre act)
  "Provide auditory feedback."
  (cond
   ((interactive-p)
    (let ((message (format  "Checking %s version %s "
                            (if buffer-read-only  "out previous " " in new  ")
                            (emacspeak-vc-get-version-id))))
      (if buffer-read-only
          (emacspeak-auditory-icon 'close-object)
        (emacspeak-auditory-icon 'open-object ))
      ad-do-it
      (message message)))
   (t ad-do-it ))
  ad-return-value )

(defadvice vc-revert-buffer (after emacspeak pre act)
  "Provide auditory feedback."
  (when (interactive-p  )
    (emacspeak-auditory-icon 'open-object)))

(defadvice vc-finish-logentry (after emacspeak pre act)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon  'close-object)
    (message "Checked   in  version %s "
             (emacspeak-vc-get-version-id))))

;;}}}
;;{{{  misc functions that have to be hand fixed:

(defadvice zap-to-char (after emacspeak pre act comp)
  "Speak line that is left."
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-speak-line 1)))

(defadvice undefined (after emacspeak pre act comp)
  "Say that this is not defined."
  (when (interactive-p)
    (emacspeak-auditory-icon 'warn-user)
    (message "No command on this key")))

(defadvice describe-mode (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (message "Displayed mode help in other window")
    (emacspeak-auditory-icon 'help)))
(loop for f in
      '(describe-bindings
        describe-prefix-bindings)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (interactive-p)
            (message "Displayed key bindings  in other window")
            (emacspeak-auditory-icon 'help)))))

(defadvice indent-for-tab-command (after emacspeak pre act comp)
  "Produce auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-current-column)))

(defadvice line-number-mode (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'button)
    (emacspeak-speak-mode-line)))

(defadvice column-number-mode (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'button)
    (emacspeak-speak-mode-line)))

(defadvice not-modified (after emacspeak pre act )
  "Provide an auditory icon."
  (when (interactive-p)
    (if (ad-get-arg 0)
        (emacspeak-auditory-icon 'modified-object )
      (emacspeak-auditory-icon 'unmodified-object))))

(defadvice comment-region (after emacspeak pre act )
  "Provide spoken feedback."
  (when (interactive-p)
    (let ((prefix-arg (ad-get-arg 2)))
      (message "%s region containing %s lines"
               (if (and prefix-arg
                        (< prefix-arg 0))
                   "Uncommented"
                 "Commented")
               (count-lines (point) (mark 'force))))))

(defadvice bury-buffer (after emacspeak pre act)
  "Announce the buffer that becomes current."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line )))

(defadvice save-buffer (around emacspeak pre act)
  "Produce an auditory icon if possible."
  (declare (special emacspeak-last-message))
  (cond
   ((interactive-p)
    (setq emacspeak-last-message nil)
    ad-do-it
    (emacspeak-auditory-icon 'save-object)
    (or emacspeak-last-message
        (message "Wrote %s"
                 (buffer-file-name))))
   (t ad-do-it))
  ad-return-value)

(defadvice save-some-buffers (around emacspeak pre act)
  "Produce an auditory icon if possible."
  (declare (special emacspeak-last-message))
  (cond
   ((interactive-p)
    (setq emacspeak-last-message nil)
    ad-do-it
    (emacspeak-auditory-icon 'save-object))
   (t ad-do-it))
  ad-return-value)

(defadvice kill-region (around emacspeak pre act)
  "Indicate region has been killed.
Use an auditory icon if possible."
  (cond
   ((interactive-p)
    (let ((count (count-lines (region-beginning)
                              (region-end))))
      ad-do-it
      (emacspeak-auditory-icon 'delete-object )
      (message "Killed region containing %s lines" count)))
   (t ad-do-it))
  ad-return-value)

(defadvice completion-kill-region (around emacspeak pre act)
  "Indicate region has been killed.
Use an auditory icon if possible."
  (cond
   ((interactive-p)
    (let ((count (count-lines (region-beginning)
                              (region-end))))
      ad-do-it
      (emacspeak-auditory-icon 'delete-object )
      (message "Killed region containing %s lines" count)))
   (t ad-do-it))
  ad-return-value)

(defadvice kill-ring-save (after emacspeak pre act)
  "Indicate that region has been copied to the kill ring.
Produce an auditory icon if possible."
  (when (interactive-p )
    (emacspeak-auditory-icon 'mark-object )
    (message "region containing %s lines  copied to kill ring "
             (count-lines (region-beginning)
                          (region-end)))))

(defadvice find-file (after emacspeak pre act )
  "Play an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice kill-buffer (around emacspeak pre act)
  "Speech-enabled by emacspeak."
  (cond
   ((interactive-p)
    (dtk-speak
     (format "Kill Buffer: %s"
             (buffer-name)))
    ad-do-it
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line))
   (t ad-do-it))
  ad-return-value)

(defadvice quit-window (after emacspeak pre act)
  "Produce an auditory icon to indicate closing of an object.
Then indicate current buffer by speaking  the modeline."
  (when (interactive-p )
    (emacspeak-auditory-icon 'close-object)))

(defadvice other-window (after emacspeak pre act )
  "Speak modeline.
Indicate change of selection with an auditory icon
  if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))
(defadvice delete-frame (after emacspeak pre act comp)
  "Provide auditory feedback about frame and buffer that becomes current."
  (when (interactive-p)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'close-object)))

(defadvice make-frame-command (after emacspeak pre act comp)
  "Indicate that a new frame is being created."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice delete-other-frames (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Deleted all other frames.")))

(defadvice other-frame (after emacspeak pre act )
  "Speak modeline.
Indicate change of selection with an auditory icon
  if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

(defadvice move-to-window-line (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice rename-buffer  (around emacspeak pre act)
  "Provide spoken feedback."
  (cond
   ((interactive-p)
    (message "Rename buffer to new name ")
    ad-do-it
    (emacspeak-speak-mode-line))
   (t ad-do-it ))
  ad-return-value)

(defadvice switch-to-buffer  (after emacspeak pre act)
  "Speak the modeline.
Indicate change of selection with
  an auditory icon if possible."
  (when (interactive-p )
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

(defadvice switch-to-buffer-other-window  (after emacspeak pre act)
  "Speak the modeline.
Indicate change of selection with
  an auditory icon if possible."
  (when (interactive-p )
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

(defadvice switch-to-buffer-other-frame  (after emacspeak pre act)
  "Speak the modeline.
Indicate change of selection with
  an auditory icon if possible."
  (when (interactive-p )
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

(defadvice local-set-key (before  emacspeak pre act)
  "Prompt using speech."
  (interactive
   (list
    (read-key-sequence "Locally bind key:")
    (read-command "To command:" ))))

(defadvice global-set-key (before  emacspeak pre act)
  "Provide spoken prompts."
  (interactive
   (list
    (read-key-sequence "Globally  bind key:")
    (read-command "To command:" ))))

(defadvice modify-syntax-entry (before  emacspeak pre act)
  "Provide spoken prompts."
  (interactive
   (list
    (read-char "Modify syntax for: ")
    (read-string "Syntax Entry: ")
    current-prefix-arg)))

(defadvice help-next-ref (after emacspeak pre act comp)
  "Speak the ref we moved to."
  (when (interactive-p)
    (emacspeak-speak-text-range 'help-xref)
    (emacspeak-auditory-icon 'large-movement)))

(defadvice help-previous-ref (after emacspeak pre act comp)
  "Speak the ref we moved to."
  (when (interactive-p)
    (emacspeak-speak-text-range 'help-xref)
    (emacspeak-auditory-icon 'large-movement)))

(defadvice help-follow (after emacspeak pre act comp)
  "Speak the ref we moved to."
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'button)))

(defadvice describe-function (after emacspeak pre act)
  "Speak the help."
  (when (interactive-p) (emacspeak-speak-help )))

(defadvice describe-variable (after emacspeak pre act)
  "Speak the help."
  (when (interactive-p) (emacspeak-speak-help )))

(defadvice describe-key (after emacspeak pre act)
  "Speak the help."
  (when (interactive-p)
    (emacspeak-speak-help )))

(defadvice help-with-tutorial (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (let ((extent nil))
      (save-excursion
        (goto-char (point-min))
        (forward-line (window-height ))
        (emacspeak-speak-region (point-min)
                                (point))))))

(defadvice key-description (around emacspeak pre act )
  "Change returned key description to a form that is suitable to be spoken."
  (declaim (special case-fold-search))
  (let ((emacspeak-scratch (get-buffer-create  " *dtk-scratch-buffer* "))
        (shift-regexp "S-\\(.\\)")
        (ctrl-regexp "C-\\(.\\)")
        (meta-regexp "M-\\(.\\)")
        (caps-regexp "\\b[A-Z]\\b")
        (hyper-regexp "C-x @ h")
        (alt-regexp "C-x @ a")
        (super-regexp "C-x @ s"))
    (condition-case nil
        (progn
          ad-do-it
          (save-excursion
            (set-buffer emacspeak-scratch )
            (setq buffer-undo-list t)
            (setq case-fold-search nil)
            (erase-buffer)
            (insert  (format " %s " ad-return-value ))
            (goto-char (point-min))
            (save-match-data
              (while (search-forward "SPC"  nil t )
                (replace-match "space"))
              (goto-char (point-min))
              (while (search-forward "ESC"  nil t )
                (replace-match "escape"))
              (goto-char (point-min))
              (while (search-forward "RET"  nil t )
                (replace-match "return"))
              (goto-char (point-min))
              (while (re-search-forward hyper-regexp  nil t )
                (replace-match "hyper "))
              (goto-char (point-min))
              (while (re-search-forward alt-regexp  nil t )
                (replace-match "alt "))
              (goto-char (point-min))
              (while (re-search-forward super-regexp  nil t )
                (replace-match "super "))
              (goto-char (point-min))
              (while (re-search-forward shift-regexp  nil t )
                (replace-match "shift \\1"))
              (goto-char (point-min))
              (while (re-search-forward ctrl-regexp  nil t )
                (replace-match "control \\1"))
              (goto-char (point-min))
              (while (re-search-forward meta-regexp  nil t )
                (replace-match "meta \\1"))
              (goto-char (point-min))
              (while (re-search-forward alt-regexp  nil t )
                (replace-match "alt \\1"))
              (goto-char (point-min))
              (while (re-search-forward caps-regexp nil t)
                (replace-match " cap \\& " t)))
            (setq ad-return-value (buffer-string ))))
      (error ""))
    ad-return-value))

(defadvice exchange-point-and-mark (after emacspeak pre act)
  "Speak the line.
Indicate large movement with an auditory icon if possible.
Auditory highlight indicates position of point."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement )
    (ems-set-personality-temporarily  (point) (1+ (point))
                                      voice-animate
                                      (emacspeak-speak-line))))

(defadvice newline (before emacspeak pre act)
  "Speak the previous line if line echo is on.
See command \\[emacspeak-toggle-line-echo].  Otherwise cue the user to
the newly created blank line."

  (declare (special emacspeak-line-echo ))
  (when (interactive-p)
    (cond
     (emacspeak-line-echo
      (emacspeak-speak-line ))
     (t(if dtk-stop-immediately (dtk-stop))
       (dtk-tone 225 120 'force   )))))

(defadvice newline-and-indent (around emacspeak pre act)
  "Speak the previous line if line echo is on.
See command \\[emacspeak-toggle-line-echo].
Otherwise cue user to the line just created."
  (declare (special emacspeak-line-echo ))
  (cond
   ((interactive-p)
    (cond
     (emacspeak-line-echo
      (emacspeak-speak-line )
      ad-do-it)
     (t ad-do-it
        (dtk-speak-using-voice voice-annotate
                               (format
                                "indent %s"
                                (current-column)))
        (dtk-force))))
   (t ad-do-it))
  ad-return-value)

(defadvice keyboard-quit (before emacspeak pre act)
  "Stop speech first."
  (dtk-pause)
  (emacspeak-auditory-icon 'warn-user)
  (dtk-speak "quit"))

(defadvice keyboard-escape-quit (before emacspeak pre act)
  "Stop speech first."
  (dtk-pause)
  (emacspeak-auditory-icon 'item)
  (emacspeak-speak-mode-line))

(defadvice delete-indentation (after emacspeak pre act)
  "Speak the line."
  (when (interactive-p) (emacspeak-speak-line)))

(defadvice eval-last-sexp (after emacspeak pre act)
  "Also speaks the result of evaluation."
  (let ((dtk-chunk-separator-syntax " .<>()$\"\'"))
    (tts-with-punctuations 'all
                           (dtk-speak
                            (format "%s" ad-return-value )))))

(defadvice eval-expression (after emacspeak pre act)
  "Also speaks the result of evaluation."
  (let ((dtk-chunk-separator-syntax " .<>()$\"\'"))
    (tts-with-punctuations 'all
                           (dtk-speak
                            (format "%s" ad-return-value )))))

(defadvice shell (after emacspeak pre act )
  "Announce switching to shell mode.
Provide an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object )
    (emacspeak-speak-mode-line)))

;;{{{  composing mail

(defadvice mail (after emacspeak pre act)
  "Give some auditory feedback."
  (emacspeak-auditory-icon 'open-object)
  (let ((emacspeak-speak-messages nil))
    (save-excursion
      (goto-char (point-min))
      (emacspeak-speak-line))))

(defadvice mail-other-window (after emacspeak pre act)
  "Give some auditory feedback."
  (emacspeak-auditory-icon 'open-object)
  (let ((emacspeak-speak-messages nil))
    (save-excursion
      (goto-char (point-min))
      (emacspeak-speak-line))))

(defadvice mail-other-frame (after emacspeak pre act)
  "Give some auditory feedback."
  (emacspeak-auditory-icon 'open-object)
  (let ((emacspeak-speak-messages nil))
    (save-excursion
      (goto-char (point-min))
      (emacspeak-speak-line))))

(defadvice mail-text (after emacspeak pre act)
  "Indicate movement."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice mail-subject (after emacspeak pre act)
  "Speak the subject line."
  (when (interactive-p)
    (emacspeak-speak-line )))

(defadvice mail-cc   (after emacspeak pre act)
  "Speak the cc  line."
  (when (interactive-p)
    (emacspeak-speak-line )))

(defadvice mail-bcc (after emacspeak pre act)
  "Speak the bcc line."
  (when (interactive-p)
    (emacspeak-speak-line )))

(defadvice mail-to (after emacspeak pre act)
  "Speak the to line."
  (when (interactive-p)
    (emacspeak-speak-line )))

(defadvice mail-reply-to (after emacspeak pre act)
  "Speak the reply-to line."
  (when (interactive-p)
    (emacspeak-speak-line )))
(defadvice mail-fcc (after emacspeak pre act)
  "Speak the fcc line."
  (when (interactive-p)
    (emacspeak-speak-line )))

(defadvice mail-signature  (after emacspeak pre act)
  "Announce you signed the message."
  (when (interactive-p)
    (message "Signed your message")))

(defadvice mail-send-and-exit (after emacspeak pre act)
  "Speak the modeline of active buffer."
  (when (interactive-p)
    (emacspeak-speak-mode-line )))

;;}}}

(defadvice goto-line (after emacspeak pre act)
  "Speak the line."
  (when (interactive-p)
    (emacspeak-speak-line )))

(defadvice find-tag (after emacspeak pre act)
  "Speak the line please."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line )))
(defadvice pop-tag-mark (after emacspeak pre act)
  "Speak the line please."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice tags-loop-continue (after emacspeak pre act)
  "Speak the line please."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line )))

(defadvice call-last-kbd-macro (around emacspeak pre act)
  "Provide spoken feedback."
  (cond
   ((interactive-p)
    (let ((dtk-quiet t)
          (emacspeak-speak-messages nil)
          (emacspeak-use-auditory-icons nil))
      ad-do-it)
    (message "Executed macro. ")
    (emacspeak-auditory-icon 'task-done))
   (t ad-do-it))
  ad-return-value )

(defadvice kbd-macro-query (after emacspeak pre act)
  "Announce yourself."
  (when (interactive-p)
    (message "Will prompt at this point in macro")))

(defadvice start-kbd-macro (before emacspeak pre act)
  "Announce yourself."
  (when (interactive-p)
    (dtk-speak "Started defining a keyboard macro ")))

(defadvice end-kbd-macro (after emacspeak pre act)
  "Announce yourself."
  (when (interactive-p)
    (dtk-speak "Finished defining keyboard macro ")))

;;; you  DONT WANT TO SUSPEND EMACS WITHOUT CONFIRMATION
(defadvice suspend-emacs (around emacspeak pre act)
  "Ask for confirmation."
  (let ((confirmation (yes-or-no-p "Do you want to suspend emacs ")))
    (cond
     (confirmation
      (message "Suspending Emacs ")
      ad-do-it)
     (t (message "Not suspending emacs")))))

(defadvice  downcase-region (after emacspeak pre act)
  "Give spoken confirmation."
  (when (interactive-p)
    (message "Downcased region containing %s lines"
             (count-lines (region-beginning)
                          (region-end)))))

(defadvice  upcase-region (after emacspeak pre act)
  "Give spoken confirmation."
  (when (interactive-p)
    (message "Upcased  region containing %s lines"
             (count-lines (region-beginning)
                          (region-end)))))

(defadvice narrow-to-region (after emacspeak pre act)
  "Announce yourself."
  (when (interactive-p)
    (message "Narrowed editing region to %s lines"
             (count-lines (region-beginning)
                          (region-end)))))

(defadvice narrow-to-page  (after emacspeak pre act)
  "Announce yourself."
  (when (interactive-p)
    (message "Narrowed editing region to current page ")))

(defadvice widen (after emacspeak pre act)
  "Announce yourself."
  (when (interactive-p)
    (message "You can now edit the entire buffer ")))

(defadvice delete-other-windows (after emacspeak pre act)
  "Provide spoken feedback."
  (when (interactive-p)
    (message "Deleted all other windows")
    (emacspeak-speak-mode-line)))

(defadvice split-window-vertically (after emacspeak pre act)
  "Provide spoken feedback."
  (when (interactive-p)
    (message "Split window vertically, current window has %s lines "
             (window-height))
    (emacspeak-speak-mode-line)))

(defadvice delete-window (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line ))  )

(defadvice shrink-window (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (message "Current window has %s lines  and %s columns"
             (window-height ) (window-width))))

(defadvice shrink-window-if-larger-than-buffer (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (message "Current window has %s lines  and %s columns"
             (window-height ) (window-width))))

(defadvice balance-windows (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (message "Current window has %s lines  and %s columns"
             (window-height ) (window-width))))

(defadvice split-window-horizontally (after emacspeak pre act)
  "Provide spoken feedback."
  (when (interactive-p)
    (message "Split window horizontally current window has %s columns "
             (window-width))
    (emacspeak-speak-mode-line)))

(defadvice transpose-chars (after emacspeak pre act )
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-speak-char  t)))

(defadvice transpose-lines (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'yank-object )
    (emacspeak-speak-line )))

(defadvice transpose-words (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'yank-object )
    (emacspeak-speak-word )))

(defadvice transpose-sexps (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'yank-object )
    (emacspeak-speak-sexp )))

(defadvice open-line (after emacspeak pre act )
  "Provide auditory feedback."
  (when (interactive-p)
    (let ((count (ad-get-arg 0)))
      (emacspeak-auditory-icon 'open-object)
      (message "Opened %s blank line%s"
               (if (= count 1) "a" count)
               (if (= count 1 ) "" "s")))))

(defadvice abort-recursive-edit (after emacspeak pre act comp)
  "Provide  auditory feedback."
  (when (interactive-p)
    (message "Aborting recursive edit")))

(defadvice undo  (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (ems-set-personality-temporarily
     (max (point-min) (1- (point)))
     (min (point-max) (1+ (point)))
     voice-bolden
     (emacspeak-speak-line ))
    (if (buffer-modified-p)
        (emacspeak-auditory-icon 'modified-object)
      (emacspeak-auditory-icon 'unmodified-object ))))

(defadvice view-emacs-news (after emacspeak pre act comp)
  "Provide auditory cue."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))
(defadvice yasb (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line )))

;;}}}
;;{{{  Emacs server

(defadvice server-start (after emacspeak pre act )
  "Provide auditory confirmation."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice server-edit (after emacspeak pre act )
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-speak-mode-line )))

;;}}}
;;{{{ view echo area
(defadvice view-echo-area-messages (after emacspeak pre act comp)
  "Speak mode-line and play auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{ selective display

(defadvice set-selective-display (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (interactive-p)
    (message "Set selective display to %s"
             (ad-get-arg 0))
    (emacspeak-auditory-icon 'button)))

;;}}}
;;{{{  avoid chatter when byte compiling etc

(defadvice byte-compile-file  (around emacspeak pre act)
  "Announce one message, quietly compile, and announce termination.
Produce an auditory icon if possible."
  (cond
   ((interactive-p)
    (let ((emacspeak-speak-messages nil))
      (dtk-speak "Byte compiling ")
      ad-do-it
      (emacspeak-auditory-icon 'task-done)
      (dtk-speak "Done byte compiling ")))
   (t ad-do-it))
  ad-return-value)

;;}}}
;;{{{  Stop talking if activity

(defadvice beginning-of-line (before emacspeak pre act)
  "Stop speech first."
  (when (interactive-p) (dtk-stop )
        (emacspeak-auditory-icon 'select-object)))

(defadvice end-of-line (before emacspeak pre act)
  "Stop speech first."
  (when (interactive-p)  (dtk-stop )
        (emacspeak-auditory-icon 'select-object)))

(defadvice recenter (before emacspeak pre act)
  "Stop speech first."
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (dtk-stop )))

;;}}}
;;{{{  yanking and popping

(defadvice yank (after emacspeak pre act)
  "Say what you yanked.
Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'yank-object )
    (emacspeak-speak-region (mark 'force) (point))))

(defadvice yank-pop (after emacspeak pre act)
  "Say what you yanked.
Also produce an auditory icon if possible."
  (when (interactive-p )
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-speak-region (point) (mark 'force))))

;;}}}
;;{{{  simple searching:

(defadvice search-forward (around emacspeak pre act)
  "Prompt using speech."
  (cond
   ((interactive-p )
    (dtk-speak "Search forward for   ")
    ad-do-it
    (ems-set-personality-temporarily
     (match-beginning 0) (match-end 0) voice-bolden
     (emacspeak-speak-line))
    (if ad-return-value
        (emacspeak-auditory-icon 'search-hit)
      (emacspeak-auditory-icon 'search-miss)))
   (t ad-do-it))
  ad-return-value)

(defadvice search-backward (before emacspeak pre act)
  "Prompt using speech."
  (cond
   ((interactive-p )
    (dtk-speak "Search backward  for ")
    ad-do-it
    (ems-set-personality-temporarily
     (match-beginning 0) (match-end 0) voice-bolden
     (emacspeak-speak-line))
    (if ad-return-value
        (emacspeak-auditory-icon 'search-hit)
      (emacspeak-auditory-icon 'search-miss)))
   (t ad-do-it))
  )

;;}}}
;;{{{  customize isearch:
;;{{{ fix isearch keys:
(declaim (special isearch-mode-map
                  minibuffer-local-isearch-map
                  emacspeak-prefix))

(define-key minibuffer-local-isearch-map emacspeak-prefix
  'emacspeak-prefix-command)
(define-key isearch-mode-map emacspeak-prefix 'emacspeak-prefix-command)

;;}}}
;;{{{  temporarily disable message advice during searches.
(defvar emacspeak-isearch-save-syntax-table  nil
  "Saved syntax table before we enter isearch mode.")

(make-variable-buffer-local
 'emacspeak-isearch-save-syntax-table)

(add-hook 'isearch-mode-hook
          #'(lambda ()
              (declare (special emacspeak-isearch-save-syntax-table))
              (setq emacspeak-isearch-save-syntax-table (syntax-table))
              (setq emacspeak-speak-messages nil)))

(add-hook 'isearch-mode-end-hook
           #'(lambda ()
             (declare (special emacspeak-isearch-save-syntax-table))
             (and emacspeak-isearch-save-syntax-table
                  (set-syntax-table emacspeak-isearch-save-syntax-table))
             (setq emacspeak-speak-messages t )))

;;}}}
;;{{{  Advice isearch-search to speak

(defadvice isearch-forward (before emacspeak pre act comp)
  "Provide auditory feedback.
Pause ongoing speech first."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (dtk-pause)))

(defadvice isearch-backward (before emacspeak pre act comp)
  "Provide auditory feedback.
Pause ongoing speech first."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (dtk-pause)))

(defadvice isearch-forward-regexp (before emacspeak pre act comp)
  "Provide auditory feedback.
Pause ongoing speech first."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (dtk-pause)))

(defadvice isearch-backward-regexp (before emacspeak pre act comp)
  "Provide auditory feedback.
Pause ongoing speech first."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (dtk-pause)))

(defadvice isearch-cancel (before emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)))

(defadvice isearch-search (after emacspeak pre act)
  "Speak the search hit."
  (emacspeak-speak-string isearch-string voice-bolden)
  (when  (sit-for 0.5)
    (emacspeak-auditory-icon 'search-hit)
    (ems-set-personality-temporarily
     (point)
     (if  isearch-forward
         (- (point) (length isearch-string ))
       (+ (point) (length isearch-string )))
     voice-bolden
     (emacspeak-speak-line nil ))))

(defadvice isearch-delete-char (after emacspeak pre act)
  "Speak the search hit.
Produce auditory icons if possible."
  (emacspeak-speak-string isearch-string voice-bolden)
  (when (sit-for 0.5)
    (emacspeak-auditory-icon 'search-hit)
    (ems-set-personality-temporarily
     (point)
     (if  isearch-forward
         (- (point) (length isearch-string ))
       (+ (point) (length isearch-string )))
     voice-bolden
     (emacspeak-speak-line nil ))))

(defadvice isearch-done (around emacspeak pre act comp)
  "Done searching --provide appropriate feedback."
  (let ((emacspeak-speak-messages-pause nil))
    ad-do-it
    ))

(defadvice isearch-exit (around emacspeak pre act comp)
  "Done searching --provide appropriate feedback."
  (let ((emacspeak-speak-messages-pause nil))
    ad-do-it))

(defadvice isearch-abort (around emacspeak pre act comp)
  "Done searching --provide appropriate feedback."
  (let ((emacspeak-speak-messages-pause nil))
    ad-do-it))

;;}}}

(defadvice isearch-yank-word (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-speak-string isearch-string voice-bolden)
    (emacspeak-auditory-icon 'yank-object)))

(defadvice isearch-yank-kill (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-speak-string isearch-string voice-bolden)
    (emacspeak-auditory-icon 'yank-object)))

(defadvice isearch-yank-line (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-speak-string isearch-string voice-bolden)
    (emacspeak-auditory-icon 'yank-object)))

(defadvice isearch-ring-advance (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-speak-string isearch-string voice-bolden)
    (emacspeak-auditory-icon 'select-object)))

(defadvice isearch-ring-retreat (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-string isearch-string voice-bolden)))

(defadvice isearch-ring-advance-edit (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-string isearch-string voice-bolden)))

(defadvice isearch-ring-retreat-edit (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-string isearch-string voice-bolden)))

;;; Note the advice on the next two toggle commands
;;; checks the variable being toggled.
;;; When our advice is called, emacs has not yet reflected
;;; the newly toggled state.

(defadvice isearch-toggle-case-fold (after emacspeak pre act comp)
  "Provide auditory confirmation"
  (emacspeak-auditory-icon
   (if isearch-case-fold-search 'off 'on))
  (dtk-speak
   (format " Case is %s significant in search"
           (if isearch-case-fold-search " not" " "))))

(defadvice isearch-toggle-regexp (after emacspeak pre act comp)
  "Provide auditory confirmation"
  (emacspeak-auditory-icon
   (if isearch-regexp 'on 'off))
  (dtk-speak
   (if isearch-regexp "Regexp search" "text search")))
;;{{{ advice non-incremental searchers
(defadvice search-forward (after emacspeak pre act comp)
  "Speak line we land on."
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))
(defadvice search-backward (after emacspeak pre act comp)
  "Speak line we land on."
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defadvice word-search-forward (after emacspeak pre act comp)
  "Speak line we land on."
  (when (interactive-p)
    (emacspeak-speak-line)this is last ))

(defadvice word-search-backward (after emacspeak pre act comp)
  "Speak line we land on."
  (when (interactive-p)
    (emacspeak-speak-line)this is last ))

;;}}}
;;}}}
;;{{{  marking objects produces auditory icons

;;; Prevent push-mark from displaying its mark set message
;;; when called from functions that know better.
(defvar emacspeak-advice-smart-mark-functions
  (list 'mark-defun
        'mark-whole-buffer
        'mark-paragraph
        'mark-page
        'mark-word
        'mark-perl-function)
  "Functions  that display their own smart mark set message.")

(defadvice push-mark (around emacspeak pre act comp)
  "Never show the mark set message."
  (or (ad-get-arg 1)
      (memq last-command emacspeak-advice-smart-mark-functions)
      (ad-set-arg 1 t))
  ad-do-it
  ad-return-value)

(defadvice set-mark-command (after emacspeak pre act)
  "Produce an auditory icon if possible."
  (when (interactive-p )
    (emacspeak-auditory-icon 'mark-object )
    (ems-set-personality-temporarily (point) (1+ (point))
                                     voice-animate
                                     (emacspeak-speak-line ))))

(defadvice pop-to-mark-command (after emacspeak pre act)
  "Produce an auditory icon if possible."
  (when (interactive-p )
    (emacspeak-auditory-icon 'mark-object )
    (ems-set-personality-temporarily (point) (1+ (point))
                                     voice-animate
                                     (emacspeak-speak-line ))))

(defadvice pop-global-mark (after emacspeak pre act)
  "Produce an auditory icon if possible."
  (when (interactive-p )
    (emacspeak-auditory-icon 'mark-object )
    (ems-set-personality-temporarily (point) (1+ (point))
                                     voice-animate
                                     (emacspeak-speak-line ))
    (emacspeak-speak-mode-line)))

(defadvice mark-defun (after emacspeak pre act)
  "Produce an auditory icon if possible."
  (when (interactive-p )
    (emacspeak-auditory-icon 'mark-object)
    (message "Marked function containing %s lines"
             (count-lines (point)
                          (mark 'force)))))

(defadvice mark-whole-buffer (after emacspeak pre act)
  "Produce an auditory icon if possible."
  (when (interactive-p )
    (emacspeak-auditory-icon 'mark-object)
    (message "Marked buffer  containing %s lines"
             (count-lines (point)
                          (mark 'force)))))

(defadvice mark-paragraph (after emacspeak pre act)
  "Produce an auditory icon if possible."
  (when (interactive-p )
    (emacspeak-auditory-icon 'mark-object)
    (message "Marked paragraph containing %s lines"
             (count-lines (point)
                          (mark 'force)))))

(defadvice mark-page (after emacspeak pre act)
  "Produce an auditory icon if possible."
  (when (interactive-p )
    (emacspeak-auditory-icon 'mark-object)
    (message "Marked page containing %s lines"
             (count-lines (point)
                          (mark 'force)))))

(defadvice mark-word (after emacspeak pre act)
  "Produce an auditory icon if possible."
  (when (interactive-p )
    (emacspeak-auditory-icon 'mark-object)
    (message "Word %s marked"
             (buffer-substring-no-properties (point) (mark 'force)))))

(defadvice mark-sexp (after emacspeak pre act)
  "Produce an auditory icon if possible."
  (when (interactive-p )
    (let ((lines (count-lines (point)
                              (mark 'force)))
          (chars (abs (- (point) (mark 'force)))))
      (emacspeak-auditory-icon 'mark-object)
      (if (> lines 1)
          (message "Marked S expression  spanning %s lines" lines)
        (message "marked S expression containing %s characters"
                 chars)))))

(defadvice mark-end-of-sentence (after emacspeak pre act)
  "Produce an auditory icon if possible."
  (when (interactive-p )
    (emacspeak-auditory-icon 'mark-object)))

;;}}}
;;{{{  emacs registers
(defadvice point-to-register (after emacspeak pre act comp)
  "Produce auditory icon to indicate mark set."
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (if current-prefix-arg
        (message "Stored current frame configuration")
      (emacspeak-speak-line))))

(defadvice copy-to-register (before emacspeak pre act)
  "Acknowledge the copy."
  (when (interactive-p)
    (let ((start (ad-get-arg 1))
          (end (ad-get-arg 2 ))
          (register (ad-get-arg 0))
          (lines nil)
          (chars nil))
      (setq lines (count-lines  start end)
            chars (abs (- start end )))
      (if (> lines 1)
          (message "Copied %s lines to register %c"
                   lines register)
        (message "Copied %s characters to register %c"
                 chars register)))))

(defadvice jump-to-register (after emacspeak pre act)
  "Speak the line you jumped to."
  (when (interactive-p) (emacspeak-speak-line )))

(defadvice insert-register (after emacspeak pre act )
  "Speak the  first line of the inserted text."
  (when (interactive-p)
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-speak-line )))

(defadvice window-configuration-to-register (after emacspeak pre act )
  "Provide auditory feedback."
  (when (interactive-p)
    (message "Copied window configuration to register %c"
             (ad-get-arg 0 ))))
(defadvice frame-configuration-to-register (after emacspeak pre act )
  "Provide auditory feedback."
  (when (interactive-p)
    (message "Copied window configuration to register %c"
             (ad-get-arg 0 ))))

;;}}}
;;{{{  set up clause boundaries for specific modes:
(defsubst emacspeak-speak-adjust-clause-boundaries ()
  "Adjust clause boundaries so that newlines dont delimit clauses."
  (declare (special dtk-chunk-separator-syntax))
  (setq dtk-chunk-separator-syntax
        ".)$\""))

(add-hook 'text-mode-hook
          'emacspeak-speak-adjust-clause-boundaries)
(add-hook 'help-mode-hook 'emacspeak-speak-adjust-clause-boundaries)
;;}}}
;;{{{ setup minibuffer hooks:
(defvar emacspeak-minibuffer-enter-auditory-icon t
  "Produce auditory icon when entering the minibuffer.")

(defun emacspeak-minibuffer-setup-hook ()
  "Actions to take when entering the minibuffer with
emacspeak running."
  (declare (special emacspeak-minibuffer-enter-auditory-icon))
  (let ((inhibit-field-text-motion t))
    (when emacspeak-minibuffer-enter-auditory-icon
      (emacspeak-auditory-icon 'open-object))
    (tts-with-punctuations 'all
                           (emacspeak-speak-buffer))))

(add-hook  'minibuffer-setup-hook 'emacspeak-minibuffer-setup-hook)

(defun emacspeak-minibuffer-exit-hook ()
  "Actions performed when exiting the minibuffer with Emacspeak loaded."
  (emacspeak-auditory-icon 'close-object))

                                        ;(declaim (special minibuffer-exit-hook))
                                        ;(setq minibuffer-exit-hook 'emacspeak-minibuffer-exit-hook)

;;}}}
;;{{{ Advice occur

(defadvice occur-prev (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'large-movement)))
(defadvice occur-next (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'large-movement)))
(defadvice occur-mode-goto-occurrence (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

;;}}}
;;{{{  abbrev mode advice

(defadvice expand-abbrev (around emacspeak pre act comp)
  "Speak what you expanded."
  (let ((start (save-excursion
                 (backward-word 1)
                 (point))))
    ad-do-it
    (dtk-speak
     (buffer-substring
      start (point)))))

(defadvice abbrev-mode (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'button)
    (message "Turned %s abbrev mode"
             (if abbrev-mode "on" "off"))))

;;}}}
;;{{{  Get auto-revert set up if it is available

(when (locate-library "autorevert")
  (declaim (special auto-revert-load-hook
                    auto-revert-mode-text))
  (add-hook 'auto-revert-load-hook
            (function
             (lambda nil
               (declare (special auto-revert-mode-text))
               (setq auto-revert-mode-text " AutoRev")))))

;;}}}
;;{{{ emacs 20 fixups

;;; We do this ugly workaround to compensate for Emacs 20
;;; Emacs 20 relies on binding standard-output to t and then using princ
;;; to display important messages
;;; We introduce a dynamic variable emacspeak-advice-advice-princ
;;; that can be set whenever we want princ to speak.

(defadvice describe-key-briefly (around emacspeak pre act comp)
  "Speak what you displayed"
  (cond
   ((interactive-p)
    ad-do-it
    (let ((dtk-stop-immediately nil))
      (dtk-speak ad-return-value)))
   (t ad-do-it))
  ad-return-value)

(defadvice where-is (around emacspeak pre act comp)
  "Provide spoken feedback"
  (cond
   ((interactive-p)
    ad-do-it
    (emacspeak-speak-message-again))
   (t ad-do-it))
  ad-return-value)

;;}}}
;;{{{ apropos and friends
(defadvice apropos-command (after emacspeak pre act com)
  "Provide an auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'help)))

(defadvice apropos-follow (after emacspeak pre act comp)
  "Speak the help you displayed."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-help)))

;;}}}
;;{{{ speak context after done garbage collecting

(defadvice garbage-collect (after emacspeak pre act comp)
  "Speak modeline when done."
  (when (interactive-p)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'select-object)))
;;}}}
;;{{{ copy-file rename-file and friends

;;; file fileio.c defines these in a way
;;; that defeats our automatic advice for interactive
;;; prompts.

(defadvice copy-file (before fix-prompt pre act comp)
  "Speak interactive prompts."
  (interactive
   (list (read-file-name "Copy file: ")
         (read-file-name "Copy to: "))))

(defadvice rename-file (before fix-prompt pre act comp)
  "Speak interactive prompts."
  (interactive
   (list (read-file-name "Rename file: ")
         (read-file-name "Rename to: "))))

(defadvice add-name-to-file (before fix-prompt pre act comp)
  "Speak interactive prompts."
  (interactive
   (list (read-file-name "Add name to: ")
         (read-file-name "Additional name: "))))

(defadvice make-symbolic-link (before fix-prompt pre act comp)
  "Speak interactive prompts."
  (interactive
   (list (read-file-name "Symbolic link  source: ")
         (read-file-name "Link Target: "))))

;;}}}
;;{{{ toggling debug state

(defadvice toggle-debug-on-error (after emacspeak pre act comp)
  "Produce an auditory icon."
  (when (interactive-p)
    (if debug-on-error
        (emacspeak-auditory-icon 'on)
      nil
      (emacspeak-auditory-icon 'off))
    (message "Turned %s debug on error" debug-on-error)))

(defadvice toggle-debug-on-quit (after emacspeak pre act comp)
  "Produce an auditory icon."
  (when (interactive-p)
    (if debug-on-error
        (emacspeak-auditory-icon 'on)
      nil
      (emacspeak-auditory-icon 'off))
    (message "Turned %s debug on error"
             debug-on-quit)))

;;}}}
;;{{{ alert if entering override mode

(defadvice overwrite-mode (after emacspeak pre act comp)
  "Provide auditory indication that overwrite mode has
changed."
  (when (interactive-p)
    (emacspeak-auditory-icon 'warn-user)
    (message "Turned %s overwrite mode"
             (or  overwrite-mode "off"))))

;;}}}
;;{{{  Options mode and custom

(defadvice customize (after emacspeak pre act comp)
  "Provide status update."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice Edit-options-toggle (after emacspeak pre act
                                      comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'button)))

(defadvice Edit-options-t (after emacspeak pre act
                                 comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'button)))

(defadvice Edit-options-nil (after emacspeak pre act
                                   comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'button)))
;;}}}
;;{{{ fix transient mark mode
(defadvice transient-mark-mode (after emacspeak pre act comp)
  "Transient mark mode is customized by emacspeak.
Variable mark-even-if-inactive is set true ."
  (setq mark-even-if-inactive t)
  (when (interactive-p)
    (emacspeak-auditory-icon
     (if transient-mark-mode 'on 'off))
    (message "Turned %s transient mark mode."
             (if transient-mark-mode "on" "off"))))

;;}}}
;;{{{ sync with tts engine on major mode change.
(add-hook 'change-major-mode-hook 'emacspeak-dtk-sync)

;;}}}
;;{{{ provide auditory icon when window config changes
(defun emacspeak-window-resize (ignore)
  "Play window resize icon."
  (emacspeak-auditory-icon 'window-resize))
(defvar emacspeak-sounds-icon-on-window-resize nil
  "If T then window resize will produce an auditory icon.")

(when emacspeak-sounds-icon-on-window-resize
  (add-hook 'window-size-change-functions
            'emacspeak-window-resize))

;;}}}
;;{{{ advice load and friends

(defadvice load (after emacspeak pre act comp)
  "Fix interactive commands just defined."
  (emacspeak-fix-commands-loaded-from
   (file-name-sans-extension
    (ad-get-arg 0))))
;;}}}
;;{{{ eldoc
(defadvice eldoc-message (around  emacspeak pre act comp)
  "Speech enable ELDoc for the rare times we use it."
  (let ((emacspeak-speak-messages nil))
    ad-do-it
    (when eldoc-last-message
      (dtk-speak eldoc-last-message))
    ad-return-value))
;;}}}
;;{{{ mail aliases
(defadvice expand-mail-aliases (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)))
;;}}}
;;{{{ elint
(defadvice elint-current-buffer (around emacspeak pre act comp)
  "Silence messages while elint is running."
  (cond
   ((interactive-p)
    (let ((emacspeak-speak-messages nil))
      ad-do-it
      (emacspeak-auditory-icon 'task-done)
      (message "Displayed lint results in other window. ")))
   (t ad-do-it))
  ad-return-value)

(defadvice elint-defun (around emacspeak pre act comp)
  "Silence messages while elint is running."
  (cond
   ((interactive-p)
    (let ((emacspeak-speak-messages nil))
      ad-do-it
      (emacspeak-auditory-icon 'task-done)
      (message "Displayed lint results in other window. ")))
   (t ad-do-it))
  ad-return-value)

;;}}}
;;{{{ advice button creation to add coicification:

(defadvice make-text-button (after emacspeak pre act comp)
  "Adds property personality."
  (let ((beg (ad-get-arg 0))
        (end (ad-get-arg 1)))
    (ems-modify-buffer-safely
     (put-text-property beg end
                        'personality
                        voice-bolden))))

(defadvice make-button (after emacspeak pre act comp)
  "Adds property personality."
  (let ((beg (ad-get-arg 0))
        (end (ad-get-arg 1)))
    (ems-modify-buffer-safely
     (put-text-property beg end
                        'personality voice-bolden))))

(defadvice push-button (after emacspeak pre act comp)
  "Produce auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'push-button)))
;;}}}
;;{{{ silence whitespace cleanup:
(loop for f in
      '(whitespace-cleanup whitespace-cleanup-internal)
      do
      (eval
       `(defadvice ,f (around emacspeak pre act comp)
          "Silence messages."
          (let ((emacspeak-speak-messages nil))
            ad-do-it
            ad-return-value))))

;;}}}
;;{{{ advice Finder:

(defadvice finder-mode (after emacspeak pre act comp)
  "Provide auditory feedback"
  (load-library "emacspeak-finder-inf")
  (when(and  (boundp 'finder-known-keywords)
             (not (eq 'emacspeak (caar finder-known-keywords))))
    (push (cons 'emacspeak "Audio Desktop")
          finder-known-keywords))
  (emacspeak-auditory-icon 'open-object)
  (emacspeak-speak-mode-line))

;;}}}
(provide 'emacspeak-advice)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
