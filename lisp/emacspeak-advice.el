;;; emacspeak-advice.el --- Advice Emacs Core   -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description: Core advice forms that make emacspeak work
;;; Keywords: Emacspeak, Speech, Advice, Spoken output
;;{{{ LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2008-08-18 17:52:34 -0700 (Mon, 18 Aug 2008) $ |
;;; $Revision: 4550 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{ Copyright:

;;;Copyright (C) 1995 -- 2018, T. V. Raman
;;; Copyright (c) 1995, 1996, 1997 by T. V. Raman
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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING. If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ Introduction:

;;; Commentary:

;;; This module defines the advice forms for making the core of Emacs speak
;;; Advice forms that are specific to Emacs subsystems do not belong here!
;;; I violate this at present by advising completion comint and
;;; shell here.

;;; Note that we needed to advice a lot more for Emacs 19 and
;;;Emacs 20 than we do for Emacs 21 and Emacs 22.
;;; As of August 2007, this file is being purged of advice forms
;;;not needed in Emacs 22.


;;; Code:

;;}}}
;;{{{ Required modules

(require 'cl-lib)
(cl-declaim (optimize (safety 0) (speed 3)))
(eval-when-compile (require 'advice))
(require 'voice-setup)
(require 'dtk-speak)
(require 'emacspeak-pronounce)
(require 'emacspeak-speak)
;;}}}
;;{{{ Forward Declarations:

(defvar emacspeak-prefix)

;;}}}
;;{{{ Silence advice chatter:

(defadvice ad--cl--defalias-fset (around emacspeak pre act comp)
  "Silence chatter."
  (ems-with-messages-silenced ad-do-it))

;;}}}
;;{{{ Advice ding

(defadvice ding (before emacspeak pre act comp)
  "Produce auditory icon."
  (emacspeak-auditory-icon 'warn-user))

;;}}}
;;{{{  Replace: define personalities

(defcustom emacspeak-replace-personality
  voice-animate
  "Personality used in search and replace to indicate word
that is being replaced."
  :group 'isearch
  :group 'emacspeak
  :type 'symbol)

;;}}}
;;{{{  Advice Replace

(defvar emacspeak-replace-highlight-on nil
  "Flag that says if replace highlight is on.")

(defvar emacspeak-replace-saved-personality nil
  "Value saved before replace-highlight changed the personality. ")

(defvar emacspeak-replace-start nil)
(defvar emacspeak-replace-end nil)

(cl-loop
 for f in
 '(query-replace query-replace-regexp)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p) (emacspeak-auditory-icon 'task-done)))))

(defadvice perform-replace (around emacspeak pre act comp)
  "Silence help message."
  (ems-with-messages-silenced
   ad-do-it))

(defadvice replace-highlight (before emacspeak pre act)
  "Voicify and speak the line containing the replacement. "
  (save-match-data
    (let ((from (ad-get-arg 0))
          (to (ad-get-arg 1)))
      (condition-case nil
          (progn
            (and emacspeak-replace-highlight-on
                 emacspeak-replace-start
                 emacspeak-replace-end
                 (put-text-property
                  (max emacspeak-replace-start (point-min))
                  (min emacspeak-replace-end (point-max))
                  'personality emacspeak-replace-saved-personality))
            (setq emacspeak-replace-highlight-on t
                  emacspeak-replace-start from
                  emacspeak-replace-end to
                  emacspeak-replace-saved-personality
                  (dtk-get-style from))
            (and from to
                 (put-text-property from to 'personality
                                    emacspeak-replace-personality))
            (dtk-stop)
            (emacspeak-speak-line))
        (error nil)))))

(defadvice replace-dehighlight (after emacspeak pre act)
  "Turn off the replacement highlight. "
  (cl-declare (special emacspeak-replace-highlight-on
                       emacspeak-replace-saved-personality
                       emacspeak-replace-start emacspeak-replace-end))
  (save-match-data
    (condition-case nil
        (progn
          (and emacspeak-replace-highlight-on
               emacspeak-replace-start
               emacspeak-replace-end
               (put-text-property
                (max emacspeak-replace-start (point-min))
                (min emacspeak-replace-end (point-max))
                'personality emacspeak-replace-saved-personality)
               (setq emacspeak-replace-start nil
                     emacspeak-replace-end nil
                     emacspeak-replace-highlight-on nil)))
      (error nil))))

;;}}}
;;{{{ advice cursor movement commands to speak

(cl-loop
 for f in
 '(next-line previous-line)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak line that you just moved to. Speak on-screen (visual) line when
`visual-line-mode' is turned on. When `visual-line-mode' is on,
position of point is indicated via an aural highlight. Landing on the
beginning or end of a physical line produces an appropriate auditory icon."
     (when (ems-interactive-p)
       (cond
        ((or line-move-visual visual-line-mode) (emacspeak-speak-visual-line))
        (t (emacspeak-speak-line)))))))

(defadvice kill-visual-line (before emacspeak pre act comp)
  "Speak line we're about to delete."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-speak-visual-line)))

(cl-loop
 for f in
 '(beginning-of-visual-line end-of-visual-line)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak visual line with show-point enabled."
     (when (ems-interactive-p)
       (let ((emacspeak-show-point t))
         (emacspeak-speak-visual-line))))))
(cl-loop
 for f in
 '(
   next-logical-line previous-logical-line
   delete-indentation back-to-indentation
   lisp-indent-line goto-line)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak line that you just moved to."
     (when (ems-interactive-p)
       (emacspeak-speak-line)))))

(cl-loop
 for f in
 '(forward-button backward-button)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Speak the button. Silence messages to reduce chatter."
     (cond
      ((ems-interactive-p)
       (ems-with-messages-silenced
        ad-do-it
        (condition-case nil
            (let* ((button (button-at (point)))
                   (start (button-start button))
                   (end (button-end button)))
              (dtk-speak (buffer-substring start end))
              (emacspeak-auditory-icon 'large-movement))
          (error nil))))
      (t ad-do-it))
     ad-return-value)))

(cl-loop
 for f in
 '(left-char right-char
             backward-char forward-char)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak char under point.
When on a close delimiter, speaking matching open delimiter after a small delay. "
     (when (ems-interactive-p)
       (and dtk-stop-immediately (dtk-stop))
       (emacspeak-speak-char t)
       (when
           (and
            (= ?\) (char-syntax (following-char)))
            (sit-for 0.25))
         (emacspeak-auditory-icon 'item)
         (save-excursion
           (forward-char 1)
           (emacspeak-blink-matching-open)))))))

(cl-loop
 for f in
 '(forward-word right-word)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak the word you just moved to."
     (when (ems-interactive-p)
       (skip-syntax-forward " ")
       (emacspeak-speak-word)))))

(cl-loop
 for f in
 '(backward-word left-word)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak the word you just moved to."
     (when (ems-interactive-p) (emacspeak-speak-word)))))

(cl-loop
 for f in
 '(next-buffer previous-buffer bury-buffer)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'select-object)
       (emacspeak-speak-mode-line)))))
(cl-loop
 for f in
 '(beginning-of-buffer end-of-buffer)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak the line."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'large-movement)
       (emacspeak-speak-line)))))

(cl-loop
 for f in
 '(
   tab-to-tab-stop indent-for-tab-command reindent-then-newline-and-indent
   indent-sexp indent-pp-sexp
   indent-region indent-relative)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'fill-object)
       (emacspeak-speak-current-column)))))

(cl-loop
 for f in
 '(backward-sentence forward-sentence)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak sentence after moving."
     (when (ems-interactive-p) (emacspeak-speak-sentence)))))

(cl-loop
 for f in
 '(forward-sexp backward-sexp
                beginning-of-defun end-of-defun)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Speak sexp after moving."
     (if (ems-interactive-p)
         (let ((start (point))
               (end (line-end-position))
               (emacspeak-show-point t))
           ad-do-it
           (emacspeak-auditory-icon 'large-movement)
           (cond
            ((>= end (point))
             (emacspeak-speak-region start (point)))
            (t (emacspeak-speak-line))))
       ad-do-it)
     ad-return-value)))

(cl-loop
 for f in
 '(forward-paragraph backward-paragraph)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak the paragraph."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'paragraph)
       (emacspeak-speak-paragraph)))))

;;; list navigation:

(cl-loop
 for f in
 '(forward-list backward-list
                up-list backward-up-list down-list)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak line."
     (when (ems-interactive-p)
       (let ((emacspeak-show-point t))
         (emacspeak-auditory-icon 'large-movement)
         (emacspeak-speak-line))))))

(cl-loop
 for f in
 '(forward-page backward-page)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'scroll)
       (emacspeak-speak-page)))))
(cl-loop
 for f in
 '(scroll-up scroll-down
             scroll-up-command scroll-down-command)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak the next screenful."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'scroll)
       (dtk-speak (emacspeak-get-window-contents))))))

;;}}}
;;{{{ Advise modify case commands to speak

(defadvice upcase-word (around emacspeak pre act comp)
  "Provide a tone to indicate that we upper cased the current word.
Speak the word that point lands on after the action
is done. If `upcase-word' is called with a negative argument,
then point does not move. In this case, we speak the words
that were upper cased."
  (cond
   ((ems-interactive-p)
    (dtk-tone-upcase)
    (cond
     ((and (numberp current-prefix-arg)
           (cl-minusp current-prefix-arg))
      ad-do-it
      (let ((start (point)))
        (save-excursion
          (forward-word current-prefix-arg)
          (emacspeak-speak-region start (point)))))
     (t ad-do-it
        (save-excursion
          (skip-syntax-forward " ")
          (if (eobp)
              (message "Upper cased final word in buffer")
            (emacspeak-speak-word))))))
   (t ad-do-it))
  ad-return-value)

(defadvice downcase-word (around emacspeak pre act comp)
  "Provide a tone to indicate that we down cased the current word.
Speak the word that point lands on after the action
is done. If `downcase-word' is called with a negative
argument, then point does not move. In this case, we speak
the words that were down cased."
  (cond
   ((ems-interactive-p)
    (dtk-tone-downcase)
    (cond
     ((and (numberp current-prefix-arg)
           (cl-minusp current-prefix-arg))
      ad-do-it
      (let ((start (point)))
        (save-excursion
          (forward-word current-prefix-arg)
          (emacspeak-speak-region start (point)))))
     (t ad-do-it
        (save-excursion
          (skip-syntax-forward " ")
          (if (eobp)
              (message "Lower cased final word in buffer")
            (emacspeak-speak-word))))))
   (t ad-do-it))
  ad-return-value)

(defadvice capitalize-word (around emacspeak pre act comp)
  "Provide a tone to indicate that we capitalized the current word.
Speak the word that point lands on after the action
is done. If `capitalize-word' is called with a negative
argument, then point does not move. In this case, we speak
the words that were capitalized."
  (cond
   ((ems-interactive-p)
    (dtk-tone-upcase)
    (cond
     ((and (numberp current-prefix-arg)
           (cl-minusp current-prefix-arg))
      ad-do-it
      (let ((start (point)))
        (save-excursion
          (forward-word current-prefix-arg)
          (emacspeak-speak-region start (point)))))
     (t ad-do-it
        (save-excursion
          (skip-syntax-forward " ")
          (if (eobp)
              (message "Capitalized final word in buffer")
            (emacspeak-speak-word))))))
   (t ad-do-it))
  ad-return-value)

;;}}}
;;{{{ Advice insert-char:

(cl-loop
 for f in
 '(ucs-insert insert-char)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak char we inserted."
     (when (ems-interactive-p) (emacspeak-speak-char-name (ad-get-arg 0))))))

;;}}}
;;{{{ Advice deletion commands:

(cl-loop
 for f in
 '(backward-delete-char backward-delete-char-untabify delete-backward-char)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Speak character you're deleting."
     (cond
      ((ems-interactive-p)
       (emacspeak-auditory-icon 'delete-object)
       (emacspeak-speak-this-char (preceding-char))
       ad-do-it)
      (t ad-do-it))
     ad-return-value)))

(cl-loop
 for f in
 '(delete-forward-char delete-char)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Speak character you're deleting."
     (cond
      ((ems-interactive-p)
       (dtk-tone-deletion)
       (emacspeak-speak-char t)
       ad-do-it)
      (t ad-do-it))
     ad-return-value)))

(defadvice kill-word (before emacspeak pre act comp)
  "Speak word before killing it."
  (when (ems-interactive-p)
    (save-excursion
      (skip-syntax-forward " ")
      (when dtk-stop-immediately (dtk-stop))
      (let ((dtk-stop-immediately nil))
        (dtk-tone-deletion)
        (emacspeak-speak-word 1)))))

(defadvice backward-kill-word (before emacspeak pre act comp)
  "Speak word before killing it."
  (when (ems-interactive-p)
    (when dtk-stop-immediately (dtk-stop))
    (let ((start (point))
          (dtk-stop-immediately nil))
      (save-excursion
        (forward-word -1)
        (dtk-tone-deletion)
        (emacspeak-speak-region (point) start)))))

;;; Large deletions also produce auditory icons if possible

(cl-loop
 for f in 
 '(kill-line kill-whole-line)
 do
 (eval
  `(defadvice ,f (before emacspeak pre act comp)
     "Speak line before killing it. "
     (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (when dtk-stop-immediately (dtk-stop))
    (let ((dtk-stop-immediately nil))
      (dtk-tone-deletion)
      (emacspeak-speak-line 1))))))


(defadvice kill-sexp (before emacspeak pre act comp)
  "Speak the sexp you killed."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (when dtk-stop-immediately (dtk-stop))
    (let ((dtk-stop-immediately nil))
      (dtk-tone-deletion)
      (emacspeak-speak-sexp 1))))

(defadvice kill-sentence (before emacspeak pre act comp)
  "Speak the line you killed."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (when dtk-stop-immediately (dtk-stop))
    (let ((dtk-stop-immediately nil))
      (dtk-tone-deletion)
      (emacspeak-speak-line 1))))

(defadvice delete-blank-lines (before emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (let (thisblank singleblank)
      (save-match-data
        (save-excursion
          (forward-line 0)
          (setq thisblank (looking-at "[ \t]*$"))
          ;; Set singleblank if there is just one blank line here.
          (setq singleblank
                (and thisblank
                     (not (looking-at "[ \t]*\n[ \t]*$"))
                     (or (bobp)
                         (progn (forward-line -1)
                                (not (looking-at "[ \t]*$"))))))))
      (cond
       ((and thisblank singleblank)
        (message "Deleting current blank line"))
       (thisblank (message "Deleting surrounding blank lines"))
       (t (message "Deleting possible subsequent blank lines"))))))

;;}}}
;;{{{ advice tabify:

(defcustom emacspeak-untabify-fixes-non-breaking-space t
  "Advice untabify to change non-breaking space chars to space."
  :type 'boolean
  :group 'emacspeak
  :version "37.0")

(defadvice untabify (after emacspeak-fix-nbspc pre act comp)
  "Fix NBSP chars if asked to ---
see option emacspeak-untabify-fixes-non-breaking-space."
  (when emacspeak-untabify-fixes-non-breaking-space
    (let ((start (ad-get-arg 0))
          (end (ad-get-arg 1)))
      (save-excursion
        (save-restriction
          (narrow-to-region start end)
          (goto-char start)
          (while (re-search-forward (format "[%c]+" 160) end 'no-error)
            (replace-match " ")))))))

;;}}}
;;{{{ Advice PComplete

(defadvice pcomplete-list (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'help)
    (emacspeak-auditory-icon 'complete)))

(defadvice pcomplete-show-completions (around emacspeak pre act comp)
  (ems-with-messages-silenced
   ad-do-it))

(defadvice pcomplete (around emacspeak pre act comp)
  "Say what you completed."
  (let ((orig (save-excursion (skip-syntax-backward "^ >") (point))))
    ad-do-it
    (when (ems-interactive-p)
      (emacspeak-speak-region orig (point))
      (emacspeak-auditory-icon 'complete))
    ad-return-value))

;;}}}
;;{{{ Advice hippie expand:
(cl-loop
 for f in
 '(hippie-expand complete)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Speak what was completed."
     (cond
      ((ems-interactive-p)
       (let ((orig (save-excursion (skip-syntax-backward "^ >") (point))))
         (ems-with-messages-silenced
          ad-do-it
          (emacspeak-auditory-icon 'complete)
          (if (< orig (point))
              (dtk-speak (buffer-substring orig (point)))
            (dtk-speak (word-at-point))))))
      (t ad-do-it))
     ad-return-value)))

;;}}}
;;{{{ advice minibuffer to speak

(voice-setup-map-face 'minibuffer-prompt 'voice-bolden)

(defadvice quoted-insert (after emacspeak pre act comp)
  "Speak the character that was inserted."
  (when (ems-interactive-p)
    (emacspeak-speak-this-char (preceding-char))))

(defvar emacspeak-speak-read-events t
  "Set to nil to silence read-event.")

(defadvice read-event (before emacspeak pre act comp)
  "Speak the prompt."
  (when (and emacspeak-speak-read-events (ad-get-arg 0))
    (ems-with-messages-silenced (message (ad-get-arg 0)))
    (tts-with-punctuations 'all
                           (dtk-speak (ad-get-arg 0)))))

(defadvice read-multiple-choice (before emacspeak pre act comp)
  "Provide auditory feedback."
  (let ((dtk-stop-immediately  nil)
        (msg (ad-get-arg 0))
        (choices 
         (mapcar
          #'(lambda (c)
              (format "%c: %s"
                      (cl-first c) (cl-second c)))
          (ad-get-arg 1)))
        (details 
         (mapcar
          #'(lambda (c)
              (format "%c: %s: %s"
                      (cl-first c) (cl-second c)
                      (or (cl-third c) "")))
          (ad-get-arg 1))))
    (emacspeak-auditory-icon 'open-object)
    (ems--log-message
     (concat msg
             (mapconcat #'identity details "\n ")))
    (dtk-speak msg)
    (sox-tones 2 2)
    (dtk-speak-list choices)))


(cl-loop
 for f in
 '(
   next-history-element previous-history-element
   next-line-or-history-element previous-line-or-history-element
   previous-matching-history-element next-matching-history-element)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak the history element just inserted."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'select-object)
       (tts-with-punctuations 'all
         (emacspeak-speak-current-field))))))

(defvar emacspeak-last-message nil
  "Holds the last output generated by the Emacs 'message function.")

(defvar emacspeak-lazy-message-time (current-time)
  "Records when we last spoke a message.")

(defadvice momentary-string-display (around emacspeak pre act comp)
  "Provide spoken feedback."
  (ems-with-messages-silenced
   (let ((msg (ad-get-arg 0))
         (exit (ad-get-arg 2)))
     (dtk-speak
      (format "%s %s"
              msg
              (format "Press %s to exit "
                      (if exit
                          (format "%c" exit)
                        "space"))))
     ad-do-it)))

(defcustom emacspeak-advice-progress-reporter t
  "Set to true if progress reporter should produce an auditory
icon."
  :type 'boolean
  :group 'emacspeak-advice)

(defadvice progress-reporter-do-update (around emacspeak pre act comp)
  "Silence progress reporters for now."
  (ems-with-messages-silenced ad-do-it)
  (when (and ad-return-value emacspeak-advice-progress-reporter)
    (emacspeak-auditory-icon 'progress)))


(defadvice progress-reporter-done (after emacspeak pre act comp)
  "Provide auditory feedback."
  (emacspeak-auditory-icon 'time))

(defvar inhibit-message)
(cl-loop
 for f in
 '( minibuffer-message set-minibuffer-message
    message display-message-or-buffer) do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Speak the message."
     (cl-declare (special emacspeak-last-message inhibit-message
                          emacspeak-speak-messages emacspeak-lazy-message-time))
     (let ((inhibit-read-only t)
           (m nil))
       ad-do-it
       (setq m
             (or 
              (current-message)
              (if (bound-and-true-p minibuffer-message-overlay)
                  (overlay-get minibuffer-message-overlay 'after-string))))
       (when
           (and
            (null inhibit-message)
            m ; our message
            emacspeak-speak-messages    ; speaking messages
            (< 0.1
               (float-time
                (time-subtract (current-time) emacspeak-lazy-message-time))))
         (setq emacspeak-lazy-message-time (current-time)
               emacspeak-last-message (ansi-color-apply m))
         ;;; so we really need to speak it
         (tts-with-punctuations 'all
           (dtk-notify-speak m 'dont-log)))
       ad-return-value))))

(defadvice display-message-or-buffer (after emacspeak pre act comp)
  "Provide auditory feedback."
  (let ((buffer-name (ad-get-arg 1)))
    (when (bufferp ad-return-value)
      (dtk-speak (format "Displayed message in buffer  %s" buffer-name)))))

(declare-function emacspeak-tts-use-notify-stream-p "emacspeak-setup.el" nil)

(with-eval-after-load "eldoc"
  (global-eldoc-mode -1)
  (setq eldoc-idle-delay 3))

(defvar emacspeak-eldoc-speak-explicitly
  (not (emacspeak-tts-use-notify-stream-p))
  "Set to T if not using a separate TTS notification stream.")

(voice-setup-map-face 'eldoc-highlight-function-argument 'voice-bolden)

(defadvice eldoc-message (around emacspeak pre act comp)
  "Speech enable ELDoc."
;;; eldoc flashes message temporarily, we speak from cache."
  (ems-with-messages-silenced
   (let ((cached-message eldoc-last-message))
     ad-do-it
     (when
         (and eldoc-last-message
              emacspeak-eldoc-speak-explicitly
              (not (string-equal cached-message eldoc-last-message)))
       (dtk-speak-and-echo eldoc-last-message))
     ad-return-value)))

(defun emacspeak-eldoc-speak-doc ()
  "Speak Eldoc documentation if available."
  (interactive)
  (cl-declare (special eldoc-documentation-function))
  (cond
   (eldoc-documentation-function
    (tts-with-punctuations
     'all
     (dtk-speak-and-echo
      (or (funcall eldoc-documentation-function) "No ElDoc here "))))
   (t (message "No ElDoc here. "))))

(defvar emacspeak-ange-ftp-last-percent nil
  "Cache the last percentage that emacspeak spoke.")

(defadvice ange-ftp-process-handle-hash (around emacspeak pre act comp)
  "Jibber intelligently."
  (cl-declare (special emacspeak-ange-ftp-last-percent
                       ange-ftp-last-percent))
  (ems-with-messages-silenced
   ad-do-it
   (when (or
          (null emacspeak-ange-ftp-last-percent)
          (>= (abs (- ange-ftp-last-percent emacspeak-ange-ftp-last-percent))
              5))
     (setq emacspeak-ange-ftp-last-percent ange-ftp-last-percent)
     (emacspeak-auditory-icon 'progress)
     (dtk-speak
      (format " %s percent" ange-ftp-last-percent)))))

;;{{{ advising signal

(defcustom emacspeak-speak-errors t
  "Specifies if error messages are cued."
  :type 'boolean
  :group 'emacspeak-speak)

(defvar emacspeak-speak-signals t
  "Specifies if signalled messages are cued.")

(cl-declaim (special command-error-function))
(when (boundp 'command-error-function)
  (setq command-error-function 'emacspeak-error-handler))

(defun emacspeak-error-handler (data context _calling-function)
  "Emacspeak custom error handling function."
  (emacspeak-auditory-icon 'warn-user)
                                        ;(ding)
  (message "%s %s"
           (or context " ")
           (error-message-string data)))

(unless (boundp 'command-error-function)
  (defadvice signal (before emacspeak pre act comp)
    "Provide spoken feedback for signals."
    (let ((error-symbol (ad-get-arg 0))
          (data (ad-get-arg 1)))
      (tts-with-punctuations
       'all
       (dtk-speak (error-message-string (cons error-symbol data)))))))

(unless (boundp 'command-error-function)
  (cl-loop
   for f in
   '(keyboard-quit keyboard-escape-quit)
   do
   (eval
    `(defadvice ,f (before emacspeak pre act comp)
       "Stop speech first."
       (when (ems-interactive-p)
         (dtk-stop)
         (emacspeak-auditory-icon 'warn-user)
         (dtk-speak "quit"))))))

(unless (boundp 'command-error-function)
;;; turn off tool-bar-mode -- since it raises signals during redisplay
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1)))

;;; Silence messages from async handlers:
(defadvice timer-event-handler (around emacspeak pre act comp)
  "Silence messages generated by timer event handlers."
  (ems-with-messages-silenced
   ad-do-it))

;;}}}

;;}}}
;;{{{ Advice completion-at-point:
(defadvice completion-at-point (around emacspeak pre act comp)
  "Say what you completed."
  (let ((orig (save-excursion (skip-syntax-backward "^ >_") (point))))
    ad-do-it
    (when (ems-interactive-p)
      (dtk-speak (buffer-substring orig (point)))
      (emacspeak-auditory-icon 'complete))
    ad-return-value))

;;}}}
;;{{{ advice various input functions to speak:

(defadvice read-passwd (before emacspeak pre act comp)
  "Provide auditory feedback."
  (emacspeak-prompt "pwd"))

(defvar emacspeak-read-char-prompt-cache nil
  "Cache prompt from read-char and friends here for later introspection.")

(cl-loop
 for f in
 '(read-key read-key-sequence read-char read-char-exclusive)
 do
 (eval
  `(defadvice ,f (before emacspeak pre act comp)
     "Speak the prompt"
     (let ((prompt (ad-get-arg 0))
           (dtk-stop-immediately nil))
       (emacspeak-auditory-icon 'item)
       (setq emacspeak-last-message prompt)
       (setq emacspeak-read-char-prompt-cache prompt)
       (tts-with-punctuations
        'all
        (dtk-speak
         (or prompt
             (substring ,(symbol-name f) 5))))))))

(defadvice read-char-choice (before emacspeak pre act comp)
  "Speak the prompt. "
  (let* ((prompt (ad-get-arg 0))
         (chars (ad-get-arg 1))
         (m
          (format
           "%s: %s"
           prompt
           (mapconcat #'(lambda (c) (format "%c" c)) chars ", "))))
    (ems--log-message m)
    (tts-with-punctuations 'all (dtk-speak m))))

;;}}}
;;{{{ advice completion functions to speak:
(cl-loop
 for f in
 '(dabbrev-expand dabbrev-completion)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Say what you completed."
     (when (ems-interactive-p)
       (tts-with-punctuations 'all
                              (dtk-speak dabbrev--last-expansion))))))

(voice-setup-add-map
 '(
   (completions-annotations voice-annotate)
   (completions-common-part voice-monotone)
   (completions-first-difference voice-brighten)))
(cl-loop
 for f in
 '(minibuffer-complete-word minibuffer-complete
                            crm-complete-word crm-complete crm-complete-and-exit
                            crm-minibuffer-complete crm-minibuffer-complete-and-exit)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Say what you completed."
     (cond
      ((ems-interactive-p)
       (ems-with-messages-silenced
        (let ((prior (point)))
          (emacspeak-kill-buffer-carefully "*Completions*")
          ad-do-it
          (if (> (point) prior)
              (tts-with-punctuations
               'all
               (dtk-speak (buffer-substring (point) prior)))
            (emacspeak-speak-completions-if-available)))))
      (t ad-do-it))
     ad-return-value)))

(cl-loop
 for f in
 '(lisp-complete-symbol complete-symbol widget-complete)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Say what you completed."
     (ems-with-messages-silenced
      (let ((prior (save-excursion (skip-syntax-backward "^ >") (point))))
        ad-do-it
        (if (> (point) prior)
            (tts-with-punctuations
             'all
             (dtk-speak (buffer-substring prior (point))))
          (emacspeak-speak-completions-if-available))
        ad-return-value)))))

(define-key minibuffer-local-completion-map "\C-o" 'switch-to-completions)
(defadvice switch-to-completions (after emacspeak pre act comp)
  "Provide spoken feedback."
  (emacspeak-auditory-icon 'select-object)
  (dtk-speak (emacspeak-get-current-completion)))

(defadvice next-completion (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (tts-with-punctuations 'all
                           (dtk-speak (emacspeak-get-current-completion)))))

(defadvice previous-completion (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (tts-with-punctuations 'all
                           (dtk-speak
                            (emacspeak-get-current-completion)))))

(defadvice choose-completion (before emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'button)))

;;}}}
;;{{{ tmm support

(defadvice tmm-goto-completions (after emacspeak pre act comp)
  "announce completions "
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'help)
    (dtk-speak (emacspeak-get-current-completion))))

(defadvice tmm-menubar (before emacspeak pre act comp)
  "Provide an auditory icon."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)))

(defadvice tmm-shortcut (after emacspeak pre act comp)
  "Provide contextual feedback when exitting minibuffer."
  (emacspeak-auditory-icon 'button))

;;}}}
;;{{{ Advice comint:
(defadvice comint-delete-output (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-speak-line)))
(cl-loop
 for f in
 '(comint-history-isearch-backward comint-history-isearch-backward-regexp)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (save-excursion
         (comint-bol-or-process-mark)
         (emacspeak-auditory-icon 'select-object)
         (emacspeak-speak-line 1))))))

(defadvice comint-clear-buffer (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-speak-line)))

(defadvice comint-magic-space (around emacspeak pre act comp)
  "Speak word or completion."
  (cond
   ((ems-interactive-p)
    (ems-with-messages-silenced
     (let ((orig (point))
           (count (ad-get-arg 0)))
       (setq count (or count 1))
       ad-do-it
       (cond
        ((= (point) (+ count orig))
         (save-excursion
           (forward-word -1)
           (emacspeak-speak-word)))
        (t
         (emacspeak-auditory-icon 'complete)
         (emacspeak-speak-region
          (comint-line-beginning-position) (point)))))))
   (t ad-do-it))
  ad-return-value)

(defadvice comint-insert-previous-argument (around emacspeak pre
                                                   act comp)
  "Provide auditory feedback."
  (cond
   ((ems-interactive-p)
    (let ((orig (point)))
      ad-do-it
      (emacspeak-speak-region orig (point))
      (emacspeak-auditory-icon 'yank-object)))
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
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
(voice-setup-add-map
 '(
   (comint-highlight-prompt voice-monotone)
   (comint-highlight-input voice-bolden)))
(cl-declaim (special emacspeak-pronounce-sha-checksum-pattern))

(emacspeak-pronounce-add-dictionary-entry
 'comint-mode
 emacspeak-pronounce-sha-checksum-pattern
 (cons 're-search-forward
       'emacspeak-pronounce-sha-checksum))

(cl-declaim (special emacspeak-pronounce-uuid-pattern))

(emacspeak-pronounce-add-dictionary-entry
 'comint-mode
 emacspeak-pronounce-uuid-pattern
 (cons 're-search-forward
       'emacspeak-pronounce-uuid))
(cl-loop
 for mode in
 '(conf-space-mode conf-unix-mode conf-mode)
 do
 (emacspeak-pronounce-add-dictionary-entry
  mode
  emacspeak-pronounce-uuid-pattern
  (cons 're-search-forward
        'emacspeak-pronounce-uuid)))

(add-hook 'shell-mode-hook 'emacspeak-pronounce-refresh-pronunciations)

(defadvice shell-dirstack-message (around emacspeak pre act comp)
  "Silence messages"
  (ems-with-messages-silenced
   ad-do-it))

(add-hook 'comint-mode-hook 'emacspeak-comint-speech-setup)

(defadvice comint-delchar-or-maybe-eof (around emacspeak pre act comp)
  "Speak character you're deleting."
  (cond
   ((ems-interactive-p)
    (cond
     ((= (point) (point-max))
      (message "Sending EOF to comint process"))
     (t (dtk-tone-deletion)
        (emacspeak-speak-char t)))
    ad-do-it)
   (t ad-do-it))
  ad-return-value)

(defadvice comint-send-eof (before emacspeak pre act comp)
  "Announce what we are doing."
  (when (ems-interactive-p)
    (message "Sending EOF to subprocess")))

(defadvice comint-accumulate (before emacspeak pre act comp)
  "Speak the line we are accumulating."
  (when (ems-interactive-p)
    (save-excursion
      (comint-bol)
      (emacspeak-auditory-icon 'select-object)
      (emacspeak-speak-line 1))))
(cl-loop
 for f in
 '(
   comint-next-matching-input-from-input
   comint-previous-matching-input-from-input)
 do
 (eval
  `(defadvice ,f (after
                  emacspeak pre act comp)
     "Speak the matched input."
     (when (ems-interactive-p)
       (save-excursion
         (goto-char (comint-line-beginning-position))
         (emacspeak-speak-line 1))
       (emacspeak-auditory-icon 'select-object)))))

(defadvice shell-forward-command (after emacspeak pre act
                                        comp)
  "Speak the line showing where point is."
  (when (ems-interactive-p)
    (let ((emacspeak-show-point t))
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'item))))

(defadvice shell-backward-command (after emacspeak pre act
                                         comp)
  "Speak the line showing where point is."
  (when (ems-interactive-p)
    (let ((emacspeak-show-point t))
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'item))))

(defadvice comint-show-output (after emacspeak pre act comp)
  "Speak the line showing where point is."
  (when (ems-interactive-p)
    (let ((emacspeak-show-point t))
      (emacspeak-auditory-icon 'large-movement)
      (emacspeak-speak-region (point) (mark)))))

(defadvice comint-show-maximum-output (after emacspeak pre act
                                             comp)
  "Speak the line showing where point is."
  (when (ems-interactive-p)
    (let ((emacspeak-show-point t))
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'scroll))))

(defadvice comint-bol-or-process-mark (after emacspeak pre act
                                             comp)
  "Speak the line showing where point is."
  (when (ems-interactive-p)
    (let ((emacspeak-show-point t))
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'select-object))))

(defadvice comint-copy-old-input (after emacspeak pre act
                                        comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-speak-line)))

(defadvice comint-output-filter (around emacspeak pre act comp)
  "Make comint speak its output."
  (let ((monitor emacspeak-comint-output-monitor)
        (buffer (process-buffer (ad-get-arg 0)))
        (dtk-stop-immediately nil))
    (with-current-buffer buffer
      ad-do-it
      (when
          (and comint-last-output-start
               emacspeak-comint-autospeak
               (or monitor (eq (window-buffer) buffer)))
        (dtk-speak (ad-get-arg 1)))
      ad-return-value)))

(defadvice comint-dynamic-list-completions (around emacspeak pre act comp)
  "Replacing mouse oriented completer with keyboard friendly equivalent"
  (let ((completions (sort (ad-get-arg 0) 'string-lessp))
        (_common (ad-get-arg 1)))
    (with-output-to-temp-buffer "*Completions*"
      (display-completion-list completions))
    (when nil ad-do-it)                 ; to silence byte-compiler
    (with-current-buffer (get-buffer "*Completions*")
      (set (make-local-variable 'comint-displayed-dynamic-completions)
           completions))
    (next-completion 1)
    (dtk-speak
     (buffer-substring (point) (point-max)))))

(defadvice comint-dynamic-complete (around emacspeak pre act comp)
  "Say what you completed."
  (cond
   ((ems-interactive-p)
    (ems-with-messages-silenced
     (let ((prior (save-excursion (skip-syntax-backward "^ >") (point))))
       ad-do-it
       (if (> (point) prior)
           (tts-with-punctuations
            'all
            (emacspeak-auditory-icon 'complete)
            (dtk-speak (buffer-substring prior (point))))
         (emacspeak-speak-completions-if-available)))))
   (t ad-do-it))
  ad-return-value)

(defadvice comint-next-input (after emacspeak pre act comp)
  "Speak the line."
  (when (ems-interactive-p)
    (tts-with-punctuations
     'all
     (save-excursion
       (goto-char (comint-line-beginning-position))
       (emacspeak-speak-line 1)))
    (emacspeak-auditory-icon 'item)))

(defadvice comint-next-matching-input (after emacspeak pre act comp)
  "Speak the line."
  (when (ems-interactive-p)
    (tts-with-punctuations
     'all
     (save-excursion
       (goto-char (comint-line-beginning-position))
       (emacspeak-speak-line 1)))
    (emacspeak-auditory-icon 'item)))

(defadvice comint-previous-input (after emacspeak pre act comp)
  "Speak the line."
  (when (ems-interactive-p)
    (tts-with-punctuations
     'all
     (save-excursion
       (goto-char (comint-line-beginning-position))
       (emacspeak-speak-line 1)))
    (emacspeak-auditory-icon 'item)))

(defadvice comint-previous-matching-input (after emacspeak pre act comp)
  "Speak the line."
  (when (ems-interactive-p)
    (tts-with-punctuations
     'all
     (save-excursion
       (goto-char (comint-line-beginning-position))
       (emacspeak-speak-line 1)))
    (emacspeak-auditory-icon 'item)))

(defadvice comint-send-input (after emacspeak pre act comp)
  "Flush any ongoing speech."
  (when (ems-interactive-p)
    (dtk-stop)
    (emacspeak-auditory-icon 'progress)))

(defadvice comint-previous-prompt (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'item)
    (if (eolp)
        (emacspeak-speak-line)
      (emacspeak-speak-line 1))))

(defadvice comint-next-prompt (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'item)
    (if (eolp)
        (emacspeak-speak-line)
      (emacspeak-speak-line 1))))

(defadvice comint-get-next-from-history (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'item)
    (save-excursion
      (comint-bol)
      (emacspeak-speak-line 1))))

(defadvice comint-dynamic-list-input-ring (around emacspeak pre act comp)
  "List in help buffer the buffer's input history."
  (cond
   ((ems-interactive-p)
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

(defadvice comint-kill-output (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (message "Nuked output of last command ")))

(defadvice comint-quit-subjob (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (message "Sent quit signal to subjob ")))

(defadvice comint-stop-subjob (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (message "Stopped the subjob")))

(defadvice comint-interrupt-subjob (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (message "Interrupted the subjob")))

(defadvice comint-kill-input (before emacspeak pre act comp)
  "Provide spoken feedback."
  (when (ems-interactive-p)
    (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
      (when (> (point) (marker-position pmark))
        (emacspeak-auditory-icon 'delete-object)
        (emacspeak-speak-region pmark (point))))))

(defadvice comint-dynamic-list-filename-completions
    (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-speak-completions-if-available)))
;;; Directory tracking for shell buffers on  systems that have  /proc
;;; Adapted from Emacs Wiki:
(defun emacspeak-shell-dirtrack-procfs (str)
  "Directory tracking using /proc.
/proc/pid/cwd is a symlink to working directory."
  (prog1 str
    (when (string-match comint-prompt-regexp str)
      (condition-case nil
          (cd
           (file-symlink-p
            (format "/proc/%s/cwd"
                    (process-id (get-buffer-process (current-buffer))))))
        (error)))))
(define-minor-mode dirtrack-procfs-mode
  "Toggle procfs-based directory tracking (Dirtrack-Procfs mode).
With a prefix argument ARG, enable Dirtrack-Procfs mode if ARG is
positive, and disable it otherwise. If called from Lisp, enable
the mode if ARG is omitted or nil.

This is an alternative to `shell-dirtrack-mode' which works by
examining the shell process's current directory with procfs. It
only works on systems that have a /proc filesystem that looks
like Linux's; specifically, /proc/PID/cwd should be a symlink to
process PID's current working directory.

Turning on Dirtrack-Procfs mode automatically turns off
Shell-Dirtrack mode; turning it off does not re-enable it."
  nil "Dir" nil
  (if (not dirtrack-procfs-mode)
      (remove-hook 'comint-preoutput-filter-functions
                   #'emacspeak-shell-dirtrack-procfs t)
    (add-hook
     'comint-preoutput-filter-functions
     #'emacspeak-shell-dirtrack-procfs nil t)
    (shell-dirtrack-mode 0)))
(when (file-exists-p "/proc")
  (add-hook 'shell-mode-hook 'dirtrack-procfs-mode))

;;}}}
;;{{{ Advice centering and filling commands:

(defadvice center-line (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'center)
    (message "Centered current line")))

(defadvice center-region (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'center)
    (message "Centered current region containing %s lines"
             (count-lines (region-beginning) (region-end)))))

(defadvice center-paragraph (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'center)
    (message "Centered current paragraph")))

(cl-loop
 for f in
 '(fill-paragraph lisp-fill-paragraph)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'fill-object)
       (message "Filled current paragraph")))))

(defadvice fill-region (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'fill-object)
    (message "Filled current region containing %s lines"
             (count-lines (region-beginning)
                          (region-end)))))

;;}}}
;;{{{ vc:
(voice-setup-add-map
 '(
   (log-edit-header voice-bolden)
   (log-edit-summary voice-lighten)
   (log-edit-unknown-header voice-monotone)))

;;; helper function: find out vc version:

;;; guess the vc version number from the variable used in minor mode alist
(defun emacspeak-vc-get-version-id ()
  "Return VC version id."
  (cl-declare (special vc-mode))
  (let ((id vc-mode))
    (cond
     ((and vc-mode
           (stringp vc-mode))
      (substring id 5 nil))
     (t " "))))

(defadvice vc-toggle-read-only (around emacspeak pre act comp)
  "Provide auditory feedback."
  (cond
   ((ems-interactive-p)
    (let ((message (format "Checking %s version %s "
                           (if buffer-read-only "out previous " " in new ")
                           (emacspeak-vc-get-version-id))))
      (if buffer-read-only
          (emacspeak-auditory-icon 'open-object)
        (emacspeak-auditory-icon 'close-object))
      ad-do-it
      (message message)))
   (t ad-do-it))
  ad-return-value)

(defadvice vc-next-action (around emacspeak pre act comp)
  "Provide auditory feedback."
  (cond
   ((ems-interactive-p)
    (let ((message (format "Checking %s version %s "
                           (if buffer-read-only "out previous " " in new ")
                           (emacspeak-vc-get-version-id))))
      (if buffer-read-only
          (emacspeak-auditory-icon 'close-object)
        (emacspeak-auditory-icon 'open-object))
      ad-do-it
      (message message)))
   (t ad-do-it))
  ad-return-value)

(defadvice vc-revert-buffer (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)))

(defadvice vc-finish-logentry (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Checked in version %s "
             (emacspeak-vc-get-version-id))))

(cl-loop
 for f in
 '(vc-dir-next-line vc-dir-previous-line
                    vc-dir-next-directory vc-dir-previous-directory)
 do
 (eval
  `(defadvice ,f (after emacspeak-pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'select-object)))))

(defadvice vc-dir-mark-file (after emacspeak-pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'mark-object)))

(defadvice vc-dir-mark (after emacspeak-pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'mark-object)))

(defadvice vc-dir (after emacspeak pre act comp)
  "Produce auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice vc-dir-hide-up-to-date (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (emacspeak-speak-line)))

(defadvice vc-dir-kill-line (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-speak-line)))

;;}}}
;;{{{ composing mail

(cl-loop
 for f in
 '(mail mail-other-window mail-other-frame)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Give some auditory feedback."
     (emacspeak-auditory-icon 'open-object)
     (save-excursion
       (goto-char (point-min))
       (emacspeak-speak-line)))))
(cl-loop
 for f in
 '(mail-text mail-subject mail-cc mail-bcc
             mail-to mail-reply-to mail-fcc)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak the reply-to line."
     (when (ems-interactive-p)
       (emacspeak-speak-line)))))

(defadvice mail-signature (after emacspeak pre act comp)
  "Announce you signed the message."
  (when (ems-interactive-p)
    (message "Signed your message")))

(defadvice mail-send-and-exit (after emacspeak pre act comp)
  "Speak the modeline of active buffer."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{ misc functions that have to be hand fixed:

(defadvice zap-to-char (after emacspeak pre act comp)
  "Speak line that is left."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-speak-line 1)))

(defadvice describe-mode (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (message "Displayed mode help in help window")
    (emacspeak-auditory-icon 'help)))

(cl-loop
 for f in
 '(
   describe-bindings describe-prefix-bindings isearch-describe-bindings)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (message "Displayed key bindings in help window")
       (emacspeak-auditory-icon 'help)))))

(defadvice line-number-mode (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'button)
    (emacspeak-speak-mode-line)))

(defadvice column-number-mode (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'button)
    (emacspeak-speak-mode-line)))

(defadvice not-modified (after emacspeak pre act comp)
  "Provide an auditory icon."
  (when (ems-interactive-p)
    (if (ad-get-arg 0)
        (emacspeak-auditory-icon 'modified-object)
      (emacspeak-auditory-icon 'unmodified-object))))

(defadvice comment-dwim (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (cond
     ((use-region-p)
      (emacspeak-speak-region (region-beginning) (region-end)))
     (t (emacspeak-speak-line)))
    (emacspeak-auditory-icon 'task-done)))

(defadvice comment-region (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (ems-interactive-p)
    (let ((prefix-arg (ad-get-arg 2)))
      (message "%s region containing %s lines"
               (if (and prefix-arg
                        (< prefix-arg 0))
                   "Uncommented"
                 "Commented")
               (count-lines (point) (mark 'force))))))

(cl-loop
 for f in
 '(save-buffer save-some-buffers)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'save-object)))))

(cl-loop
 for f in
 '(delete-region kill-region completion-kill-region)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Indicate region has been killed.
Use an auditory icon if possible."
     (cond
      ((ems-interactive-p)
       (let ((count (count-lines (region-beginning) (region-end))))
         ad-do-it
         (emacspeak-auditory-icon 'delete-object)
         (message "Killed region containing %s lines" count)))
      (t ad-do-it))
     ad-return-value)))

(defadvice kill-ring-save (after emacspeak pre act comp)
  "Indicate that region has been copied to the kill ring.
Produce an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (message "region containing %s lines copied to kill ring "
             (count-lines (region-beginning) (region-end)))))

(defadvice find-file (after emacspeak pre act comp)
  "Play an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(cl-loop
 for f in
 '(kill-buffer kill-current-buffer quit-window)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speech-enabled by emacspeak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'close-object)
       (emacspeak-speak-mode-line)))))

(cl-loop
 for f in
 '(delete-windows-on delete-other-frames
                     delete-window delete-completion-window
                     split-window-below split-window-right
                     split-window-vertically split-window-horizontally)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speech-enabled by emacspeak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'window-resize)
       (emacspeak-speak-mode-line)))))

(cl-loop
 for f in
 '(other-frame other-window
               switch-to-prev-buffer switch-to-next-buffer pop-to-buffer
               switch-to-buffer switch-to-buffer-other-window
               switch-to-buffer-other-frame)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak modeline.
Indicate change of selection with an auditory icon
 if possible."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'select-object)
       (emacspeak-speak-mode-line)))))

(defadvice display-buffer (after emacspeak pre act comp)
  "Provide auditory icon."
  (when (ems-interactive-p)
    (let ((buffer (ad-get-arg 0)))
      (emacspeak-auditory-icon 'open-object)
      (message "Displayed %s" (if (bufferp buffer) (buffer-name buffer) buffer)))))

(defadvice make-frame-command (after emacspeak pre act comp)
  "Indicate that a new frame is being created."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice move-to-window-line (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice rename-buffer (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (ems-interactive-p)
    (emacspeak-speak-mode-line)))

(defadvice local-set-key (before emacspeak pre act comp)
  "Prompt using speech."
  (interactive
   (list
    (read-key-sequence "Locally bind key:")
    (read-command "To command:"))))

(defadvice global-set-key (before emacspeak pre act comp)
  "Provide spoken prompts."
  (interactive
   (list
    (read-key-sequence "Globally bind key:")
    (read-command "To command:"))))

(defadvice modify-syntax-entry (before emacspeak pre act comp)
  "Provide spoken prompts."
  (interactive
   (list
    (read-char "Modify syntax for: ")
    (read-string "Syntax Entry: ")
    current-prefix-arg)))
(defadvice help-follow (after emacspeak pre act comp)
  "Speak the ref we moved to."
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'button)))

;;; Silence help for help
(defadvice help-window-display-message (around emacspeak pre act comp)
  (ems-with-messages-silenced ad-do-it))

(defadvice describe-key (after emacspeak pre act comp)
  "Speak the help."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'help)
    (unless ad-return-value
      (emacspeak-speak-help))))

(cl-loop
 for f in
 '(
   describe-function describe-variable describe-symbol
   describe-face describe-font
   describe-text-properties describe-syntax
   describe-package
   describe-char describe-char-after describe-character-set
   describe-chars-in-region
   describe-coding-system describe-current-coding-system
   describe-current-coding-system-briefly
   describe-current-display-table describe-fontset
   describe-help-keys describe-input-method describe-language-environment
   describe-minor-mode describe-minor-mode-from-indicator describe-minor-mode-from-symbol
   describe-personal-keybindings describe-theme)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak the help."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'help)
       (emacspeak-speak-help)))))

(defadvice help-with-tutorial (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (dtk-set-punctuations 'all)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-predefined-window 1)))



(defadvice exchange-point-and-mark (after emacspeak pre act comp)
  "Speak the line.
Indicate large movement with an auditory icon if possible.
Auditory highlight indicates position of point."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (let ((emacspeak-show-point t))
      (emacspeak-speak-line))))
(cl-loop
 for f in
 '(newline newline-and-indent electric-newline-and-maybe-indent)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Speak the previous line if line echo is on.
See command \\[emacspeak-toggle-line-echo]. Otherwise cue the user to
the newly created  line."
     (cl-declare (special emacspeak-line-echo))
     (cond
      ((ems-interactive-p)
       (cond
        (emacspeak-line-echo (emacspeak-speak-line))
        (t
         ad-do-it
         (dtk-tone 225 75 'force)
         (emacspeak-speak-line))))
      (t ad-do-it))
     ad-return-value)))

(cl-loop
 for f in
 '(eval-last-sexp eval-expression)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Also speaks the result of evaluation."
     (when (ems-interactive-p)
       (let ((dtk-chunk-separator-syntax " .<>()$\"\'"))
         (tts-with-punctuations 'all
                                (dtk-speak
                                 (format "%s" ad-return-value))))))))

(defadvice shell (after emacspeak pre act comp)
  "Announce switching to shell mode.
Provide an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(cl-loop
 for f in
 '(find-tag pop-tag-mark tags-cl-loop-continue)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak the line please."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'open-object)
       (emacspeak-speak-line)))))

(defadvice call-last-kbd-macro (around emacspeak pre act comp)
  "Provide spoken feedback."
  (cond
   ((ems-interactive-p)
    (ems-with-messages-silenced
     (let ((dtk-quiet t)
           (emacspeak-use-auditory-icons nil))
       ad-do-it))
    (message "Executed macro. ")
    (emacspeak-auditory-icon 'task-done))
   (t ad-do-it))
  ad-return-value)

(defadvice kbd-macro-query (after emacspeak pre act comp)
  "Announce yourself."
  (when (ems-interactive-p)
    (message "Will prompt at this point in macro")))

(defadvice start-kbd-macro (before emacspeak pre act comp)
  "Announce yourself."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (dtk-speak "Started defining a keyboard macro ")))

(defadvice end-kbd-macro (after emacspeak pre act comp)
  "Announce yourself."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (dtk-speak "Finished defining keyboard macro ")))

;;; you DONT WANT TO SUSPEND EMACS WITHOUT CONFIRMATION
(defadvice suspend-emacs (around emacspeak pre act comp)
  "Ask for confirmation."
  (let ((confirmation (yes-or-no-p "Do you want to suspend emacs ")))
    (cond
     (confirmation
      (message "Suspending Emacs ")
      ad-do-it)
     (t (message "Not suspending emacs")))))

(defadvice downcase-region (after emacspeak pre act comp)
  "Give spoken confirmation."
  (when (ems-interactive-p)
    (message "Downcased region containing %s lines"
             (count-lines (region-beginning)
                          (region-end)))))

(defadvice upcase-region (after emacspeak pre act comp)
  "Give spoken confirmation."
  (when (ems-interactive-p)
    (message "Upcased region containing %s lines"
             (count-lines (region-beginning)
                          (region-end)))))
(cl-loop
 for f in
 '(narrow-to-region narrow-to-page)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Announce yourself."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'mark-object)
       (message "Narrowed editing region to %s lines"
                (count-lines (region-beginning)
                             (region-end)))))))

(defadvice narrow-to-defun (after emacspeak pre act comp)
  "Announce yourself."
  (when (ems-interactive-p)
    (require 'which-func)
    (emacspeak-auditory-icon 'mark-object)
    (message "Narrowed to function %s"
             (which-function))))

(defadvice widen (after emacspeak pre act comp)
  "Announce yourself."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "You can now edit the entire buffer ")))

(defadvice delete-other-windows (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (ems-interactive-p)
    (message "Deleted all other windows")
    (emacspeak-auditory-icon 'window-resize)
    (emacspeak-speak-mode-line)))

(defadvice split-window-vertically (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (ems-interactive-p)
    (message "Split window vertically, current window has %s lines "
             (window-height))
    (emacspeak-speak-mode-line)))

(defadvice delete-window (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice shrink-window (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (message "Current window has %s lines and %s columns"
             (window-height) (window-width))))

(defadvice shrink-window-if-larger-than-buffer (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (message "Current window has %s lines and %s columns"
             (window-height) (window-width))))

(defadvice balance-windows (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (message "Current window has %s lines and %s columns"
             (window-height) (window-width))))

(defadvice split-window-horizontally (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (ems-interactive-p)
    (message "Split window horizontally current window has %s columns "
             (window-width))
    (emacspeak-speak-mode-line)))

(defadvice transpose-chars (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-speak-char t)))

(defadvice transpose-lines (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-speak-line)))

(defadvice transpose-words (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-speak-word)))

(defadvice transpose-sexps (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-speak-sexp)))

(defadvice open-line (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (let ((count (ad-get-arg 0)))
      (emacspeak-auditory-icon 'open-object)
      (message "Opened %s blank line%s"
               (if (= count 1) "a" count)
               (if (= count 1) "" "s")))))

(defadvice abort-recursive-edit (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (message "Aborting recursive edit")))
(cl-loop
 for f in
 '(undo undo-redo undo-only)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (let ((emacspeak-show-point t))
         (emacspeak-speak-line))
       (if (buffer-modified-p)
           (emacspeak-auditory-icon 'modified-object)
         (emacspeak-auditory-icon 'unmodified-object))))))

(defadvice view-emacs-news (after emacspeak pre act comp)
  "Provide auditory cue."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defvar emacspeak--help-char-helpbuf " *Char Help*"
  "This is hard-coded in subr.el")

(defadvice help-form-show (after emacspeak pre act comp)
  "Speak displayed help form."
  (cl-declare (special emacspeak--help-char-helpbuf))
  (when (buffer-live-p (get-buffer emacspeak--help-char-helpbuf))
    (with-current-buffer emacspeak--help-char-helpbuf
      (goto-char (point-min))
      (emacspeak-speak-buffer))))
(defcustom emacspeak-speak-tooltips nil
  "Enable to get tooltips spoken."
  :type 'boolean
  :group 'emacspeak)

(defadvice tooltip-show-help (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when emacspeak-speak-tooltips
    (let ((msg (ad-get-arg 0)))
      (if msg
          (dtk-speak msg)
        (emacspeak-auditory-icon 'close-object)))))

(cl-loop
 for f in
 '(tooltip-show-help-non-mode tooltip-sho)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak the tooltip."
     (when emacspeak-speak-tooltips
       (let ((help (ad-get-arg 0)))
         (dtk-speak help)
         (emacspeak-auditory-icon 'help))))))

;;}}}
;;{{{ Emacs server
(defun emacspeak-speak-announce-server-buffer ()
  "Announce opening of an emacsclient buffer."
  (emacspeak-speak-mode-line)
  (emacspeak-auditory-icon 'open-object))
(add-hook 'server-done-hook
          #'(lambda nil
              (emacspeak-auditory-icon 'close-object)))

(add-hook 'server-switch-hook 'emacspeak-speak-announce-server-buffer)

(defadvice server-start (after emacspeak pre act comp)
  "Provide auditory confirmation."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice server-edit (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{ view echo area

(defadvice view-echo-area-messages (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Displayed messages in other window.")))

;;}}}
;;{{{ selective display

(defadvice set-selective-display (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (ems-interactive-p)
    (message "Set selective display to %s"
             (ad-get-arg 0))
    (emacspeak-auditory-icon 'button)))

;;}}}
;;{{{ avoid chatter when byte compiling etc

(defadvice byte-compile-file (around emacspeak pre act comp)
  "Announce one message, quietly compile, and announce termination.
Produce an auditory icon if possible."
  (cond
   ((ems-interactive-p)
    (ems-with-messages-silenced
     (dtk-speak "Byte compiling ")
     ad-do-it
     (emacspeak-auditory-icon 'task-done)
     (dtk-speak "Done byte compiling ")))
   (t ad-do-it))
  ad-return-value)

;;}}}
;;{{{ Stop talking if activity

(cl-loop
 for f in
 '(beginning-of-line end-of-line
                     move-beginning-of-line move-end-of-line
                     recenter-top-bottom recenter)
 do
 (eval
  `(defadvice ,f (before emacspeak pre act comp)
     "Speak line."
     (when (ems-interactive-p)
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'select-object)))))

;;}}}
;;{{{ yanking and popping

(cl-loop
 for f in
 '(yank yank-pop)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Say what you yanked.
Produce an auditory icon if possible."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'yank-object)
       (emacspeak-speak-region (mark 'force) (point))))))

;;}}}
;;{{{ advice non-incremental searchers

(cl-loop
 for f in
 '(search-forward search-backward
                  word-search-forward word-search-backward)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak line we land on."
     (when (ems-interactive-p)
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'search-hit)))))

;;}}}
;;{{{ customize isearch:

;;; Fix key bindings:

(cl-declaim (special isearch-mode-map
                     minibuffer-local-isearch-map emacspeak-prefix))

(define-key minibuffer-local-isearch-map
  emacspeak-prefix 'emacspeak-prefix-command)
(define-key isearch-mode-map emacspeak-prefix 'emacspeak-prefix-command)
(define-key isearch-mode-map "\M-m" 'isearch-exit)
(define-key isearch-mode-map "\C-f" 'isearch-forward-symbol-at-point)
(define-key isearch-mode-map (kbd "C-.") 'isearch-occur)
;;; ISearch setup/teardown

;;; Produce auditory icon
(defun emacspeak-isearch-setup ()
  "Setup emacspeak environment for isearch."
  (emacspeak-auditory-icon 'open-object)
  (setq emacspeak-speak-messages nil)
  (dtk-speak (isearch-message-prefix)))

(defun emacspeak-isearch-teardown ()
  "Teardown emacspeak environment for isearch."
  (setq emacspeak-speak-messages t)
  (emacspeak-auditory-icon 'close-object))

(add-hook 'isearch-mode-hook 'emacspeak-isearch-setup)
(add-hook 'isearch-mode-end-hook 'emacspeak-isearch-teardown)
(add-hook 'isearch-mode-end-hook-quit 'emacspeak-isearch-teardown)

;;; Advice isearch-search to speak
(defadvice isearch-search (after emacspeak pre act comp)
  "Speak the search hit."
  (cond
   ((null isearch-success) (emacspeak-auditory-icon 'search-miss))
   (t
    (emacspeak-auditory-icon 'search-hit)
    (when (sit-for 0.2)
      (save-excursion
        (ems-set-personality-temporarily
         (point) isearch-other-end voice-bolden
         (dtk-speak
          (buffer-substring (line-beginning-position) (line-end-position)))))))))

(defadvice isearch-delete-char (after emacspeak pre act comp)
  "Speak the search hit.
Produce auditory icons if possible."
  (dtk-speak (propertize isearch-string 'personality  voice-bolden))
  (when (sit-for 0.5)
    (emacspeak-auditory-icon 'search-hit)
    (ems-set-personality-temporarily
     (point)
     (if isearch-forward
         (- (point) (length isearch-string))
       (+ (point) (length isearch-string)))
     voice-bolden
     (emacspeak-speak-line))))
(cl-loop
 for f in
 '(isearch-yank-word isearch-yank-kill isearch-yank-line)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (dtk-speak (propertize  isearch-string voice-bolden))
       (emacspeak-auditory-icon 'yank-object)))))
(cl-loop
 for f in
 '(isearch-ring-advance isearch-ring-retreat
                        isearch-ring-advance-edit isearch-ring-retreat-edit)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-speak-string isearch-string voice-bolden)
       (emacspeak-auditory-icon 'item)))))

;;; Note the advice on the next two toggle commands
;;; checks the variable being toggled.
;;; When our advice is called, emacs has not yet reflected
;;; the newly toggled state.

(defadvice isearch-toggle-case-fold (after emacspeak pre act comp)
  "Provide auditory confirmation"
  (emacspeak-auditory-icon (if isearch-case-fold-search 'off 'on))
  (dtk-speak
   (format " Case is %s significant in search"
           (if isearch-case-fold-search " not" " "))))

(defadvice isearch-toggle-regexp (after emacspeak pre act comp)
  "Provide auditory confirmation"
  (emacspeak-auditory-icon (if isearch-regexp 'on 'off))
  (dtk-speak
   (if isearch-regexp "Regexp search" "text search")))

(defadvice isearch-occur (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (dtk-speak "Opened occur results")))

;;}}}
;;{{{ marking objects produces auditory icons

;;; Prevent push-mark from displaying its mark set message
;;; when called from functions that know better.

(defadvice push-mark (around emacspeak pre act comp)
  "Never show the mark set message."
  (ems-with-messages-silenced ad-do-it))
(cl-loop
 for f in
 '(set-mark-command pop-to-mark-command)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Produce an auditory icon if possible."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'mark-object)
       (let ((emacspeak-show-point t))
         (emacspeak-speak-line))))))

(defadvice pop-global-mark (after emacspeak pre act comp)
  "Speak buffer name if notification stream is available."
  (when (ems-interactive-p)
    (let ((emacspeak-show-point t))
      (emacspeak-speak-line))
    (when (process-live-p dtk-notify-process)
      (dtk-notify-speak (buffer-name)))))

(defadvice mark-defun (after emacspeak pre act comp)
  "Produce an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (message "Marked function containing %s lines"
             (count-lines (point) (mark 'force)))))

(defadvice mark-whole-buffer (after emacspeak pre act comp)
  "Produce an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (dtk-speak-and-echo
     (format "Marked buffer containing %s lines"
             (count-lines (point) (mark 'force))))))

(defadvice mark-paragraph (after emacspeak pre act comp)
  "Produce an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (dtk-speak-and-echo
     (format "Marked paragraph containing %s lines"
             (count-lines (point)
                          (mark 'force))))))

(defadvice mark-page (after emacspeak pre act comp)
  "Produce an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (dtk-speak-and-echo
     (format "Marked page containing %s lines"
             (count-lines (point) (mark 'force))))))

(defadvice mark-word (after emacspeak pre act comp)
  "Produce an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (dtk-speak-and-echo
     (format "Word %s marked"
             (buffer-substring-no-properties (point) (mark 'force))))))

(defadvice mark-sexp (after emacspeak pre act comp)
  "Produce an auditory icon if possible."
  (when (ems-interactive-p)
    (let ((lines (count-lines (point) (marker-position (mark-marker))))
          (chars (abs (- (point) (marker-position (mark-marker))))))
      (emacspeak-auditory-icon 'mark-object)
      (dtk-speak-and-echo
       (if (> lines 1)
           (format "Marked S expression spanning %s lines" lines)
         (format "marked S expression containing %s characters" chars))))))

(defadvice mark-end-of-sentence (after emacspeak pre act comp)
  "Produce an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'mark-object)))

;;}}}
;;{{{ emacs registers

(defadvice point-to-register (after emacspeak pre act comp)
  "Produce auditory icon to indicate mark set."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (if current-prefix-arg
        (message "Stored current frame configuration")
      (emacspeak-speak-line))))

(defadvice copy-to-register (after emacspeak pre act comp)
  "Acknowledge the copy."
  (when (ems-interactive-p)
    (let ((start (ad-get-arg 1))
          (end (ad-get-arg 2))
          (register (ad-get-arg 0))
          (lines nil)
          (chars nil))
      (setq lines (count-lines start end)
            chars (abs (- start end)))
      (if (> lines 1)
          (message "Copied %s lines to register %c"
                   lines register)
        (message "Copied %s characters to register %c"
                 chars register)))))
(defadvice view-register (after emacspeak pre act comp)
  "Speak displayed contents."
  (when (ems-interactive-p)
    (with-current-buffer "*Output*"
      (dtk-speak (buffer-string))
      (emacspeak-auditory-icon 'open-object))))

(defadvice jump-to-register (after emacspeak pre act comp)
  "Speak the line you jumped to."
  (when (ems-interactive-p)
    (let ((emacspeak-show-point t))
      (emacspeak-speak-line))))
(defadvice insert-parentheses (after emacspeak pre act comp)
  "Speak what you inserted."
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'open-object)))
(defadvice insert-register (after emacspeak pre act comp)
  "Speak the first line of the inserted text."
  (when (ems-interactive-p)
    (let ((emacspeak-show-point t))
      (emacspeak-auditory-icon 'yank-object)
      (emacspeak-speak-line))))

(defadvice window-configuration-to-register (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (message "Copied window configuration to register %c" (ad-get-arg 0))))
(cl-loop
 for f in
 '(frameset-to-register frame-configuration-to-register)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (message "Copied frame  configuration to register %c" (ad-get-arg 0))))))

;;}}}
;;{{{ set up clause boundaries for specific modes:

(defun emacspeak-speak-adjust-clause-boundaries ()
  "Adjust clause boundaries so that newlines dont delimit clauses."
  (cl-declare (special dtk-chunk-separator-syntax))
  (setq dtk-chunk-separator-syntax ".)$\""))

(add-hook 'help-mode-hook 'emacspeak-speak-adjust-clause-boundaries)
(add-hook 'text-mode-hook 'emacspeak-speak-adjust-clause-boundaries)

;;}}}
;;{{{ setup minibuffer hooks:

(defun emacspeak-minibuffer-setup-hook ()
  "Actions to take when entering the minibuffer with emacspeak running."
  (cl-declare (special minibuffer-exit-hook minibuffer-default))
  (let ((inhibit-field-text-motion t))
    (unless (memq 'emacspeak-minibuffer-exit-hook minibuffer-exit-hook)
      (add-hook 'minibuffer-exit-hook #'emacspeak-minibuffer-exit-hook))
    (emacspeak-auditory-icon 'open-object)
    (when minibuffer-default (emacspeak-auditory-icon 'help))
    (tts-with-punctuations
     'all
     (dtk-speak
      (concat
       (buffer-string)
       (if (stringp minibuffer-default)
           minibuffer-default
         ""))))))

(add-hook 'minibuffer-setup-hook 'emacspeak-minibuffer-setup-hook 'at-end)

(defun emacspeak-minibuffer-exit-hook ()
  "Actions performed when exiting the minibuffer with Emacspeak loaded."
  (dtk-stop)
  (emacspeak-auditory-icon 'close-object))

(add-hook 'minibuffer-exit-hook #'emacspeak-minibuffer-exit-hook)
;;}}}
;;{{{ Advice occur

(cl-loop
 for f in
 '(occur-prev occur-next occur-mode-goto-occurrence)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide spoken feedback."
     (when (ems-interactive-p)
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'large-movement)))))
(defadvice occur-mode-display-occurrence (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Displayed occurrence in other window")))

;;}}}
;;{{{ abbrev mode advice

(defadvice abbrev-edit-save-buffer (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'save-object)
    (dtk-speak "Saved Abbrevs")))

(defadvice edit-abbrevs-redefine (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (dtk-speak "Redefined abbrevs")))

(defadvice list-abbrevs (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Displayed abbrevs in other window.")))

(defadvice edit-abbrevs (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice expand-abbrev (around emacspeak pre act comp)
  "Speak what you expanded."
  (when buffer-read-only (dtk-speak "Buffer is read-only. "))
  (cond
   ((ems-interactive-p)
    (let ((start (save-excursion (backward-word 1) (point))))
      ad-do-it
      (dtk-speak (buffer-substring start (point)))))
   (t ad-do-it))
  ad-return-value)

(defadvice abbrev-mode (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'button)
    (message "Turned %s abbrev mode"
             (if abbrev-mode "on" "off"))))

;;}}}
;;{{{ advice where-is and friends
(defun ems-canonicalize-key-description (desc)
  "Change key description to a speech-friendly form."
  (let ((shift-regexp "S-\\(.\\)")
        (ctrl-regexp "C-\\(.\\)")
        (meta-regexp "M-\\(.\\)")
        (caps-regexp "\\b[A-Z]\\b")
        (hyper-regexp "C-x @ h")
        (alt-regexp "C-x @ a")
        (super-regexp "C-x @ s"))
        (with-temp-buffer
          (setq buffer-undo-list t)
          (setq case-fold-search nil)
          (erase-buffer)
          (insert desc)
          (goto-char (point-min))
          (save-match-data
            (while (search-forward "SPC" nil t)
              (replace-match "space"))
            (goto-char (point-min))
            (while (search-forward "ESC" nil t)
              (replace-match "escape"))
            (goto-char (point-min))
            (while (search-forward "RET" nil t)
              (replace-match "return"))
            (goto-char (point-min))
            (while (re-search-forward hyper-regexp nil t)
              (replace-match "hyper "))
            (goto-char (point-min))
            (while (re-search-forward alt-regexp nil t)
              (replace-match "alt "))
            (goto-char (point-min))
            (while (re-search-forward super-regexp nil t)
              (replace-match "super "))
            (goto-char (point-min))
            (while (re-search-forward shift-regexp nil t)
              (replace-match "shift \\1"))
            (goto-char (point-min))
            (while (re-search-forward ctrl-regexp nil t)
              (replace-match "control \\1"))
            (goto-char (point-min))
            (while (re-search-forward meta-regexp nil t)
              (replace-match "meta \\1"))
            (goto-char (point-min))
            (goto-char (point-min))
            (while (re-search-forward caps-regexp nil t)
              (replace-match " cap \\& " t)))
          (buffer-string))))


(defadvice describe-key-briefly (around emacspeak pre act comp)
  "Speak what you displayed"
  (cond
   ((ems-interactive-p)
    (let ((emacspeak-speak-messages nil))
      ad-do-it
      (dtk-speak (ems-canonicalize-key-description ad-return-value))))
   (t ad-do-it)))

(defun ems--get-where-is (cmd )
  "Return string describing keys that invoke `cmd'. "
  (let* ((keys (where-is-internal cmd overriding-local-map nil nil ))
	 (desc (mapconcat 'key-description keys ", ")))
    (concat
     (format "%s is on " cmd)
     (ems-canonicalize-key-description desc))))

(defadvice where-is (after emacspeak pre act comp)
  "Provide spoken feedback"
  (when (ems-interactive-p)
    (dtk-speak (ems--get-where-is (ad-get-arg 0)))))

;;}}}
;;{{{ apropos and friends
(cl-loop
 for f in
 '(
   apropos apropos-char apropos-library
   apropos-unicode apropos-user-option apropos-value apropos-variable
   apropos-command apropos-documentation)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide an auditory icon."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'help)
       (message "Displayed apropos in other window.")))))

(defadvice apropos-follow (after emacspeak pre act comp)
  "Speak the help you displayed."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-help)))

;;}}}
;;{{{ speak context after done garbage collecting

(defadvice garbage-collect (after emacspeak pre act comp)
  "Speak modeline when done."
  (when (ems-interactive-p)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'task-done)))

;;}}}
;;{{{ toggling debug state

(defadvice toggle-debug-on-error (after emacspeak pre act comp)
  "Produce an auditory icon."
  (when (ems-interactive-p)
    (if debug-on-error
        (emacspeak-auditory-icon 'on)
      nil
      (emacspeak-auditory-icon 'off))
    (message "Turned %s debug on error" debug-on-error)))

(defadvice toggle-debug-on-quit (after emacspeak pre act comp)
  "Produce an auditory icon."
  (when (ems-interactive-p)
    (if debug-on-error
        (emacspeak-auditory-icon 'on)
      nil
      (emacspeak-auditory-icon 'off))
    (message "Turned %s debug on quit"
             debug-on-quit)))

;;}}}
;;{{{ alert if entering override mode

(defadvice overwrite-mode (after emacspeak pre act comp)
  "Provide auditory indication that overwrite mode has changed."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'warn-user)
    (message "Turned %s overwrite mode" (or overwrite-mode "off"))))

;;}}}
;;{{{ Options mode and custom

(defadvice customize (after emacspeak pre act comp)
  "Provide status update."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))
(defadvice customize-save-variable (around emacspeak pre act comp)
  "Silence chatter."
  (ems-with-messages-silenced
   (let ((dtk-quiet t))
     ad-do-it)))

;;}}}
;;{{{ transient mark mode

(defadvice transient-mark-mode (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon (if transient-mark-mode 'on 'off))
    (message "Turned %s transient mark." (if transient-mark-mode "on" "off"))))

;;}}}
;;{{{ provide auditory icon when window config changes

;;}}}
;;{{{ mail aliases

(defadvice expand-mail-aliases (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (let ((end (point))
          (start (re-search-backward " " nil t)))
      (message (buffer-substring start end))
      (emacspeak-auditory-icon 'select-object))))

;;}}}
;;{{{ elint

(cl-loop
 for f in
 '(elint-current-buffer elint-file elint-defun)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Silence messages while elint is running."
     (cond
      ((ems-interactive-p)
       (ems-with-messages-silenced
        ad-do-it
        (emacspeak-auditory-icon 'task-done)
        (message "Displayed lint results in other window. ")))
      (t ad-do-it))
     ad-return-value)))

;;}}}
;;{{{ advice button creation to add voicification:

(cl-loop
 for f in
 '(make-button make-text-button)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Adds property personality."
     (let ((start (ad-get-arg 0))
           (end (ad-get-arg 1)))
       (with-silent-modifications
         (condition-case nil
             (progn
               (put-text-property start end 'auditory-icon 'button))
           (error nil)))))))

(defadvice push-button (after emacspeak pre act comp)
  "Produce auditory icon."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'button)
    (emacspeak-speak-line)))

;;}}}
;;{{{ silence whitespace cleanup:

(cl-loop
 for f in
 '(whitespace-cleanup whitespace-cleanup-internal)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Silence messages."
     (ems-with-messages-silenced
      ad-do-it
      ad-return-value))))

;;}}}
;;{{{ advice Finder:
(defadvice finder-commentary (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-speak-buffer)
    (emacspeak-auditory-icon 'open-object)))

(defadvice finder-mode (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (and (boundp 'finder-known-keywords)
             (not (eq 'emacspeak (caar finder-known-keywords))))
    (push (cons 'emacspeak "Audio Desktop")
          finder-known-keywords))
  (emacspeak-auditory-icon 'open-object)
  (emacspeak-speak-mode-line))

(defadvice finder-exit (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (with-current-buffer (window-buffer (selected-window))
      (emacspeak-speak-mode-line))))

;;}}}
;;{{{ display world time

(defadvice world-clock (after emacspeak pre act comp)
  "Speak what you displayed."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (save-current-buffer
      (set-buffer "*wclock*")
      (emacspeak-speak-buffer))))

;;}}}
;;{{{ browse-url

(cl-loop for f in
         '(browse-url-of-buffer browse-url-of-region)
         do
         (eval
          `(defadvice ,f (around emacspeak pre act comp)
             "Automatically speak results of rendering."
             (cond
              ((ems-interactive-p)
               (emacspeak-auditory-icon 'open-object)
               (emacspeak-webutils-autospeak)
               ad-do-it)
              (t ad-do-it))
             ad-return-value)))

;;}}}
;;{{{ Cue input method changes

(defadvice toggle-input-method (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon (if current-input-method 'on 'off))
    (dtk-speak
     (format "Current input method is %s"
             (or current-input-method "none")))))

;;}}}
;;{{{ silence midnight cleanup:
(defadvice clean-buffer-list (around emacspeak pre act comp)
  (ems-with-messages-silenced ad-do-it))

;;}}}
;;{{{ Splash Screen:

(cl-loop
 for f in
 '(about-emacs display-about-screen)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'open-object)
       (with-current-buffer (window-buffer (selected-window))
         (emacspeak-speak-buffer))))))

(defadvice exit-splash-screen (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{ copyright commands:

(cl-loop for f in
         '(copyright copyright-update)
         do
         (eval
          `(defadvice ,f (after emacspeak pre act comp)
             "Provide auditory feedback."
             (when (ems-interactive-p)
               (emacspeak-auditory-icon 'task-done)
               (emacspeak-speak-line)))))

(defadvice copyright-update-directory (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)))

;;}}}
;;{{{ Asking Questions:

(defadvice yes-or-no-p (before emacspeak pre act comp)
  "Play auditory icon."
  (emacspeak-auditory-icon 'ask-question))

(defadvice yes-or-no-p (after emacspeak pre act comp)
  "Play auditory icon."
  (cond
   (ad-return-value
    (emacspeak-auditory-icon 'yes-answer))
   (t (emacspeak-auditory-icon 'no-answer))))

(defadvice ask-user-about-lock (before emacspeak pre act comp)
  "Play auditory icon."
  (emacspeak-auditory-icon 'ask-short-question))

(defadvice ask-user-about-lock (after emacspeak pre act comp)
  "Play auditory icon."
  (cond
   (ad-return-value (emacspeak-auditory-icon 'y-answer))
   (t (emacspeak-auditory-icon 'n-answer))))

(defadvice ask-user-about-lock-help (after emacspeak pre act comp)
  "Play auditory icon."
  (emacspeak-auditory-icon 'help))

(defadvice y-or-n-p (before emacspeak pre act comp)
  "Play auditory icon."
  (emacspeak-auditory-icon 'ask-short-question))

(defadvice y-or-n-p (after emacspeak pre act comp)
  "Play auditory icon."
  (cond
   (ad-return-value (emacspeak-auditory-icon 'y-answer))
   (t (emacspeak-auditory-icon 'n-answer))))

;;}}}
;;{{{ Advice process-menu

(defadvice process-menu-delete-process (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-speak-line)))

(defadvice list-processes (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Displayed process list in other window.")))

(defadvice timer-list (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{list-timers:

(defadvice list-timers (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

;;}}}
;;{{{find-library:

(defadvice find-library (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{lv-message:

(defvar ems--lv-cache nil
  "Emacspeak's private cache of the last lv message.")

(voice-setup-set-voice-for-face 'lv-separator  'inaudible)

(defadvice lv-message (after emacspeak pre act comp)
  "Provide auditory feedback."
  (cl-declare (special ems--lv-cache))
  (emacspeak-auditory-icon 'help)
  (with-current-buffer (window-buffer (lv-window))
    (setq ems--lv-cache (buffer-substring (point-min) (point-max)))
    (emacspeak-speak-buffer)))

(defadvice lv-delete-window (after emacspeak pre act comp)
  "Provide auditory feedback."
  (dtk-stop)
  (emacspeak-auditory-icon 'delete-object))

;;}}}
;;{{{log-edit-done

(defadvice log-edit-done (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'close-object)))


;;}}}
(provide 'emacspeak-advice)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
