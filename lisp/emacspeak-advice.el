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

;;;Copyright (C) 1995 -- 2021, T. V. Raman
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
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ Introduction:

;;; Commentary:

;;; This module defines the advice forms for making the core of Emacs speak
;;; Advice forms that are specific to Emacs subsystems do not belong here!
;;; I violate this at present by advising completion.
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
(require 'emacspeak-preamble)

;;}}}
;;{{{  Advice Replace

(voice-setup-set-voice-for-face 'query-replace 'voice-animate)

(cl-loop
 for f in
 '(query-replace query-replace-regexp) do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Icon"
     (when (ems-interactive-p) (emacspeak-auditory-icon 'task-done)))))

(defadvice perform-replace (around emacspeak pre act comp)
  "Silence help."
  (ems-with-messages-silenced ad-do-it))

(defadvice replace-highlight (after emacspeak pre act comp)
  "Speak line. "
  (emacspeak-speak-line))

;;}}}
;;{{{ advice overlays

;;; Helpers:

(defun ems--add-personality (start end voice &optional object)
  "Apply personality VOICE."
  (when
      (and
       (integerp start) (integerp end)
       (not (= start end)))
    (with-current-buffer
        (if (bufferp object) object (current-buffer))
      (with-silent-modifications
        (put-text-property start end 'personality voice object)))))

(defun ems--remove-personality  (start end voice &optional object)
  "Remove  personality. "
  (when
      (and
       voice
       (integerp start) (integerp end)
       (not (= start end))
       (eq voice (get-text-property start 'personality object)))
      (with-current-buffer
          (if (bufferp object) object (current-buffer))
        (with-silent-modifications
          (put-text-property start end 'personality nil object)))))

(defvar ems--voiceify-overlays t
  "Voicify overlays")

;;; Needed for  outline support:
(defadvice remove-overlays (around emacspeak pre act comp)
  "Clean up properties mirrored from overlays."
  (let ((ems--voiceify-overlays  nil)
        (beg (or (ad-get-arg 0) (point-min)))
        (end (or (ad-get-arg 1) (point-max)))
        (name (ad-get-arg 2)))
    (when (zerop beg) (setq beg (point-min)))
    (with-silent-modifications          ; ignores value for now 
      (put-text-property beg end name nil))
    ad-do-it))

(defadvice delete-overlay (before voice-setup  pre act comp)
  "Augment voice lock."
  (when ems--voiceify-overlays
    (let* ((o (ad-get-arg 0))
           (buffer (overlay-buffer o))
           (start (overlay-start o))
           (end (overlay-end o))
           (voice (dtk-get-voice-for-face (overlay-get o 'face)))
           (invisible (overlay-get o 'invisible)))
      (when  (and  start end voice buffer)
        (with-current-buffer buffer
          (save-restriction
            (widen)
            (ems--remove-personality start end voice buffer))))
      (when  (and start end invisible)
        (with-silent-modifications
          (put-text-property start end 'invisible nil))))))

(defadvice overlay-put (after voice-setup pre act comp)
  "Augment voice lock."
  (when (and (overlay-buffer (ad-get-arg 0)) ems--voiceify-overlays)
    (let* ((overlay (ad-get-arg 0))
           (prop (ad-get-arg 1))
           (value (ad-get-arg 2))
           (start (overlay-start overlay))
           (end (overlay-end overlay))
           (voice nil))
      (cond
       ((and
         (or
          (memq prop '(font-lock-face face))
             (and (eq prop 'category) (get value 'face)))
         (integerp start) (integerp end))
        (when (eq prop 'category) (setq value (get value 'face)))
        (setq voice (dtk-get-voice-for-face value))
        (when voice
            (ems--add-personality
             start end voice (overlay-buffer overlay))))
       ((eq prop 'invisible)
        (with-current-buffer (overlay-buffer overlay)
          (with-silent-modifications
            (put-text-property start end 'invisible (or value nil)))))))))

(defadvice move-overlay (before voice-setup pre act comp)
  "Used by emacspeak to augment voice lock."
  (when ems--voiceify-overlays
    (let*
        ((overlay (ad-get-arg 0))
         (beg (ad-get-arg 1))
         (end (ad-get-arg 2))
         (object (ad-get-arg 3))
         (buffer (overlay-buffer overlay))
         (voice (dtk-get-voice-for-face (overlay-get overlay 'face)))
         (invisible (overlay-get overlay 'invisible)))
      (unless object (setq object (or buffer (current-buffer))))
      (when
          (and voice
               (integerp (overlay-start overlay))
               (integerp (overlay-end overlay)))
        (ems--remove-personality
         (overlay-start overlay) (overlay-end overlay) voice buffer)
        (ems--add-personality beg end voice object))
      (when invisible
        (with-current-buffer buffer
          (with-silent-modifications
            (put-text-property
             (overlay-start overlay) (overlay-end overlay) 'invisible nil)))
        (with-current-buffer object
          (with-silent-modifications
            (put-text-property beg end 'invisible invisible)))))))

;;}}}
;;{{{ advice cursor movement commands to speak

(cl-loop
 for f in
 '(next-line previous-line)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak line. Speak  (visual) line if
`visual-line-mode' is  on, and 
indicate  point  by an aural highlight, and  moving to 
beginning or end of a physical line produces an  auditory icon."
     (when (ems-interactive-p)
       (cond
        ((or line-move-visual visual-line-mode) (emacspeak-speak-visual-line))
        (t (emacspeak-speak-line)))))))

(defadvice kill-visual-line (before emacspeak pre act comp)
  "Speak line we're  to kill."
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
   lisp-indent-line goto-line goto-line-relative)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak line."
     (when (ems-interactive-p)
       (emacspeak-speak-line)))))

(cl-loop
 for f in
 '(forward-button backward-button)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Speak button with messages Silenced."
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
When on a close delimiter, speak matching delimiter after a small delay. "
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
     "Speak  word."
     (when (ems-interactive-p)
       (skip-syntax-forward " ")
       (emacspeak-speak-word)))))

(cl-loop
 for f in
 '(backward-word left-word)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak word."
     (when (ems-interactive-p) (emacspeak-speak-word)))))

(cl-loop
 for f in
 '(next-buffer previous-buffer bury-buffer)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
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
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'fill-object)
       (emacspeak-speak-current-column)))))

(cl-loop
 for f in
 '(backward-sentence forward-sentence)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak sentence."
     (when (ems-interactive-p) (emacspeak-speak-sentence)))))

(cl-loop
 for f in
 '(forward-sexp backward-sexp
                beginning-of-defun end-of-defun)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Speak sexp."
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
     "Speak paragraph."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'paragraph)
       (emacspeak-speak-paragraph)))))

;;; list navigation:

(cl-loop
 for f in
 '(
   forward-list backward-list
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
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'scroll)
       (emacspeak-speak-page)))))

(cl-loop
 for f in
 '(
   scroll-up scroll-down
   scroll-up-command scroll-down-command)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak next screenful."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'scroll)
       (dtk-speak (emacspeak-get-window-contents))))))

;;}}}
;;{{{ Advise modify case commands to speak

(defadvice upcase-word (around emacspeak pre act comp)
  "Provide a tone, then Speak the word at point. "
  (cond
   ((ems-interactive-p)
    (dtk-tone-upcase)
    (cond
     ((and (numberp current-prefix-arg)
           (< current-prefix-arg 0))
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
  "Provide a tone and Speak  word at point. "
  (cond
   ((ems-interactive-p)
    (dtk-tone-downcase)
    (cond
     ((and (numberp current-prefix-arg)
           (< current-prefix-arg 0))
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
  "Provide a tone  and Speak  word at point. "
  (cond
   ((ems-interactive-p)
    (dtk-tone-upcase)
    (cond
     ((and (numberp current-prefix-arg)
           (< current-prefix-arg 0))
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

(defadvice insert-char (after emacspeak pre act comp)
     "Speak char."
     (when (ems-interactive-p) (emacspeak-speak-char-name (ad-get-arg 0))))

;;}}}
;;{{{ Advice deletion commands:

(cl-loop
 for f in
 '(backward-delete-char backward-delete-char-untabify delete-backward-char)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Speak deleted character."
     (cond
      ((ems-interactive-p)
       (dtk-tone-deletion)
       (emacspeak-speak-this-char (preceding-char))
       ad-do-it)
      (t ad-do-it))
     ad-return-value)))

(cl-loop
 for f in
 '(delete-forward-char delete-char) do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Speak deleted character."
     (cond
      ((ems-interactive-p)
       (dtk-tone-deletion)
       (emacspeak-speak-char t)
       ad-do-it)
      (t ad-do-it))
     ad-return-value)))

(defadvice kill-word (before emacspeak pre act comp)
  "Speak word beingkilled."
  (when (ems-interactive-p)
    (save-excursion
      (skip-syntax-forward " ")
        (dtk-tone-deletion)
        (emacspeak-speak-word 1))))

(defadvice backward-kill-word (before emacspeak pre act comp)
  "Speak word beingkilled."
  (when (ems-interactive-p)
      (save-excursion
        (let ((start (point)))
          (forward-word -1)
          (dtk-tone-deletion)
          (emacspeak-speak-region (point) start)))))

(cl-loop
 for f in 
 '(kill-line kill-whole-line)
 do
 (eval
  `(defadvice ,f (before emacspeak pre act comp)
     "Speak line being killed. "
     (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)
      (dtk-tone-deletion)
      (emacspeak-speak-line 1)))))


(defadvice kill-sexp (before emacspeak pre act comp)
  "Speak the killed  sexp."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)
      (dtk-tone-deletion)
      (emacspeak-speak-sexp 1)))

(defadvice kill-sentence (before emacspeak pre act comp)
  "Speak the kill."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)
      (dtk-tone-deletion)
      (emacspeak-speak-line 1)))

(defadvice delete-blank-lines (before emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (let (thisblank singleblank)
        (save-excursion
          (forward-line 0)
          (setq thisblank (looking-at "[ \t]*$"))
          ;; Set singleblank if there is just one blank line here.
          (setq singleblank
                (and thisblank
                     (not (looking-at "[ \t]*\n[ \t]*$"))
                     (or (bobp)
                         (progn (forward-line -1)
                                (not (looking-at "[ \t]*$")))))))
      (cond
       ((and thisblank singleblank)
        (message "Deleting current blank line"))
       (thisblank (message "Deleting surrounding blank lines"))
       (t (message "Deleting possible subsequent blank lines"))))))

;;}}}
;;{{{ advice tabify:

(defadvice untabify (after emacspeak-fix-nbspc pre act comp)
  "Fix NBSP chars."
  (let ((start (ad-get-arg 0))
          (end (ad-get-arg 1)))
      (save-excursion
        (save-restriction
          (narrow-to-region start end)
          (goto-char start)
          (while (re-search-forward (format "[%c]+" 160) end 'no-error)
            (replace-match " "))))))

;;}}}
;;{{{ Advice PComplete

(defadvice pcomplete-list (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'help)
    (emacspeak-auditory-icon 'complete)))

(defadvice pcomplete-show-completions (around emacspeak pre act comp)
  (ems-with-messages-silenced ad-do-it))

(defadvice pcomplete (around emacspeak pre act comp)
  "Speak completion."
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
     "Speak completion."
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

(voice-setup-set-voice-for-face 'minibuffer-prompt 'voice-bolden)

(defadvice quoted-insert (after emacspeak pre act comp)
  "Speak inserted  character."
  (when (ems-interactive-p)
    (emacspeak-speak-this-char (preceding-char))))

(defadvice read-event (before emacspeak pre act comp)
  "Speak prompt."
  (when  (ad-get-arg 0)
    (message (ad-get-arg 0))))

(defadvice read-multiple-choice (before emacspeak pre act comp)
  "speak."
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
         (dtk-speak (minibuffer-contents)))))))

(defvar emacspeak-last-message nil
  "Last output from `message'.")

(defvar emacspeak-lazy-message-time (current-time)
  "Time message was spoken")

(defcustom emacspeak-speak-messages-filter
  '("psession")
  "List of strings used to filter spoken messages."
  :type '(repeat :tag "Filtered Strings"
                 (string :tag "String" ))
  :set #'(lambda (sym val)
           (set-default sym val )
           (setq ems--message-filter-pattern (apply #'regexp-quote val)))
  :group 'emacspeak-speak)


(defadvice momentary-string-display (around emacspeak pre act comp)
  "Speak."
  (ems-with-messages-silenced
      (let ((msg (ad-get-arg 0))
            (exit (ad-get-arg 2)))
        (dtk-speak
         (format
          "%s Press %s to exit"
          msg
          (if exit
              (format "%c" exit)
            "space")))
        ad-do-it)))

(defadvice progress-reporter-do-update (around emacspeak pre act comp)
  "Silence progress reporters."
  (ems-with-messages-silenced ad-do-it)
  (when  ad-return-value (emacspeak-auditory-icon 'progress)))

(defadvice progress-reporter-done (after emacspeak pre act comp)
  "speak."
  (emacspeak-auditory-icon 'time))

(cl-loop
 for f in
 '( minibuffer-message set-minibuffer-message
    message display-message-or-buffer) do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Speak message."
     (cl-declare (special emacspeak-last-message inhibit-message
                          ems--message-filter-pattern
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
            m                           ; our message
            emacspeak-speak-messages    ; speaking messages
            (not (string-match ems--message-filter-pattern m))
            (< 0.1
               (float-time
                (time-subtract (current-time) emacspeak-lazy-message-time))))
         (setq emacspeak-lazy-message-time (current-time)
               emacspeak-last-message  m)
         ;;; so we really need to speak it
         (tts-with-punctuations 'all
           (dtk-notify-speak m 'dont-log)))
       ad-return-value))))

(defadvice display-message-or-buffer (after emacspeak pre act comp)
  "speak."
  (let ((buffer-name (ad-get-arg 1)))
    (when (bufferp ad-return-value)
      (dtk-speak (format "Displayed message in buffer  %s" buffer-name)))))

(with-eval-after-load "eldoc"
  (global-eldoc-mode -1)
  (setq eldoc-idle-delay 3))

(defvar emacspeak-eldoc-speak-explicitly
  (not (tts-multistream-p dtk-program))
  "Set to T if not using a separate TTS notification stream.")

(voice-setup-set-voice-for-face 'eldoc-highlight-function-argument 'voice-bolden)

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

(defadvice ange-ftp-process-handle-hash (around emacspeak pre act comp)
  "Jibber intelligently."
  (cl-declare (special ange-ftp-last-percent))
  (ems-with-messages-silenced
   ad-do-it
     (emacspeak-auditory-icon 'progress)
     (dtk-speak (format " %s percent" ange-ftp-last-percent))))



(cl-declaim (special command-error-function))
(setq command-error-function 'emacspeak-error-handler)

(defun emacspeak-error-handler (data context _calling-function)
  "Emacspeak custom error handler."
  (emacspeak-auditory-icon 'warn-user)
  (message "%s %s"
           (or context " ")
           (error-message-string data)))

;;; Silence messages from async handlers:
(defadvice timer-event-handler (around emacspeak pre act comp)
  "Silence messages from by timer events."
  (ems-with-messages-silenced ad-do-it))

;;}}}
;;{{{ Advice completion-at-point:

(defadvice completion-at-point (around emacspeak pre act comp)
  "Speak completion."
  (let ((orig (save-excursion (skip-syntax-backward "^ >_") (point))))
    ad-do-it
    (when (ems-interactive-p)
      (dtk-speak (buffer-substring orig (point)))
      (emacspeak-auditory-icon 'complete))
    ad-return-value))

;;}}}
;;{{{ advice various input functions to speak:

(defadvice read-passwd (before emacspeak pre act comp)
  "speak."
  (emacspeak-prompt "pwd"))

(defvar emacspeak-read-char-prompt-cache nil
  "Cache prompt from read-char etc.")

(cl-loop
 for f in
 '(read-key read-key-sequence read-key-sequence-vector
            read-char read-char-exclusive)
 do
 (eval
  `(defadvice ,f (before emacspeak pre act comp)
     "Speak prompt"
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
     "Speak completion."
     (when (ems-interactive-p)
       (tts-with-punctuations 'all (dtk-speak dabbrev--last-expansion))))))

(voice-setup-add-map
 '(
   (completions-annotations voice-annotate)
   (completions-common-part voice-monotone-extra)
   (completions-first-difference voice-bolden)))

(cl-loop
 for f in
 '(
   minibuffer-complete-word minibuffer-complete
   crm-complete-word crm-complete crm-complete-and-exit
   crm-minibuffer-complete crm-minibuffer-complete-and-exit)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Speak completion."
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
     "Speak completion."
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
  "Speak."
  (emacspeak-auditory-icon 'select-object)
  (dtk-speak (emacspeak-get-current-completion)))

(defadvice next-completion (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (tts-with-punctuations 'all
                           (dtk-speak (emacspeak-get-current-completion)))))

(defadvice previous-completion (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (tts-with-punctuations 'all
                           (dtk-speak (emacspeak-get-current-completion)))))

(defadvice choose-completion (before emacspeak pre act comp)
  "speak."
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
  "Icon"
  (when (ems-interactive-p) (emacspeak-auditory-icon 'open-object)))

(defadvice tmm-shortcut (after emacspeak pre act comp)
  "Icon"
  (emacspeak-auditory-icon 'button))

;;}}}
;;{{{ Advice centering and filling commands:

(defadvice center-line (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'center)
    (message "Centered current line")))

(defadvice center-region (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'center)
    (message "Centered current region containing %s lines"
             (count-lines (region-beginning) (region-end)))))

(defadvice center-paragraph (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'center)
    (message "Centered current paragraph")))

(cl-loop
 for f in
 '(fill-paragraph lisp-fill-paragraph)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'fill-object)
       (message "Filled current paragraph")))))

(defadvice fill-region (after emacspeak pre act comp)
  "speak."
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
   (log-edit-unknown-header voice-monotone-extra)))

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
  "speak."
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
  "speak."
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
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)))

(defadvice vc-finish-logentry (after emacspeak pre act comp)
  "speak."
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
     "speak."
     (when (ems-interactive-p)
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'select-object)))))

(defadvice vc-dir-mark-file (after emacspeak-pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'mark-object)))

(defadvice vc-dir-mark (after emacspeak-pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'mark-object)))

(defadvice vc-dir (after emacspeak pre act comp)
  "Produce auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice vc-dir-hide-up-to-date (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (emacspeak-speak-line)))

(defadvice vc-dir-kill-line (after emacspeak pre act comp)
  "speak."
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
  "speak."
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
     "speak."
     (when (ems-interactive-p)
       (message "Displayed key bindings in help window")
       (emacspeak-auditory-icon 'help)))))

(defadvice line-number-mode (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'button)
    (emacspeak-speak-mode-line)))

(defadvice column-number-mode (after emacspeak pre act comp)
  "speak."
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
  "speak."
  (when (ems-interactive-p)
    (cond
     ((use-region-p)
      (emacspeak-speak-region (region-beginning) (region-end)))
     (t (emacspeak-speak-line)))
    (emacspeak-auditory-icon 'task-done)))

(defadvice comment-region (after emacspeak pre act comp)
  "Speak."
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
     "speak."
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
               next-window-any-frame previous-window-any-frame
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
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice rename-buffer (after emacspeak pre act comp)
  "Speak."
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
(cl-loop
 for f in
 '(describe-key describe-keymap)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
    "Speak the help."
    (when (ems-interactive-p)
      (emacspeak-auditory-icon 'help)
      (unless ad-return-value
        (emacspeak-speak-help))))))




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
  "speak."
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
        (t (dtk-tone 225 75 'force)))))
     ad-do-it
     ad-return-value)))

(cl-loop
 for f in
 '(eval-last-sexp eval-expression)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Also speaks the result of evaluation."
     (when (ems-interactive-p)
       (let ((dtk-chunk-separator-syntax " .<>()$\"'"))
         (tts-with-punctuations 'all
           (dtk-speak (format "%s" ad-return-value))))))))

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
  "Speak."
  (cl-declare (special emacspeak-use-auditory-icons))
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
(declare-function which-function "which-func" nil)


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
  "Speak."
  (when (ems-interactive-p)
    (message "Deleted all other windows")
    (emacspeak-auditory-icon 'window-resize)
    (emacspeak-speak-mode-line)))

(defadvice split-window-vertically (after emacspeak pre act comp)
  "Speak."
  (when (ems-interactive-p)
    (message "Split window vertically, current window has %s lines "
             (window-height))
    (emacspeak-speak-mode-line)))

(defadvice delete-window (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice shrink-window (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (message "Current window has %s lines and %s columns"
             (window-height) (window-width))))

(defadvice shrink-window-if-larger-than-buffer (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (message "Current window has %s lines and %s columns"
             (window-height) (window-width))))

(defadvice balance-windows (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (message "Current window has %s lines and %s columns"
             (window-height) (window-width))))

(defadvice split-window-horizontally (after emacspeak pre act comp)
  "Speak."
  (when (ems-interactive-p)
    (message "Split window horizontally current window has %s columns "
             (window-width))
    (emacspeak-speak-mode-line)))

(defadvice transpose-chars (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-speak-char t)))

(defadvice transpose-lines (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-speak-line)))

(defadvice transpose-words (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-speak-word)))

(defadvice transpose-sexps (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-speak-sexp)))

(defadvice open-line (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (let ((count (ad-get-arg 0)))
      (emacspeak-auditory-icon 'open-object)
      (message "Opened %s blank line%s"
               (if (= count 1) "a" count)
               (if (= count 1) "" "s")))))

(defadvice abort-recursive-edit (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (message "Aborting recursive edit")))
(cl-loop
 for f in
 '(undo undo-redo undo-only)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
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
  "speak."
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
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{ view echo area

(defadvice view-echo-area-messages (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Displayed messages in other window.")))

;;}}}
;;{{{ selective display

(defadvice set-selective-display (after emacspeak pre act comp)
  "Speak."
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
(define-key isearch-mode-map "\M-y" 'isearch-yank-pop)
;;; face navigators 
(define-key isearch-mode-map (ems-kbd "C-b") 'emacspeak-speak-face-backward )
(define-key isearch-mode-map (ems-kbd "C-f") 'emacspeak-speak-face-forward )
;;; ISearch setup/teardown

;;; Produce auditory icon
(defun emacspeak-isearch-setup ()
  "Setup emacspeak isearch."
  (emacspeak-auditory-icon 'open-object)
  (setq emacspeak-speak-messages nil)
  (dtk-speak (isearch-message-prefix)))

(defun emacspeak-isearch-teardown ()
  "Teardown emacspeak isearch."
  (setq emacspeak-speak-messages t)
  (emacspeak-auditory-icon 'close-object))

(add-hook 'isearch-mode-hook 'emacspeak-isearch-setup)
(add-hook 'isearch-mode-end-hook 'emacspeak-isearch-teardown)
(add-hook 'isearch-mode-end-hook-quit 'emacspeak-isearch-teardown)

;;; Advice isearch-search to speak
(defadvice isearch-search (after emacspeak pre act comp)
  "Speak the hit."
  (cond
   ((null isearch-success) (emacspeak-auditory-icon 'search-miss))
   (t
    (emacspeak-auditory-icon 'search-hit)
    (when (sit-for 0.1)
      (save-excursion
        (ems-set-personality-temporarily
         (point) isearch-other-end voice-bolden
         (dtk-speak
          (buffer-substring (line-beginning-position) (line-end-position)))))))))

(defadvice isearch-delete-char (after emacspeak pre act comp)
  "Speak search hit. "
  (dtk-speak (propertize isearch-string 'personality  voice-bolden))
  (when (sit-for 0.1)
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
 '(isearch-yank-word isearch-yank-kill isearch-yank-line) do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (dtk-speak (propertize  isearch-string 'personality voice-bolden))
       (emacspeak-auditory-icon 'yank-object)))))

(cl-loop
 for f in
 '(
   isearch-ring-advance isearch-ring-retreat
   isearch-ring-advance-edit isearch-ring-retreat-edit) do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (dtk-speak (propertize  isearch-string 'personality voice-bolden))
       (emacspeak-auditory-icon 'item)))))

;;; Note the advice on the next two toggle commands
;;; checks the variable being toggled.
;;; When our advice is called, emacs has not yet reflected
;;; the newly toggled state.

(defadvice isearch-toggle-case-fold (after emacspeak pre act comp)
  "Speak"
  (emacspeak-auditory-icon (if isearch-case-fold-search 'off 'on))
  (dtk-speak
   (format " Case is %s significant in search"
           (if isearch-case-fold-search " not" " "))))

(defadvice isearch-toggle-regexp (after emacspeak pre act comp)
  "Speak"
  (emacspeak-auditory-icon (if isearch-regexp 'on 'off))
  (dtk-speak (if isearch-regexp "Regexp search" "text search")))

(defadvice isearch-occur (after emacspeak pre act comp)
  "speak."
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
  "speak."
  (when (ems-interactive-p)
    (message "Copied window configuration to register %c" (ad-get-arg 0))))
(cl-loop
 for f in
 '(frameset-to-register frame-configuration-to-register)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (message "Copied frame  configuration to register %c" (ad-get-arg 0))))))

;;}}}
;;{{{ set up clause boundaries for specific modes:
;;;###autoload

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
     "Speak."
     (when (ems-interactive-p)
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'large-movement)))))
(defadvice occur-mode-display-occurrence (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Displayed occurrence in other window")))

;;}}}
;;{{{ abbrev mode advice

(defadvice abbrev-edit-save-buffer (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'save-object)
    (dtk-speak "Saved Abbrevs")))

(defadvice edit-abbrevs-redefine (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (dtk-speak "Redefined abbrevs")))

(defadvice list-abbrevs (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Displayed abbrevs in other window.")))

(defadvice edit-abbrevs (after emacspeak pre act comp)
  "speak."
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
  "speak."
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
              (replace-match " cap \\& " t))
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
	 (desc
          (if (zerop (length keys))
              "is not on any key"
            (mapconcat 'key-description keys ", "))))
    (concat
     (format "%s  " cmd)
     (ems-canonicalize-key-description desc))))

(defadvice where-is (after emacspeak pre act comp)
  "Speak"
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
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon (if transient-mark-mode 'on 'off))
    (message "Turned %s transient mark." (if transient-mark-mode "on" "off"))))

;;}}}
;;{{{ provide auditory icon when window config changes

;;}}}
;;{{{ mail aliases

(defadvice expand-mail-aliases (after emacspeak pre act comp)
  "speak."
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
             (let ((inhibit-read-only t))
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
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-buffer)
    (emacspeak-auditory-icon 'open-object)))

(defadvice finder-mode (after emacspeak pre act comp)
  "speak"
  (when (and (boundp 'finder-known-keywords)
             (not (eq 'emacspeak (caar finder-known-keywords))))
    (push (cons 'emacspeak "Audio Desktop")
          finder-known-keywords))
  (emacspeak-auditory-icon 'open-object)
  (emacspeak-speak-mode-line))

(defadvice finder-exit (after emacspeak pre act comp)
  "speak."
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
               (emacspeak-eww-autospeak)
               ad-do-it)
              (t ad-do-it))
             ad-return-value)))

;;}}}
;;{{{ Cue input method changes

(defadvice toggle-input-method (after emacspeak pre act comp)
  "speak."
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
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'open-object)
       (with-current-buffer (window-buffer (selected-window))
         (emacspeak-speak-buffer))))))

(defadvice exit-splash-screen (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{ copyright commands:

(cl-loop
 for f in
 '(copyright copyright-update)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'task-done)
       (emacspeak-speak-line)))))

(defadvice copyright-update-directory (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)))

;;}}}
;;{{{ Asking Questions:

(defadvice yes-or-no-p (around emacspeak pre act comp)
  "Play auditory icon."
  (cond
   ((ems-interactive-p)
    (emacspeak-auditory-icon 'ask-question)
    ad-do-it
    (emacspeak-auditory-icon (if ad-return-value 'yes-answer 'no-answer )))
   (t ad-do-it))
  ad-return-value)



(defadvice y-or-n-p (around emacspeak pre act comp)
  "Play auditory icon."
  (cond
   ((ems-interactive-p)
    (emacspeak-auditory-icon 'ask-short-question)
    ad-do-it
    (emacspeak-auditory-icon (if ad-return-value 'y-answer 'n-answer )))
   (t ad-do-it)))

(defadvice ask-user-about-lock (around emacspeak pre act comp)
  "Play auditory icon."
  (cond
   ((ems-interactive-p)
    (emacspeak-auditory-icon 'ask-short-question)
    ad-do-it
    (emacspeak-auditory-icon (if ad-return-value 'y-answer 'n-answer )))
   (t ad-do-it)))

(defadvice ask-user-about-lock-help (after emacspeak pre act comp)
  "Play auditory icon."
  (emacspeak-auditory-icon 'help))

;;}}}
;;{{{ Advice process-menu

(defadvice process-menu-delete-process (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-speak-line)))

(defadvice list-processes (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Displayed process list in other window.")))

(defadvice timer-list (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{list-timers:

(defadvice list-timers (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

;;}}}
;;{{{find-library:

(defadvice find-library (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{log-edit-done

(defadvice log-edit-done (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'close-object)))


;;}}}
;;{{{ advice find-func etc.

(cl-loop
 for f in
 '(
   find-function find-function-at-point find-variable
   find-variable-at-point find-function-on-key)
 do
 (eval
  `(defadvice ,f  (after emacspeak pre act comp)
     "Speak current line"
     (when  (ems-interactive-p)
       (emacspeak-auditory-icon 'open-object)
       (emacspeak-speak-line)))))

;;}}}
;;{{{Advice Semantic:

(defadvice semantic-complete-symbol (around emacspeak pre act comp)
  "speak."
  (let ((prior (point))
        (dtk-stop-immediately t))
    (emacspeak-kill-buffer-carefully "*Completions*")
    ad-do-it
    (if (> (point) prior)
        (tts-with-punctuations 'all
          (emacspeak-speak-rest-of-buffer))
      (emacspeak-speak-completions-if-available))
    ad-return-value))

;;}}}
(provide 'emacspeak-cedet)

;;{{{ advice Imenu

(defadvice imenu (after emacspeak pre act comp)
  "speak"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

;;}}}
;;{{{Advice property search

(cl-loop
 for f in 
 '(text-property-search-backward text-property-search-forward)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak range."
     (when (ems-interactive-p)
       (unless ad-return-value
         (emacspeak-auditory-icon 'warn-user)
         (emacspeak-speak-line))
       (when-let ((m ad-return-value))
         (emacspeak-speak-region
          (prop-match-beginning m) (prop-match-end m))
         (emacspeak-auditory-icon 'select-object))))))


;;}}}
;;{{{ielm: header-line

(defadvice ielm (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (cl-declare (special ielm-working-buffer))
     (setq
      header-line-format
      '((:eval
         (concat
          (propertize "Interactive Elisp" 'personality voice-annotate)
          (format "On %s" (buffer-name ielm-working-buffer) )))))
     (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-header-line)))

;;}}}
;;{{{Help Navigation:

(cl-loop
 for f in 
 '(help-goto-next-page help-goto-previous-page)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'scroll)
       (emacspeak-speak-line)))))


;;}}}
(provide 'emacspeak-advice)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
