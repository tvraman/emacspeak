;;; emacspeak-advice.el --- Advice all core Emacs functionality to speak
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description: Core advice forms that make emacspeak work
;;; Keywords: Emacspeak, Speech, Advice, Spoken output
;;{{{ LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2008-08-18 17:52:34 -0700 (Mon, 18 Aug 2008) $ |
;;; $Revision: 4550 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{ Copyright:

;;;Copyright (C) 1995 -- 2011, T. V. Raman
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
;;; This also means that this and subsequent versions of
;;;Emacspeak should not be run on versions of Emacs older than
;;;Emacs 21,
;;; And preferably only run on Emacs 22.
;;; This version of Emacspeak is only tested on Emacs 22.

;;
;;; Code:

;;}}}
;;{{{ Required modules

(require 'advice)
(require 'cl)
(declaim (optimize (safety 0) (speed 3)))
(require 'voice-setup)
(require 'dtk-speak)
(require 'emacspeak-pronounce)
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
(require 'ansi-color)
;;}}}
;;{{{ Forward Declarations:
(defvar emacspeak-prefix)

;;}}}
;;{{{ Advice ding

(defadvice ding (before emacspeak pre act comp)
  "Produce auditory icon."
  (emacspeak-auditory-icon 'warn-user))

;;}}}
;;{{{ advice cursor movement commands to speak

(loop
 for f in
 '(next-line previous-line goto-line
             delete-indentation back-to-indentation lisp-indent-line)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak line that you just moved to."
     (when (ems-interactive-p )
       (emacspeak-speak-line )))))

(loop
 for f in
 '(forward-button backward-button)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak the button."
     (when (ems-interactive-p )
       (condition-case nil
           (let* ((button (button-at (point)))
                  (start (button-start button))
                  (end (button-end button)))
             (dtk-speak (buffer-substring start end)))
         (error nil))
       (emacspeak-auditory-icon 'large-movement)))))

(loop
 for f in
 '(left-char right-char
             backward-char forward-char)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak char under point."
     (when (ems-interactive-p )
       (and dtk-stop-immediately (dtk-stop))
       (emacspeak-speak-char t )))))

(loop
 for f in
 '(forward-word right-word)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak the word you just moved to."
     (when (ems-interactive-p )
       (skip-syntax-forward " ")
       (emacspeak-speak-word )))))
(loop
 for f in
 '(backward-word left-word)
 do
 (eval
  `(defadvice,f (after emacspeak pre act comp)
     "Speak the word you just moved to."
     (when (ems-interactive-p ) (emacspeak-speak-word )))))

(loop
 for f in
 '(next-buffer previous-buffer bury-buffer)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p )
       (emacspeak-auditory-icon 'select-object)
       (emacspeak-speak-mode-line)))))
(loop
 for f in
 '(beginning-of-buffer end-of-buffer
                       beginning-of-defun end-of-defun)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak the line."
     (when (ems-interactive-p )
       (emacspeak-auditory-icon 'large-movement)
       (emacspeak-speak-line )))))
(loop
 for f in
 '(tab-to-tab-stop indent-for-tab-command reindent-then-newline-and-indent
                   indent-sexp indent-pp-sexp
                   indent-region indent-relative)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p )
       (emacspeak-auditory-icon 'fill-object)
       (emacspeak-speak-current-column)))))

(loop
 for f in
 '(backward-sentence forward-sentence)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak sentence after moving."
     (when (ems-interactive-p ) (emacspeak-speak-sentence )))))

(loop
 for f in
 '(forward-sexp backward-sexp)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Speak sexp after moving."
     (if (ems-interactive-p)
         (let ((start (point))
               (end (line-end-position)))
           ad-do-it
           (emacspeak-auditory-icon 'large-movement)
           (cond
            ((>= end (point))
             (emacspeak-speak-region start (point)))
            (t (emacspeak-speak-line))))
       ad-do-it)
     ad-return-value)))

(loop
 for f in
 '(forward-paragraph backward-paragraph)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak the paragraph."
     (when(ems-interactive-p ) (emacspeak-speak-paragraph)))))

;;; list navigation:

(loop
 for f in
 '(forward-list backward-list
                up-list backward-up-list down-list)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Speak the list.
If you moved more than a line,
 only speak the target line."
     (if (ems-interactive-p )
         (let ((start (point)))
           ad-do-it
           (cond
            ((ems-same-line-p start (point))
             (emacspeak-speak-region start (point )))
            (t (emacspeak-speak-line))))
       ad-do-it)
     ad-return-value)))
(loop
 for f in
 '(forward-page backward-page)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p )
       (emacspeak-auditory-icon 'scroll)
       (emacspeak-speak-page )))))
(loop
 for f in
 '(scroll-up scroll-down
             scroll-up-command scroll-down-command)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak the next screenful."
     (when (ems-interactive-p )
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
   ((ems-interactive-p )
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
is done. If `downcase-word' is called with a negative
argument, then point does not move. In this case, we speak
the words that were down cased."

  (cond
   ((ems-interactive-p )
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
is done. If `capitalize-word' is called with a negative
argument, then point does not move. In this case, we speak
the words that were capitalized."
  (cond
   ((ems-interactive-p )
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
;;{{{ Advice insert-char:

(loop
 for f in
 '(ucs-insert insert-char)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak char we inserted."
     (when (ems-interactive-p) (emacspeak-speak-char-name (ad-get-arg 0))))))

;;}}}
;;{{{ Advice deletion commands:

(loop
 for f in
 '(backward-delete-char backward-delete-char-untabify delete-backward-char )
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Speak character you're deleting."
     (cond
      ((ems-interactive-p )
       (dtk-tone 500 30 'force)
       (emacspeak-speak-this-char (preceding-char ))
       ad-do-it)
      (t ad-do-it))
     ad-return-value)))
(loop
 for f in
 '(delete-forward-char delete-char)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Speak character you're deleting."
     (cond
      ((ems-interactive-p )
       (dtk-tone 500 30 'force)
       (emacspeak-speak-char t)
       ad-do-it)
      (t ad-do-it))
     ad-return-value)))

(defadvice kill-word (before emacspeak pre act comp)
  "Speak word before killing it."
  (when (ems-interactive-p )
    (save-excursion
      (skip-syntax-forward " ")
      (when dtk-stop-immediately (dtk-stop))
      (let ((dtk-stop-immediately nil))
        (dtk-tone 500 30)
        (emacspeak-speak-word 1 )))))

(defadvice backward-kill-word (before emacspeak pre act comp)
  "Speak word before killing it."
  (when (ems-interactive-p )
    (when dtk-stop-immediately (dtk-stop))
    (let ((start (point ))
          (dtk-stop-immediately nil))
      (save-excursion
        (forward-word -1)
        (dtk-tone 500 30)
        (emacspeak-speak-region (point) start )))))

;;; Large deletions also produce auditory icons if possible

(defadvice kill-line(before emacspeak pre act comp)
  "Speak line before killing it. "
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'delete-object)
    (when dtk-stop-immediately (dtk-stop))
    (let ((dtk-stop-immediately nil))
      (dtk-tone 500 30)
      (emacspeak-speak-line 1))))

(defadvice kill-sexp (before emacspeak pre act comp)
  "Speak the sexp you killed."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'delete-object)
    (when dtk-stop-immediately (dtk-stop))
    (let ((dtk-stop-immediately nil))
      (dtk-tone 500 30)
      (emacspeak-speak-sexp 1 ))))

(defadvice kill-sentence (before emacspeak pre act comp)
  "Speak the line you killed."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'delete-object)
    (when dtk-stop-immediately (dtk-stop))
    (let ((dtk-stop-immediately nil))
      (dtk-tone 500 30)
      (emacspeak-speak-line 1 ))))

(defadvice delete-blank-lines (before emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
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
       ( thisblank (message "Deleting surrounding blank lines"))
       (t (message "Deleting possible subsequent blank lines"))))))

;;}}}
;;{{{ advice tabify:

;;;###autoload
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
          (while (re-search-forward (format "[%c]+" 160)end 'no-error)
            (replace-match" ")))))))

;;}}}
;;{{{ Advice PComplete

(defadvice pcomplete-list (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'help)
    (emacspeak-auditory-icon 'complete)))

(defadvice pcomplete-show-completions (around emacspeak pre act comp)
  (let ((emacspeak-speak-messages nil))
    ad-do-it))

(defadvice pcomplete (around emacspeak pre act comp)
  "Say what you completed."
  (let ((orig (point)))
    ad-do-it
    (when (ems-interactive-p )
      (emacspeak-speak-region orig (point))
      (emacspeak-auditory-icon 'complete))
    ad-return-value))

;;}}}
;;{{{ advice insertion commands to speak.

;;; Dont advice if we catch this through post-self-insert-hook
(unless (and (boundp 'post-self-insert-hook)
             post-self-insert-hook
             (memq 'emacspeak-post-self-insert-hook post-self-insert-hook))
  (defadvice completion-separator-self-insert-autofilling
      (after emacspeak pre act comp)
    "Speak what was completed."
    (declare (special emacspeak-word-echo))
    (when (and emacspeak-word-echo (ems-interactive-p ))
      (let ((display (get-char-property (1- (point)) 'display)))
        (if (stringp display)
            (dtk-say display)
          (condition-case nil
              (save-excursion
                (skip-syntax-backward " ")
                (backward-char 1)
                (emacspeak-speak-word))
            (error nil ))))))

  (defadvice completion-separator-self-insert-command (after emacspeak act comp)
    "Speak char after inserting it."
    (declare (special emacspeak-character-echo))
    (when (and emacspeak-character-echo (ems-interactive-p ))
      (let ((display (get-char-property (1- (point)) 'display)))
        (if (stringp display)
            (dtk-say display)
          (emacspeak-speak-this-char (preceding-char ))))))
  )

;;}}}
;;{{{ advice minibuffer to speak

(voice-setup-map-face 'minibuffer-prompt 'voice-bolden)

(defadvice quoted-insert (after emacspeak pre act comp)
  "Speak the character that was inserted."
  (when (ems-interactive-p )
    (emacspeak-speak-this-char (preceding-char ))))

(defvar emacspeak-speak-read-events t
  "Set to nil to silence read-event.")

(defadvice read-event (before emacspeak pre act comp)
  "Speak the prompt."
  (when (and emacspeak-speak-read-events (ad-get-arg 0))
    (tts-with-punctuations 'all
                           (dtk-speak (ad-get-arg 0)))))
(loop
 for f in
 '(
   next-history-element previous-history-element
                        next-line-or-history-element previous-line-or-history-element
                        previous-matching-history-element next-matching-history-element)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak the history element just inserted."
     (when (ems-interactive-p )
       (emacspeak-auditory-icon 'select-object)
       (tts-with-punctuations 'all
                              (emacspeak-speak-current-field ))))))

(defvar emacspeak-last-message nil
  "Holds the last output generated by the Emacs 'message function.")

(defvar emacspeak-lazy-message-time 0
  "Records when we last spoke a message.")

(defadvice momentary-string-display (around emacspeak pre act comp)
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

(defcustom emacspeak-advice-progress-reporter t
  "Set to true if progress reporter should produce an auditory
icon."
  :type 'boolean
  :group 'emacspeak-advice)

(defadvice progress-reporter-do-update (around emacspeak pre act comp)
  "Silence progress reporters for now."
  (let ((emacspeak-speak-messages nil))
    (when emacspeak-advice-progress-reporter
      (emacspeak-auditory-icon 'progress))
    ad-do-it))

(defadvice message (around emacspeak pre act comp)
  "Speak the message."
  (declare (special emacspeak-last-message
                    emacspeak-speak-messages emacspeak-lazy-message-time))
  (let ((inhibit-read-only t))
    ad-do-it
    (when (and
           (current-message)
           emacspeak-speak-messages ; speaking messages
           (/= emacspeak-lazy-message-time ;; previous message not recent
               (setq emacspeak-lazy-message-time (nth 1 (current-time)))))
      (setq emacspeak-last-message (ansi-color-apply (current-message)))
      ;; so we really need to speak it
      (tts-with-punctuations 'all
                             (dtk-speak emacspeak-last-message)))
    ad-return-value))

(defadvice eldoc-message (around emacspeak pre act comp)
  "Speech enable ELDoc --- now used by semantic."
 ;;; eldoc flashes message temporarily, we cache and speak.
  (let ((emacspeak-speak-messages nil))
    ad-do-it
    (when eldoc-last-message
      (dtk-speak-and-echo eldoc-last-message))
    ad-return-value))

(defvar emacspeak-ange-ftp-last-percent nil
  "Cache the last percentage that emacspeak spoke.")

(defadvice ange-ftp-process-handle-hash (around emacspeak pre act comp)
  "Jibber intelligently."
  (declare (special emacspeak-ange-ftp-last-percent
                    ange-ftp-last-percent ))
  (let ((emacspeak-speak-messages nil ))
    ad-do-it
    (when (or
           (null emacspeak-ange-ftp-last-percent)
           (>= (abs (- ange-ftp-last-percent emacspeak-ange-ftp-last-percent ))
               5))
      (setq emacspeak-ange-ftp-last-percent ange-ftp-last-percent )
      (emacspeak-auditory-icon 'progress)
      (dtk-speak
       (format " %s percent" ange-ftp-last-percent )))))

;;{{{ advising signal

;;;###autoload
(defcustom emacspeak-speak-errors t
  "Specifies if error messages are cued."
  :type 'boolean
  :group 'emacspeak-speak)

;;;###autoload
(defvar emacspeak-speak-signals t
  "Specifies if signalled messages are cued.")

(defadvice signal (before emacspeak pre act comp)
  "Provide spoken feedback for signals."
  (let ((error-symbol(ad-get-arg 0))
        (data (ad-get-arg 1)))
    (tts-with-punctuations
     'all
     (dtk-speak (error-message-string (cons error-symbol data))))))

(defun emacspeak-error-handler (data context calling-function)
  "Emacspeak custom error handling function."

  (emacspeak-auditory-icon 'warn-user)
  (message "%s %s"
           (error-message-string data)
           (or context " ")))

(declaim (special command-error-function))
(when (boundp 'command-error-function)
  (ad-deactivate 'signal)
  (setq command-error-function 'emacspeak-error-handler))

(unless (boundp 'command-error-function)
 ;;; turn off tool-bar-mode -- since it raises signals during redisplay
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1)))

;;; Silence messages from async handlers:
(defadvice timer-event-handler (around emacspeak pre act comp)
  "Silence messages generated by timer event handlers."
  (declare (special emacspeak-speak-messages))
  (let ((emacspeak-speak-messages nil))
    ad-do-it))

;;}}}

(defadvice y-or-n-p (around emacspeak pre act comp)
  "Use speech when prompting.
Produce an auditory icon if possible."
  (emacspeak-auditory-icon 'ask-short-question )
  (tts-with-punctuations 'all
                         (dtk-speak (format "%s y or n" (ad-get-arg 0 ))))
  ad-do-it
  (cond
   (ad-return-value
    (emacspeak-auditory-icon 'y-answer )
    (dtk-say "y"))
   (t (emacspeak-auditory-icon 'n-answer )
      (dtk-say "n" )))
  ad-return-value )

;;}}}
;;{{{ Advice completion-at-point:
(defadvice completion-at-point (around emacspeak pre act comp)
  "Say what you completed."
  (let ((orig (point)))
    ad-do-it
    (when (ems-interactive-p )
      (emacspeak-speak-region orig (point))
      (emacspeak-auditory-icon 'complete))
    ad-return-value))

;;}}}
;;{{{ advice various input functions to speak:

(loop
 for f in
 '(read-key read-key-sequence read-char read-char-exclusive)
 do
 (eval
  `(defadvice ,f (before emacspeak pre act comp)
     "Speak the prompt"
     (when (ad-get-arg 0)
       (tts-with-punctuations 'all (dtk-speak (ad-get-arg 0)))))))

(defadvice read-char-choice(before emacspeak pre act comp)
  "Speak the prompt"
  (let ((prommmmpt (ad-get-arg 0))
        (chars (ad-get-arg 1)))
    (tts-with-punctuations
     'all
     (dtk-speak
      (format "%s: %s"
              prompt
              (mapconcat
               #'(lambda (c) (format "%c" c))
               chars
               ", "))))))

;;}}}
;;{{{ advice completion functions to speak:
(loop for f in
      '(dabbrev-expand dabbrev-completion)
      do
      (eval
       `(defadvice,f (after emacspeak pre act comp)
          "Say what you completed."
          (when (ems-interactive-p )
            (tts-with-punctuations 'all
                                   (dtk-speak
                                    dabbrev--last-expansion))))))

(voice-setup-add-map
 '(
   (completions-annotations voice-annotate)
   (completions-common-part voice-monotone)
   (completions-first-difference voice-brighten)))
(loop for f in
      '(minibuffer-complete-word minibuffer-complete)
      do
      (eval
       `(defadvice ,f (around emacspeak pre act comp)
          "Say what you completed."
          (cond
           ((ems-interactive-p )
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
      '(lisp-complete-symbol complete-symbol
                             widget-complete)
      do
      (eval
       `(defadvice ,f (around emacspeak pre act comp)
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
(define-key minibuffer-local-completion-map "\C-o" 'switch-to-completions)
(defadvice switch-to-completions(after emacspeak pre act comp)
  "Provide spoken feedback."
  (emacspeak-auditory-icon 'select-object)
  (dtk-speak (emacspeak-get-current-completion)))

(defadvice complete (around emacspeak pre act comp)
  "Say what you completed."
  (let ((emacspeak-speak-messages nil)
        (emacspeak-last-message nil))
    ad-do-it
    (when (ems-interactive-p )
      (dtk-speak
       (format "%s %s"
               (save-excursion (backward-char 1)
                               (sexp-at-point ))
               (or emacspeak-last-message "")))
      ad-return-value)))

(defadvice minibuffer-complete-shell-command (around emacspeak pre act comp)
  "Say what you completed."
  (let ((emacspeak-speak-messages nil)
        (emacspeak-last-message nil))
    ad-do-it
    (when (ems-interactive-p )
      (dtk-speak
       (format "%s %s"
               (save-excursion (backward-char 1)
                               (sexp-at-point ))
               (or emacspeak-last-message "")))
      ad-return-value)))

(defadvice next-completion (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'select-object)
    (tts-with-punctuations 'all
                           (dtk-speak (emacspeak-get-current-completion)))))

(defadvice previous-completion (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'select-object)
    (tts-with-punctuations 'all
                           (dtk-speak
                            (emacspeak-get-current-completion )))))

(defadvice choose-completion (before emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'select-object)))

(defadvice minibuffer-message (around emacspeak pre act comp)
  "Speak the message if appropriate."
  (declare (special emacspeak-last-message
                    emacspeak-speak-messages emacspeak-lazy-message-time))
  (let ((dtk-stop-immediately t ))
    ad-do-it
    (setq emacspeak-last-message ad-return-value )
    (when (and emacspeak-speak-messages ; speaking messages
               ad-return-value ;we really do have a message
               (/= emacspeak-lazy-message-time ;; previous message not recent
                   (setq emacspeak-lazy-message-time
                         (nth 1 (current-time)))))
      ;; so we really need to speak it
      (tts-with-punctuations 'all
                             (dtk-speak ad-return-value)))))

;;}}}
;;{{{ tmm support

(defadvice tmm-goto-completions (after emacspeak pre act comp)
  "announce completions "
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'help)
    (dtk-speak (emacspeak-get-current-completion))))

(defadvice minibuffer-complete-and-exit (before emacspeak pre act comp)
  "Provide an auditory icon."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'button)))

(defadvice tmm-menubar (before emacspeak pre act comp)
  "Provide an auditory icon."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'open-object )))

(defadvice tmm-shortcut (after emacspeak pre act comp)
  "Provide contextual feedback when exitting minibuffer."
  (emacspeak-auditory-icon 'button))

;;}}}
;;{{{ Advice comint:

(defadvice comint-magic-space (around emacspeak pre act comp)
  "Speak word or completion."
  (cond
   ((ems-interactive-p )
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
   ((ems-interactive-p )
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
(declaim (special emacspeak-pronounce-sha-checksum-pattern))

(emacspeak-pronounce-add-dictionary-entry
 'comint-mode
 emacspeak-pronounce-sha-checksum-pattern
 (cons 're-search-forward
       'emacspeak-pronounce-sha-checksum))

(declaim (special emacspeak-pronounce-uuid-pattern))

(emacspeak-pronounce-add-dictionary-entry
 'comint-mode
 emacspeak-pronounce-uuid-pattern
 (cons 're-search-forward
       'emacspeak-pronounce-uuid))
(loop for mode in
      '(conf-space-mode conf-unix-mode conf-mode)
      do
      (emacspeak-pronounce-add-dictionary-entry
       mode
       emacspeak-pronounce-uuid-pattern
       (cons 're-search-forward
             'emacspeak-pronounce-uuid)))

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

(defadvice comint-delchar-or-maybe-eof (around emacspeak pre act comp)
  "Speak character you're deleting."
  (cond
   ((ems-interactive-p )
    (cond
     ((= (point) (point-max))
      (message "Sending EOF to comint process"))
     (t (dtk-tone 500 30 'force)
        (emacspeak-speak-char t)))
    ad-do-it)
   (t ad-do-it))
  ad-return-value)

(defadvice comint-send-input (before emacspeak pre act comp)
  "Aurally highlight input."
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (emacspeak-personality-append start end
                                  'emacspeak-comint-input-personality)))

(defadvice comint-send-eof (before emacspeak pre act comp)
  "Announce what we are doing."
  (when (ems-interactive-p )
    (message "Sending EOF to subprocess")))

(defadvice comint-accumulate (before emacspeak pre act comp)
  "Speak the line we are accumulating."
  (when (ems-interactive-p )
    (save-excursion
      (comint-bol-or-process-mark)
      (emacspeak-auditory-icon 'select-object)
      (emacspeak-speak-line 1))))

(defadvice comint-next-matching-input-from-input (after
                                                  emacspeak
                                                  pre act comp)
  "Speak the line showing where point is."
  (when (ems-interactive-p )
    (let ((emacspeak-show-point t))
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'select-object))))

(defadvice comint-previous-matching-input-from-input (after
                                                      emacspeak
                                                      pre act comp)
  "Speak the line showing where point is."
  (when (ems-interactive-p )
    (let ((emacspeak-show-point t))
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'select-object))))

(defadvice shell-forward-command (after emacspeak pre act
                                        comp)
  "Speak the line showing where point is."
  (when (ems-interactive-p )
    (let ((emacspeak-show-point t))
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'select-object))))

(defadvice shell-backward-command (after emacspeak pre act
                                         comp)
  "Speak the line showing where point is."
  (when (ems-interactive-p )
    (let ((emacspeak-show-point t))
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'select-object))))

(defadvice comint-show-output (after emacspeak pre act
                                     comp)
  "Speak the line showing where point is."
  (when (ems-interactive-p )
    (let ((emacspeak-show-point t)
          )
      (emacspeak-auditory-icon 'large-movement)
      (emacspeak-speak-region (point) (mark)))))

(defadvice comint-show-maximum-output (after emacspeak pre act
                                             comp)
  "Speak the line showing where point is."
  (when (ems-interactive-p )
    (let ((emacspeak-show-point t)
          )
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'select-object))))

(defadvice comint-bol-or-process-mark (after emacspeak pre act
                                             comp)
  "Speak the line showing where point is."
  (when (ems-interactive-p )
    (let ((emacspeak-show-point t)
          )
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'select-object))))

(defadvice comint-copy-old-input (after emacspeak pre act
                                        comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-speak-line)))

(defadvice comint-output-filter (around emacspeak pre act comp)
  "Make comint speak its output."
  (let ((inhibit-read-only t)
        (monitor emacspeak-comint-output-monitor)
        (buffer (process-buffer (ad-get-arg 0)))
        (dtk-stop-immediately nil))
    (with-current-buffer buffer
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
      (when (and
             comint-last-output-start
             emacspeak-comint-autospeak 
             (or monitor (eq (window-buffer) buffer)))
        (emacspeak-speak-region comint-last-output-start (point )))
      ad-return-value)))

(defadvice comint-dynamic-list-completions(around emacspeak pre act comp)
  "Replacing mouse oriented completer with keyboard friendly equivalent"
  (let ((completions (sort (ad-get-arg 0) 'string-lessp)))
    (with-output-to-temp-buffer "*Completions*"
      (display-completion-list completions))
    (with-current-buffer (get-buffer "*Completions*")
      (set (make-local-variable 'comint-displayed-dynamic-completions)
           completions))
    (next-completion 1)
    (dtk-speak
     (buffer-substring (point) (point-max)))))

(defadvice comint-dynamic-complete (around emacspeak pre act comp)
  "Say what you completed."
  (cond
   ((ems-interactive-p )
    (let ((prior (point ))
          (emacspeak-speak-messages nil))
      ad-do-it
      (if (> (point) prior)
          (tts-with-punctuations
           'all
           (emacspeak-auditory-icon 'complete)
           (dtk-speak (buffer-substring prior (point ))))
        (emacspeak-speak-completions-if-available))))
   (t ad-do-it))
  ad-return-value)

(defadvice comint-next-input (after emacspeak pre act comp)
  "Speak the line."
  (when (ems-interactive-p )
    (tts-with-punctuations 'all
                           (emacspeak-speak-line ))
    (emacspeak-auditory-icon 'select-object)))

(defadvice comint-next-matching-input (after emacspeak pre act comp)
  "Speak the line."
  (when (ems-interactive-p )
    (tts-with-punctuations 'all
                           (emacspeak-speak-line ))
    (emacspeak-auditory-icon 'select-object)))

(defadvice comint-previous-input (after emacspeak pre act comp)
  "Speak the line."
  (when (ems-interactive-p )
    (tts-with-punctuations 'all
                           (emacspeak-speak-line ))
    (emacspeak-auditory-icon 'select-object)))

(defadvice comint-previous-matching-input (after emacspeak pre act comp)
  "Speak the line."
  (when (ems-interactive-p )
    (comint-skip-prompt)
    (tts-with-punctuations 'all
                           (emacspeak-speak-line))
    (emacspeak-auditory-icon 'select-object)))

(defadvice comint-send-input (after emacspeak pre act comp)
  "Flush any ongoing speech."
  (when (ems-interactive-p )
    (dtk-stop)))

(defadvice comint-previous-prompt (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'large-movement)
    (if (eolp)
        (emacspeak-speak-line)
      (emacspeak-speak-line 1))))

(defadvice comint-next-prompt (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'large-movement)
    (if (eolp)
        (emacspeak-speak-line)
      (emacspeak-speak-line 1))))

(defadvice comint-dynamic-list-input-ring (around emacspeak pre act comp)
  "List in help buffer the buffer's input history."
  (cond
   ((ems-interactive-p )
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
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'delete-object)
    (message "Nuked output of last command ")))

(defadvice comint-quit-subjob (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (message "Sent quit signal to subjob ")))

(defadvice comint-stop-subjob (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (message "Stopped the subjob")))

(defadvice comint-interrupt-subjob (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (message "Interrupted the subjob")))

(defadvice comint-kill-input (before emacspeak pre act comp)
  "Provide spoken feedback."
  (when (ems-interactive-p )
    (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
      (when (> (point) (marker-position pmark))
        (emacspeak-auditory-icon 'delete-object )
        (emacspeak-speak-region pmark (point))))))

(defadvice comint-dynamic-list-filename-completions
    (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-speak-completions-if-available)))
;;; Directory tracking for shell buffers on  systems that have  /proc
;;; Adapted from Emacs Wiki:
(defun emacspeak-shell-dirtrack-procfs  (str)
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

Turning on Ditrack-Procfs mode automatically turns off
Shell-Dirtrack mode; turning it off does not re-enable it."
  nil "DirTrack" nil 
  (if (not dirtrack-procfs-mode)
      (remove-hook 'comint-preoutput-filter-functions #'emacspeak-shell-dirtrack-procfs t)
    (add-hook 'comint-preoutput-filter-functions #'emacspeak-shell-dirtrack-procfs nil t)
    (shell-dirtrack-mode 0)))
(when (file-exists-p "/proc")
  (add-hook 'shell-mode-hook 'dirtrack-procfs-mode))
;;}}}
;;{{{ Advice centering and filling commands:

(defadvice center-line (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'select-object)
    (message"Centered current line")))

(defadvice center-region (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'select-object)
    (message"Centered current region containing %s lines"
            (count-lines (region-beginning) (region-end)))))

(defadvice center-paragraph (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'select-object)
    (message"Centered current paragraph")))

(loop
 for f in
 '(fill-paragraph lisp-fill-paragraph)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p )
       (emacspeak-auditory-icon 'fill-object )
       (message "Filled current paragraph")))))

(defadvice fill-region (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'fill-object )
    (message "Filled current region containing %s lines"
             (count-lines (region-beginning)
                          (region-end)))))

;;}}}
;;{{{ vc:

;;; helper function: find out vc version:

;;; guess the vc version number from the variable used in minor mode alist
(defsubst emacspeak-vc-get-version-id ()
  "Return VC version id."
  (declare (special vc-mode ))
  (let ((id vc-mode ))
    (cond
     ((and vc-mode
           (stringp vc-mode))
      (substring id 5 nil ))
     (t " "))))

(defadvice vc-toggle-read-only (around emacspeak pre act comp)
  "Provide auditory feedback."
  (cond
   ((ems-interactive-p )
    (let ((message (format "Checking %s version %s "
                           (if buffer-read-only "out previous " " in new ")
                           (emacspeak-vc-get-version-id))))
      (if buffer-read-only
          (emacspeak-auditory-icon 'open-object )
        (emacspeak-auditory-icon 'close-object))
      ad-do-it
      (message message )))
   (t ad-do-it ))
  ad-return-value )

(defadvice vc-next-action (around emacspeak pre act comp)
  "Provide auditory feedback."
  (cond
   ((ems-interactive-p )
    (let ((message (format "Checking %s version %s "
                           (if buffer-read-only "out previous " " in new ")
                           (emacspeak-vc-get-version-id))))
      (if buffer-read-only
          (emacspeak-auditory-icon 'close-object)
        (emacspeak-auditory-icon 'open-object ))
      ad-do-it
      (message message)))
   (t ad-do-it ))
  ad-return-value )

(defadvice vc-revert-buffer (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'open-object)))

(defadvice vc-finish-logentry (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'close-object)
    (message "Checked in version %s "
             (emacspeak-vc-get-version-id))))

(loop for f in
      '(vc-dir-next-line vc-dir-previous-line
                         vc-dir-next-directory vc-dir-previous-directory
                         )
      do
      (eval
       `(defadvice ,f (after emacspeak-pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p )
            (emacspeak-speak-line)
            (emacspeak-auditory-icon 'select-object)))))

(defadvice vc-dir-mark-file (after emacspeak-pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'mark-object)))

(defadvice vc-dir-mark (after emacspeak-pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'mark-object)))

(defadvice vc-dir (after emacspeak pre act comp)
  "Produce auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice vc-dir-hide-up-to-date (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'task-done)
    (emacspeak-speak-line)))

(defadvice vc-dir-kill-line (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-speak-line)))

;;}}}
;;{{{ composing mail

(loop for f in
      '(mail mail-other-window mail-other-frame )
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Give some auditory feedback."
          (emacspeak-auditory-icon 'open-object)
          (save-excursion
            (goto-char (point-min))
            (emacspeak-speak-line)))))
(loop for f in
      '(mail-text mail-subject mail-cc mail-bcc
                  mail-to mail-reply-to mail-fcc)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Speak the reply-to line."
          (when (ems-interactive-p )
            (emacspeak-speak-line )))))

(defadvice mail-signature (after emacspeak pre act comp)
  "Announce you signed the message."
  (when (ems-interactive-p )
    (message "Signed your message")))

(defadvice mail-send-and-exit (after emacspeak pre act comp)
  "Speak the modeline of active buffer."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line )))

;;}}}
;;{{{ misc functions that have to be hand fixed:

(defadvice zap-to-char (after emacspeak pre act comp)
  "Speak line that is left."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-speak-line 1)))

(defadvice describe-mode (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (message "Displayed mode help in help window")
    (emacspeak-auditory-icon 'help)))
(loop for f in
      '(describe-bindings
        describe-prefix-bindings)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p )
            (message "Displayed key bindings in help window")
            (emacspeak-auditory-icon 'help)))))

(defadvice line-number-mode (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'button)
    (emacspeak-speak-mode-line)))

(defadvice column-number-mode (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'button)
    (emacspeak-speak-mode-line)))

(defadvice not-modified (after emacspeak pre act comp)
  "Provide an auditory icon."
  (when (ems-interactive-p )
    (if (ad-get-arg 0)
        (emacspeak-auditory-icon 'modified-object )
      (emacspeak-auditory-icon 'unmodified-object))))

(defadvice comment-region (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (ems-interactive-p )
    (let ((prefix-arg (ad-get-arg 2)))
      (message "%s region containing %s lines"
               (if (and prefix-arg
                        (< prefix-arg 0))
                   "Uncommented"
                 "Commented")
               (count-lines (point) (mark 'force))))))
(loop
 for f in
 '(save-buffer save-some-buffers)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p )
       (emacspeak-auditory-icon 'save-object)))))
(loop
 for f in
 '(kill-region completion-kill-region)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Indicate region has been killed.
Use an auditory icon if possible."
     (cond
      ((ems-interactive-p )
       (let ((count (count-lines (region-beginning) (region-end))))
         ad-do-it
         (emacspeak-auditory-icon 'delete-object )
         (message "Killed region containing %s lines" count)))
      (t ad-do-it))
     ad-return-value)))

(defadvice kill-ring-save (after emacspeak pre act comp)
  "Indicate that region has been copied to the kill ring.
Produce an auditory icon if possible."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'mark-object )
    (message "region containing %s lines copied to kill ring "
             (count-lines (region-beginning) (region-end)))))

(defadvice find-file (after emacspeak pre act comp)
  "Play an auditory icon if possible."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))
(loop
 for f in
 '(kill-buffer  quit-window)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speech-enabled by emacspeak."
     (when (ems-interactive-p )
       (emacspeak-auditory-icon 'close-object)
       (emacspeak-speak-mode-line)))))

(loop
 for f in
 '(delete-windows-on delete-other-frames
                     delete-window delete-completion-window
                     split-window-below split-window-right
                     split-window-vertically split-window-horizontally)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speech-enabled by emacspeak."
     (when (ems-interactive-p )
       (emacspeak-auditory-icon 'window-resize)
       (emacspeak-speak-mode-line)))))

(loop
 for f in
 '(
   other-frame other-window
               switch-to-prev-buffer switch-to-next-buffer
               switch-to-buffer switch-to-buffer-other-window switch-to-buffer-other-frame)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak modeline.
Indicate change of selection with an auditory icon
 if possible."
     (when (ems-interactive-p )
       (emacspeak-auditory-icon 'select-object)
       (emacspeak-speak-mode-line)))))

(defadvice make-frame-command (after emacspeak pre act comp)
  "Indicate that a new frame is being created."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice move-to-window-line (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice rename-buffer (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (ems-interactive-p )
    (emacspeak-speak-mode-line)))

(defadvice local-set-key (before emacspeak pre act comp)
  "Prompt using speech."
  (interactive
   (list
    (read-key-sequence "Locally bind key:")
    (read-command "To command:" ))))

(defadvice global-set-key (before emacspeak pre act comp)
  "Provide spoken prompts."
  (interactive
   (list
    (read-key-sequence "Globally bind key:")
    (read-command "To command:" ))))

(defadvice modify-syntax-entry (before emacspeak pre act comp)
  "Provide spoken prompts."
  (interactive
   (list
    (read-char "Modify syntax for: ")
    (read-string "Syntax Entry: ")
    current-prefix-arg)))
(defadvice help-follow (after emacspeak pre act comp)
  "Speak the ref we moved to."
  (when (ems-interactive-p )
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'button)))

;;; Silence help for help 
(defadvice help-window-display-message (around emacspeak pre act comp)
  (let ((emacspeak-speak-messages  nil))
    ad-do-it))

(loop
 for f in
 '(describe-function describe-variable describe-key)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak the help."
     (when  (ems-interactive-p )
       (emacspeak-auditory-icon 'help)
       (emacspeak-speak-help )))))

(defadvice help-with-tutorial (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'open-object)
    (let ((extent nil))
      (save-excursion
        (goto-char (point-min))
        (forward-line (window-height ))
        (emacspeak-speak-region (point-min) (point))))))
(defsubst ems-canonicalize-key-description (desc)
  "Change key description to a speech-friendly form."
  (let ((shift-regexp "S-\\(.\\)")
        (ctrl-regexp "C-\\(.\\)")
        (meta-regexp "M-\\(.\\)")
        (caps-regexp "\\b[A-Z]\\b")
        (hyper-regexp "C-x @ h")
        (alt-regexp "C-x @ a")
        (super-regexp "C-x @ s"))
    (condition-case nil
        (with-temp-buffer 
          (setq buffer-undo-list t)
          (setq case-fold-search nil)
          (erase-buffer)
          (insert desc)
          (goto-char (point-min))
          (save-match-data
            (while (search-forward "SPC" nil t )
              (replace-match "space"))
            (goto-char (point-min))
            (while (search-forward "ESC" nil t )
              (replace-match "escape"))
            (goto-char (point-min))
            (while (search-forward "RET" nil t )
              (replace-match "return"))
            (goto-char (point-min))
            (while (re-search-forward hyper-regexp nil t )
              (replace-match "hyper "))
            (goto-char (point-min))
            (while (re-search-forward alt-regexp nil t )
              (replace-match "alt "))
            (goto-char (point-min))
            (while (re-search-forward super-regexp nil t )
              (replace-match "super "))
            (goto-char (point-min))
            (while (re-search-forward shift-regexp nil t )
              (replace-match "shift \\1"))
            (goto-char (point-min))
            (while (re-search-forward ctrl-regexp nil t )
              (replace-match "control \\1"))
            (goto-char (point-min))
            (while (re-search-forward meta-regexp nil t )
              (replace-match "meta \\1"))
            (goto-char (point-min))
            (while (re-search-forward alt-regexp nil t )
              (replace-match "alt \\1"))
            (goto-char (point-min))
            (while (re-search-forward caps-regexp nil t)
              (replace-match " cap \\& " t)))
          (buffer-string ))
      (error ""))))

(defadvice exchange-point-and-mark (after emacspeak pre act comp)
  "Speak the line.
Indicate large movement with an auditory icon if possible.
Auditory highlight indicates position of point."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'large-movement )
    (let ((emacspeak-show-point t))
      (emacspeak-speak-line))))
(loop
 for f in
 '(newline newline-and-indent
           electric-newline-and-maybe-indent)
 do
 (eval
  `(defadvice ,f (before emacspeak pre act comp)
     "Speak the previous line if line echo is on.
See command \\[emacspeak-toggle-line-echo]. Otherwise cue the user to
the newly created blank line."
     (declare (special emacspeak-line-echo ))
     (when (ems-interactive-p )
       (cond
        (emacspeak-line-echo (emacspeak-speak-line ))
        (t(when dtk-stop-immediately (dtk-stop))
          (dtk-tone 225 120 'force )))))))
(loop
 for f in
 '(keyboard-quit keyboard-escape-quit)
 do
 (eval
  `(defadvice ,f (before emacspeak pre act comp)
     "Stop speech first."
     (when (ems-interactive-p)
       (dtk-stop)
       (emacspeak-auditory-icon 'warn-user)
       (dtk-speak "quit")))))
(loop
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
                                 (format "%s" ad-return-value ))))))))

(defadvice shell (after emacspeak pre act comp)
  "Announce switching to shell mode.
Provide an auditory icon if possible."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'select-object )
    (emacspeak-speak-mode-line)))

(loop
 for f in
 '(find-tag pop-tag-mark tags-loop-continue)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak the line please."
     (when (ems-interactive-p )
       (emacspeak-auditory-icon 'open-object)
       (emacspeak-speak-line )))))

(defadvice call-last-kbd-macro (around emacspeak pre act comp)
  "Provide spoken feedback."
  (cond
   ((ems-interactive-p )
    (let ((dtk-quiet t)
          (emacspeak-speak-messages nil)
          (emacspeak-use-auditory-icons nil))
      ad-do-it)
    (message "Executed macro. ")
    (emacspeak-auditory-icon 'task-done))
   (t ad-do-it))
  ad-return-value )

(defadvice kbd-macro-query (after emacspeak pre act comp)
  "Announce yourself."
  (when (ems-interactive-p )
    (message "Will prompt at this point in macro")))

(defadvice start-kbd-macro (before emacspeak pre act comp)
  "Announce yourself."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'open-object)
    (dtk-speak "Started defining a keyboard macro ")))

(defadvice end-kbd-macro (after emacspeak pre act comp)
  "Announce yourself."
  (when (ems-interactive-p )
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
  (when (ems-interactive-p )
    (message "Downcased region containing %s lines"
             (count-lines (region-beginning)
                          (region-end)))))

(defadvice upcase-region (after emacspeak pre act comp)
  "Give spoken confirmation."
  (when (ems-interactive-p )
    (message "Upcased region containing %s lines"
             (count-lines (region-beginning)
                          (region-end)))))
(loop for f in
      '(narrow-to-region narrow-to-page)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Announce yourself."
          (when (ems-interactive-p )
            (emacspeak-auditory-icon 'select-object)
            (message "Narrowed editing region to %s lines"
                     (count-lines (region-beginning)
                                  (region-end)))))))

(defadvice narrow-to-defun (after emacspeak pre act comp)
  "Announce yourself."
  (when (ems-interactive-p )
    (require 'which-func)
    (emacspeak-auditory-icon 'select-object)
    (message "Narrowed to function %s"
             (which-function))))

(defadvice widen (after emacspeak pre act comp)
  "Announce yourself."
  (when (ems-interactive-p )
    (message "You can now edit the entire buffer ")))

(defadvice delete-other-windows (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (ems-interactive-p )
    (message "Deleted all other windows")
    (emacspeak-auditory-icon 'window-resize)
    (emacspeak-speak-mode-line)))

(defadvice split-window-vertically (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (ems-interactive-p )
    (message "Split window vertically, current window has %s lines "
             (window-height))
    (emacspeak-speak-mode-line)))

(defadvice delete-window (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line )) )

(defadvice shrink-window (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (message "Current window has %s lines and %s columns"
             (window-height ) (window-width))))

(defadvice shrink-window-if-larger-than-buffer (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (message "Current window has %s lines and %s columns"
             (window-height ) (window-width))))

(defadvice balance-windows (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (message "Current window has %s lines and %s columns"
             (window-height ) (window-width))))

(defadvice split-window-horizontally (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (ems-interactive-p )
    (message "Split window horizontally current window has %s columns "
             (window-width))
    (emacspeak-speak-mode-line)))

(defadvice transpose-chars (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-speak-char t)))

(defadvice transpose-lines (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'yank-object )
    (emacspeak-speak-line )))

(defadvice transpose-words (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'yank-object )
    (emacspeak-speak-word )))

(defadvice transpose-sexps (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'yank-object )
    (emacspeak-speak-sexp )))

(defadvice open-line (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (let ((count (ad-get-arg 0)))
      (emacspeak-auditory-icon 'open-object)
      (message "Opened %s blank line%s"
               (if (= count 1) "a" count)
               (if (= count 1 ) "" "s")))))

(defadvice abort-recursive-edit (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (message "Aborting recursive edit")))
(loop for f in
      '(undo undo-only)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p )
            (let ((emacspeak-show-point t))
              (emacspeak-speak-line ))
            (if (buffer-modified-p)
                (emacspeak-auditory-icon 'modified-object)
              (emacspeak-auditory-icon 'unmodified-object ))))))

(defadvice view-emacs-news (after emacspeak pre act comp)
  "Provide auditory cue."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defvar emacspeak--help-char-helpbuf " *Char Help*"
  "This is hard-coded in subr.el")

(defadvice help-form-show (after emacspeak pre act comp)
  "Speak displayed help form."
  (declare (special emacspeak--help-char-helpbuf))
  (when (buffer-live-p (get-buffer emacspeak--help-char-helpbuf))
    (with-current-buffer emacspeak--help-char-helpbuf
      (goto-char (point-min))
      (emacspeak-speak-buffer))))
(defcustom emacspeak-speak-tooltips nil
  "Enable to get tooltips spoken."
  :type 'boolean
  :group 'emacspeak)

(defadvice tooltip-show-help(after emacspeak pre act comp)
  "Provide auditory feedback."
  (when emacspeak-speak-tooltips
    (let ((msg (ad-get-arg 0)))
      (if msg
          (dtk-speak msg)
        (emacspeak-auditory-icon 'close-object)))))

(loop for f in
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

(defadvice server-start (after emacspeak pre act comp)
  "Provide auditory confirmation."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'task-done)))

(defadvice server-edit (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-speak-mode-line )))

;;}}}
;;{{{ view echo area

(defadvice view-echo-area-messages (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'open-object)
    (message "Displayed messages in other window.")))

;;}}}
;;{{{ selective display

(defadvice set-selective-display (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (ems-interactive-p )
    (message "Set selective display to %s"
             (ad-get-arg 0))
    (emacspeak-auditory-icon 'button)))

;;}}}
;;{{{ avoid chatter when byte compiling etc

(defadvice byte-compile-file (around emacspeak pre act comp)
  "Announce one message, quietly compile, and announce termination.
Produce an auditory icon if possible."
  (cond
   ((ems-interactive-p )
    (let ((emacspeak-speak-messages nil))
      (dtk-speak "Byte compiling ")
      ad-do-it
      (emacspeak-auditory-icon 'task-done)
      (dtk-speak "Done byte compiling ")))
   (t ad-do-it))
  ad-return-value)

;;}}}
;;{{{ Stop talking if activity

(loop
 for f in
 '(beginning-of-line end-of-line
                     move-beginning-of-line move-end-of-line
                     recenter-top-bottom recenter)
 do
 (eval
  `(defadvice ,f (before emacspeak pre act comp)
     "Speak line."
     (when (ems-interactive-p )
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'select-object)))))

;;}}}
;;{{{ yanking and popping

(loop
 for f in
 '(yank yank-pop)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Say what you yanked.
Produce an auditory icon if possible."
     (when (ems-interactive-p )
       (emacspeak-auditory-icon 'yank-object )
       (emacspeak-speak-region (mark 'force) (point))))))

;;}}}
;;{{{ advice non-incremental searchers

(loop
 for f in
 '(search-forward search-backward
                  word-search-forward word-search-backward)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak line we land on."
     (when (ems-interactive-p )
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'select-object)))))

;;}}}
;;{{{ customize isearch:

;;; Fix key bindings:

(declaim (special isearch-mode-map
                  minibuffer-local-isearch-map emacspeak-prefix))

(define-key
  minibuffer-local-isearch-map emacspeak-prefix 'emacspeak-prefix-command)
(define-key isearch-mode-map emacspeak-prefix 'emacspeak-prefix-command)
(define-key isearch-mode-map "\M-m" 'isearch-exit)
;;; ISearch setup/teardown

;;; temporarily silence messages,
;;; Produce auditory icon

(defsubst emacspeak-isearch-setup()
  "Setup emacspeak environment for isearch."
  (declare (special emacspeak-speak-messages))
  (emacspeak-auditory-icon 'open-object)
  (setq emacspeak-speak-messages nil)
  (dtk-speak "I-Search: "))

(defsubst emacspeak-isearch-teardown()
  "Teardown emacspeak environment for isearch."
  (declare (special emacspeak-speak-messages))
  (emacspeak-auditory-icon 'close-object)
  (setq emacspeak-speak-messages t))

(add-hook 'isearch-mode-hook 'emacspeak-isearch-setup)
(add-hook 'isearch-mode-end-hook 'emacspeak-isearch-teardown)

;;; Advice isearch-search to speak

(defadvice isearch-search (after emacspeak pre act comp)
  "Speak the search hit."
  (emacspeak-speak-string isearch-string voice-bolden)
  (when (sit-for 0.5)
    (emacspeak-auditory-icon 'search-hit)
    (save-excursion
      (ems-set-personality-temporarily
       (point)
       (if isearch-forward
           (- (point) (length isearch-string ))
         (+ (point) (length isearch-string )))
       voice-bolden
       (dtk-speak
        (buffer-substring
         (line-beginning-position)
         (line-end-position)))))))

(defadvice isearch-delete-char (after emacspeak pre act comp)
  "Speak the search hit.
Produce auditory icons if possible."
  (emacspeak-speak-string isearch-string voice-bolden)
  (when (sit-for 0.5)
    (emacspeak-auditory-icon 'search-hit)
    (ems-set-personality-temporarily
     (point)
     (if isearch-forward
         (- (point) (length isearch-string ))
       (+ (point) (length isearch-string )))
     voice-bolden
     (emacspeak-speak-line nil ))))
(loop
 for f in
 '(isearch-yank-word isearch-yank-kill isearch-yank-line)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p )
       (emacspeak-speak-string isearch-string voice-bolden)
       (emacspeak-auditory-icon 'yank-object)))))
(loop
 for f in
 '(isearch-ring-advance isearch-ring-retreat
                        isearch-ring-advance-edit isearch-ring-retreat-edit)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p )
       (emacspeak-speak-string isearch-string voice-bolden)
       (emacspeak-auditory-icon 'select-object)))))

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

;;}}}
;;{{{ marking objects produces auditory icons

;;; Prevent push-mark from displaying its mark set message
;;; when called from functions that know better.
(defvar emacspeak-advice-smart-mark-functions
  (list 'mark-defun
        'mark-whole-buffer
        'mark-paragraph
        'mark-page
        'mark-word
        'mark-perl-function)
  "Functions that display their own smart mark set message.")

(defadvice push-mark (around emacspeak pre act comp)
  "Never show the mark set message."
  (or (ad-get-arg 1)
      (memq last-command emacspeak-advice-smart-mark-functions)
      (ad-set-arg 1 t))
  ad-do-it
  ad-return-value)
(loop
 for f in
 '(set-mark-command pop-to-mark-command pop-global-mark)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Produce an auditory icon if possible."
     (when (ems-interactive-p )
       (emacspeak-auditory-icon 'mark-object )
       (let ((emacspeak-show-point t))
         (emacspeak-speak-line ))))))

(defadvice mark-defun (after emacspeak pre act comp)
  "Produce an auditory icon if possible."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'mark-object)
    (message "Marked function containing %s lines"
             (count-lines (point) (mark 'force)))))

(defadvice mark-whole-buffer (after emacspeak pre act comp)
  "Produce an auditory icon if possible."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'mark-object)
    (message "Marked buffer containing %s lines"
             (count-lines (point) (mark 'force)))))

(defadvice mark-paragraph (after emacspeak pre act comp)
  "Produce an auditory icon if possible."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'mark-object)
    (message "Marked paragraph containing %s lines"
             (count-lines (point)
                          (mark 'force)))))

(defadvice mark-page (after emacspeak pre act comp)
  "Produce an auditory icon if possible."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'mark-object)
    (message "Marked page containing %s lines"
             (count-lines (point)
                          (mark 'force)))))

(defadvice mark-word (after emacspeak pre act comp)
  "Produce an auditory icon if possible."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'mark-object)
    (message "Word %s marked"
             (buffer-substring-no-properties (point) (mark 'force)))))

(defadvice mark-sexp (after emacspeak pre act comp)
  "Produce an auditory icon if possible."
  (when (ems-interactive-p )
    (let ((lines (count-lines (point)
                              (mark 'force)))
          (chars (abs (- (point) (mark 'force)))))
      (emacspeak-auditory-icon 'mark-object)
      (if (> lines 1)
          (message "Marked S expression spanning %s lines" lines)
        (message "marked S expression containing %s characters"
                 chars)))))

(defadvice mark-end-of-sentence (after emacspeak pre act comp)
  "Produce an auditory icon if possible."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'mark-object)))

;;}}}
;;{{{ emacs registers

(defadvice point-to-register (after emacspeak pre act comp)
  "Produce auditory icon to indicate mark set."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'mark-object)
    (if current-prefix-arg
        (message "Stored current frame configuration")
      (emacspeak-speak-line))))

(defadvice copy-to-register (after emacspeak pre act comp)
  "Acknowledge the copy."
  (when (ems-interactive-p )
    (let ((start (ad-get-arg 1))
          (end (ad-get-arg 2 ))
          (register (ad-get-arg 0))
          (lines nil)
          (chars nil))
      (setq lines (count-lines start end)
            chars (abs (- start end )))
      (if (> lines 1)
          (message "Copied %s lines to register %c"
                   lines register)
        (message "Copied %s characters to register %c"
                 chars register)))))
(defadvice view-register (after emacspeak pre act comp)
  "Speak displayed contents."
  (when (ems-interactive-p )
    (with-current-buffer "*Output*"
      (dtk-speak (buffer-string ))
      (emacspeak-auditory-icon 'open-object))))

(defadvice jump-to-register (after emacspeak pre act comp)
  "Speak the line you jumped to."
  (when (ems-interactive-p )
    (let ((emacspeak-show-point t))
      (emacspeak-speak-line ))))

(defadvice insert-register (after emacspeak pre act comp)
  "Speak the first line of the inserted text."
  (when (ems-interactive-p )
    (let ((emacspeak-show-point t))
      (emacspeak-auditory-icon 'yank-object)
      (emacspeak-speak-line ))))

(defadvice window-configuration-to-register (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (message "Copied window configuration to register %c" (ad-get-arg 0 ))))
(loop
 for  f in
 '(frameset-to-register frame-configuration-to-register)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p )
       (message "Copied frame  configuration to register %c" (ad-get-arg 0 ))))))

;;}}}
;;{{{ set up clause boundaries for specific modes:

(defsubst emacspeak-speak-adjust-clause-boundaries ()
  "Adjust clause boundaries so that newlines dont delimit clauses."
  (declare (special dtk-chunk-separator-syntax))
  (setq dtk-chunk-separator-syntax ".)$\""))

(add-hook 'help-mode-hook 'emacspeak-speak-adjust-clause-boundaries)
(add-hook 'text-mode-hook 'emacspeak-speak-adjust-clause-boundaries)

;;}}}
;;{{{ setup minibuffer hooks:

(defvar emacspeak-minibuffer-enter-auditory-icon t
  "Produce auditory icon when entering the minibuffer.")

(defun emacspeak-minibuffer-setup-hook ()
  "Actions to take when entering the minibuffer with emacspeak running."
  (declare (special emacspeak-minibuffer-enter-auditory-icon))
  (let ((inhibit-field-text-motion t))
    (when emacspeak-minibuffer-enter-auditory-icon
      (emacspeak-auditory-icon 'open-object))
    (unwind-protect
        (tts-with-punctuations 'all (emacspeak-speak-buffer)))))

(add-hook 'minibuffer-setup-hook 'emacspeak-minibuffer-setup-hook)

(defun emacspeak-minibuffer-exit-hook ()
  "Actions performed when exiting the minibuffer with Emacspeak loaded."
  (emacspeak-auditory-icon 'close-object))

;;}}}
;;{{{ Advice occur

(loop
 for f in
 '(occur-prev occur-next occur-mode-goto-occurrence)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide spoken feedback."
     (when (ems-interactive-p )
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'large-movement)))))

;;}}}
;;{{{ abbrev mode advice

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
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'button)
    (message "Turned %s abbrev mode"
             (if abbrev-mode "on" "off"))))

;;}}}
;;{{{ advice where-is and friends

(defadvice describe-key-briefly (after  emacspeak pre act comp)
  "Speak what you displayed"
  (when (ems-interactive-p )
    (dtk-speak (ems-canonicalize-key-description ad-return-value))))

(defadvice where-is (after emacspeak pre act comp)
  "Provide spoken feedback"
  (when (ems-interactive-p )
    (emacspeak-speak-message-again)))

;;}}}
;;{{{ apropos and friends
(loop
 for f in
 '(apropos-command apropos-documentation)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide an auditory icon."
     (when (ems-interactive-p )
       (emacspeak-auditory-icon 'help)))))

(defadvice apropos-follow (after emacspeak pre act comp)
  "Speak the help you displayed."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-help)))

;;}}}
;;{{{ speak context after done garbage collecting

(defadvice garbage-collect (after emacspeak pre act comp)
  "Speak modeline when done."
  (when (ems-interactive-p )
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'select-object)))

;;}}}
;;{{{ toggling debug state

(defadvice toggle-debug-on-error (after emacspeak pre act comp)
  "Produce an auditory icon."
  (when (ems-interactive-p )
    (if debug-on-error
        (emacspeak-auditory-icon 'on)
      nil
      (emacspeak-auditory-icon 'off))
    (message "Turned %s debug on error" debug-on-error)))

(defadvice toggle-debug-on-quit (after emacspeak pre act comp)
  "Produce an auditory icon."
  (when (ems-interactive-p )
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
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'warn-user)
    (message "Turned %s overwrite mode" (or overwrite-mode "off"))))

;;}}}
;;{{{ Options mode and custom

(defadvice customize (after emacspeak pre act comp)
  "Provide status update."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))
(defadvice customize-save-variable (around emacspeak pre act comp)
  "Silence chatter."
  (let ((emacspeak-speak-messages nil)
        (dtk-quiet t))
    ad-do-it))

;;}}}
;;{{{ transient mark mode

(defadvice transient-mark-mode (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon (if transient-mark-mode 'on 'off))
    (message "Turned %s transient mark." (if transient-mark-mode "on" "off"))))

;;}}}
;;{{{ provide auditory icon when window config changes

;;}}}
;;{{{ advice load to speech-enable new commands

(defadvice load (after emacspeak pre act comp)
  "Fix interactive commands just defined."
  (emacspeak-fix-commands-loaded-from
   (file-name-sans-extension (ad-get-arg 0))))

;;}}}
;;{{{ mail aliases

(defadvice expand-mail-aliases (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'select-object)))

;;}}}
;;{{{ elint

(loop
 for f in
 '(elint-current-buffer elint-file elint-defun)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Silence messages while elint is running."
     (cond
      ((ems-interactive-p )
       (let ((emacspeak-speak-messages nil))
         ad-do-it
         (emacspeak-auditory-icon 'task-done)
         (message "Displayed lint results in other window. ")))
      (t ad-do-it))
     ad-return-value)))

;;}}}
;;{{{ advice button creation to add voicification:

(loop
 for f in
 '(make-button make-text-button)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Adds property personality."
     (let ((beg (ad-get-arg 0))
           (end (ad-get-arg 1)))
       (with-silent-modifications
         (add-text-properties
          beg end
          (list 'personality voice-bolden
                'auditory-icon 'button)))))))

(defadvice push-button (after emacspeak pre act comp)
  "Produce auditory icon."
  (when  (ems-interactive-p ) 
    (emacspeak-auditory-icon 'push-button)
    (emacspeak-speak-line)))

;;}}}
;;{{{ silence whitespace cleanup:

(loop
 for f in
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
  (when(and (boundp 'finder-known-keywords)
            (not (eq 'emacspeak (caar finder-known-keywords))))
    (push (cons 'emacspeak "Audio Desktop")
          finder-known-keywords))
  (emacspeak-auditory-icon 'open-object)
  (emacspeak-speak-mode-line))

;;}}}
;;{{{ Advice show-paren:

;;; We use our own blink-paren functionality:

(defadvice show-paren-mode (after emacspeak pre act comp)
  "Warn user."
  (when (ad-get-arg 0)
    (message "Warning: show-paren mode with Emacspeak will not work.")))

;;}}}
;;{{{ display world time

(defadvice display-time-world (after emacspeak pre act comp)
  "Speak what you displayed."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'open-object)
    (save-current-buffer
      (set-buffer "*wclock*")
      (emacspeak-speak-buffer))))

;;}}}
;;{{{ browse-url

(loop for f in
      '(browse-url-of-buffer browse-url-of-region)
      do
      (eval
       `(defadvice ,f (around emacspeak pre act comp)
          "Automatically speak results of rendering."
          (cond
           ((ems-interactive-p )
            (emacspeak-webutils-autospeak)
            ad-do-it)
           (t ad-do-it))
          ad-return-value)))

;;}}}
;;{{{ Cue input method changes

(defadvice toggle-input-method (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon
     (if current-input-method 'on 'off))
    (dtk-speak
     (format "Current input method is %s"
             (or current-input-method "none")))))

;;}}}
;;{{{ silence midnight cleanup:
(defadvice clean-buffer-list(around emacspeak pre act comp)
  (let ((emacspeak-speak-messages nil))
    ad-do-it))

;;}}}
;;{{{ Splash Screen:

(loop for f in
      '(about-emacs display-about-screen)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p )
            (emacspeak-auditory-icon 'open-object)
            (emacspeak-speak-buffer)))))

(defadvice exit-splash-screen (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{ copyright commands:

(loop for f in
      '(copyright copyright-update)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p )
            (emacspeak-auditory-icon 'task-done)
            (emacspeak-speak-line)))))

(defadvice copyright-update-directory (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'task-done)))

;;}}}
;;{{{ Managing packages:
(defadvice package-menu-execute(around emacspeak pre act comp)
  "Silence messages while installing packages. "
  (let ((emacspeak-speak-messages nil))
    ad-do-it))
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
(provide 'emacspeak-advice)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
