;;; emacspeak-comint.el --- Speech-enable COMINT  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable COMINT An Emacs Interface to comint
;;; Keywords: Emacspeak,  Audio Desktop comint
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;;Copyright (C) 1995 -- 2007, 2019, T. V. Raman
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
;;; MERCHANTABILITY or FITNCOMINT FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:

;;; comint == command interaction.
;;; Advice comint and friends to speak.
;;; 


;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'comint)
(require 'shell)

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
  "Make comint speak its output.
Try not to speak the shell prompt,
instead, always play an auditory icon when the shell prompt is displayed."
  (let ((monitor emacspeak-comint-output-monitor)
        (buffer (process-buffer (ad-get-arg 0)))
        (output (ad-get-arg 1)))
    (with-current-buffer buffer
      ad-do-it
      (when
          (and comint-last-output-start
               (or monitor (eq (window-buffer) buffer)))
        (let
            ((prompt-p
              (memq
               'comint-highlight-prompt
               (get-text-property comint-last-output-start 'font-lock-face))))
          (cond
           ( (and emacspeak-comint-autospeak (not prompt-p))
             (dtk-speak output))
           (prompt-p (emacspeak-auditory-icon 'item)))))
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

;;}}}
(provide 'emacspeak-comint)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
