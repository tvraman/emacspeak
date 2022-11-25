;;; emacspeak-speak.el --- Core Speech Lib -*- lexical-binding: t; -*-
;;
;; $Author: tv.raman.tv $
;; Description:  Contains the functions for speaking various chunks of text
;; Keywords: Emacspeak,  Spoken Output
;;{{{  LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;; 
;;  $Revision: 4552 $ |
;; Location undetermined
;; 

;;}}}
;;{{{  Copyright:

;; Copyright (C) 1995 -- 2022, T. V. Raman
;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
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
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  Introduction:

;;; Commentary:

;; This module defines the core speech services used by emacspeak.
;; It depends on the speech server interface modules
;; It protects other parts of emacspeak
;; from becoming dependent on the speech server modules

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'voice-setup)
(require 'voice-defs)
(require 'dtk-speak)
(require 'emacspeak-pronounce)
(require 'emacspeak-sounds)
(require 'sox-gen)
(declare-function emacspeak-play-auditory-icon "emacspeak-sounds" (sound-name))
(declare-function operate-on-rectangle "rect" (function start end coerce-tabs))
(declare-function which-function "which-func" nil)
(declare-function calendar-cursor-to-nearest-date "cal-move" nil)

(declare-function word-at-point "thingatpt" (&optional no-properties))
(require 'text-property-search)
;;}}}
;;{{{ This line:

(defsubst ems--this-line ()
  "Return current line as string."
  (buffer-substring (line-beginning-position) (line-end-position)))

;;}}}
;;{{{Clause Boundary Helper:

(defun emacspeak-speak-adjust-clause-boundaries ()
  "Adjust clause boundaries so that newlines dont delimit clauses."
  (cl-declare (special dtk-chunk-separator-syntax))
  (setq dtk-chunk-separator-syntax ".)$\""))

;;}}}
;;{{{Helper: Log Message Quietly

(defun ems--log-message (m)
  "Log a message without echoing it."
  (let ((inhibit-read-only t))
    (with-current-buffer (messages-buffer)
      (goto-char (point-max))
      (insert (format "%s\n" m)))))

;;}}}
;;{{{  line, Word and Character echo

(defcustom emacspeak-line-echo nil
  "If t, then emacspeak echoes lines as you type.
You can use \\[emacspeak-toggle-line-echo] to set this
option."
  :group 'emacspeak
  :type 'boolean)

(ems-generate-switcher 'emacspeak-toggle-line-echo
                       'emacspeak-line-echo
                       "Toggle state of  Emacspeak  line echo.
Interactive PREFIX arg means toggle  the global default value, and then set the
current local  value to the result.")

(defcustom emacspeak-word-echo t
  "If t, then emacspeak echoes words as you type.
You can use \\[emacspeak-toggle-word-echo] to toggle this
option."
  :group 'emacspeak
  :type 'boolean)

(ems-generate-switcher 'emacspeak-toggle-word-echo
                       'emacspeak-word-echo
                       "Toggle state of  Emacspeak  word echo.
Interactive PREFIX arg means toggle  the global default value, and then set the
current local  value to the result.")

(defcustom emacspeak-character-echo t
  "If t, then emacspeak echoes characters  as you type.
You can
use \\[emacspeak-toggle-character-echo] to toggle this
setting."
  :group 'emacspeak
  :type 'boolean)

(ems-generate-switcher 'emacspeak-toggle-character-echo
                       'emacspeak-character-echo
                       "Toggle state of  Emacspeak  character echo.
Interactive PREFIX arg means toggle  the global default value, and then set the
current local  value to the result.")

;;}}}
;;{{{Echo Typing Chars:

(defun emacspeak-post-self-insert-hook ()
  "Speaks the character if emacspeak-character-echo is true.
See  command emacspeak-toggle-word-echo bound to
\\[emacspeak-toggle-word-echo].
Speech flushes as you type."
  (cl-declare (special last-command-event 
                       emacspeak-character-echo emacspeak-word-echo))
  (when buffer-read-only (dtk-speak "Buffer is read-only. "))
  (when
      (and (eq (preceding-char) last-command-event) ; Sanity check.
           (not executing-kbd-macro)
           (not noninteractive))
    (let ((display (get-char-property (1- (point)) 'display)))
      (dtk-stop)
      (cond
       ((stringp display) (dtk-say display))
       ((and emacspeak-word-echo
             (= (char-syntax last-command-event)32))
        (save-excursion
          (condition-case nil
              (forward-word -1)
            (error nil))
          (emacspeak-speak-word)))
       (emacspeak-character-echo
        (emacspeak-speak-this-char (preceding-char)))))))

(add-hook 'post-self-insert-hook 'emacspeak-post-self-insert-hook)

;;}}}
;;{{{ Shell Command Helper:

(defcustom emacspeak-speak-messages t
  "Option indicating if messages are spoken.  If nil,
emacspeak will not speak messages as they are echoed to the
message area.  You can use command
`emacspeak-toggle-speak-messages' bound to
\\[emacspeak-toggle-speak-messages]."

  :group 'emacspeak
  :type 'boolean)

;; Emacspeak silences messages from shell-command when called
;; non-interactively.  This replacement is used within Emacspeak to
;; invoke commands whose output we want to hear.

(defun emacspeak-shell-command (command)
  "Run shell command COMMANDAND speak its output."
  (interactive "sCommand:")
  (cl-declare (special default-directory))
  (let ((directory default-directory)
        (output (get-buffer-create "*Emacspeak Shell Command*")))
    (with-current-buffer output
      (erase-buffer)
      (setq default-directory directory)
      (ems-with-messages-silenced
       (shell-command command output))
      (emacspeak-auditory-icon 'open-object)
      (dtk-speak (buffer-string)))))

;;}}}
;;{{{ Utility command to run and tabulate shell output

(defun emacspeak-speak-run-shell-command (command &optional read-as-csv)
  "Invoke shell COMMAND and display its output as a table. The
results are placed in a buffer in Emacspeak's table browsing
mode. Optional interactive prefix arg read-as-csv interprets the
result as csv. . Use this for running shell commands that produce
tabulated output. This command should be used for shell commands
that produce tabulated output that works with Emacspeak's table
recognizer. Verify this first by running the command in a shell
and executing command `emacspeak-table-display-table-in-region'
normally bound to \\[emacspeak-table-display-table-in-region]."
  (interactive
   (list
    (read-from-minibuffer "Shell command: ")
    current-prefix-arg))
  (let ((buffer-name (format "%s" command))
        (start nil)
        (end nil))
    (shell-command command buffer-name)
    (save-current-buffer
      (set-buffer buffer-name)
      (untabify (point-min) (point-max))
      (setq start (point-min)
            end (1- (point-max)))
      (condition-case nil
          (cond
           (read-as-csv (emacspeak-table-view-csv-buffer (current-buffer)))
           (t (emacspeak-table-display-table-in-region start end)))
        (error (message "Output could not be tabulated correctly")))
      (emacspeak-auditory-icon 'open-object)
      (emacspeak-speak-mode-line))))

;;}}}
;;{{{ Notifications:

(defun emacspeak--notifications-init ()
  "Init Notifications buffer."
  (let ((buffer (get-buffer-create "*Notifications*")))
    (with-current-buffer buffer
      (special-mode)
      buffer)))

(defvar emacspeak-notifications-buffer
  (emacspeak--notifications-init)
  "Notifications buffer. Retains at most `emacspeak-notifications-max lines.")

(defun emacspeak-view-notifications ()
  "Display notifications."
  (interactive)
  (cl-declare (special emacspeak-notifications-buffer))
  (unless (buffer-live-p emacspeak-notifications-buffer)
    (setq emacspeak-notifications-buffer (emacspeak--notifications-init)))
  (emacspeak-auditory-icon 'open-object)
  (funcall-interactively #'pop-to-buffer emacspeak-notifications-buffer))

(defconst emacspeak-notifications-max 128
  " notifications cache-size")

(defun emacspeak-notifications-truncate ()
  "Trim notifications cache."
  (cl-declare (special emacspeak-notifications-buffer 
                       emacspeak-notifications-max))
  (with-current-buffer emacspeak-notifications-buffer
    (let ((lines (count-lines (point-min) (point-max)))
          (inhibit-read-only t))
      (when (> lines emacspeak-notifications-max)
        (goto-char (point-min))
        (forward-line (- lines emacspeak-notifications-max))
        (delete-region (point-min) (point))))))

(defun emacspeak-log-notification (text)
  "Log a notification."
  (cl-declare (special emacspeak-notifications-buffer))
  (unless (buffer-live-p emacspeak-notifications-buffer)
    (setq emacspeak-notifications-buffer (emacspeak--notifications-init)))
  (with-current-buffer emacspeak-notifications-buffer
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (format "%s\n" text)))))

(defvar emacspeak-notifications-gc-timer
  (run-at-time 1800 1800 #'emacspeak-notifications-truncate)
  "Idle timer that runs every 30 minutes to cleanup notifications.")

;;}}}
;;{{{ Completion helper:

(defun emacspeak-speak-completions-if-available ()
  "Speak completions if available."
  (interactive)
  (let ((completions (get-buffer "*Completions*")))
    (cond
     ((and completions
           (window-live-p (get-buffer-window completions)))
      (save-current-buffer
        (set-buffer completions)
        (emacspeak-auditory-icon 'help)
        (dtk-chunk-on-white-space-and-punctuations)
        (next-completion 1)
        (tts-with-punctuations
         'all
         (dtk-speak (buffer-substring (point) (point-max))))))
     (t (emacspeak-speak-line)))))

;;}}}
;;{{{  Macros

;; Save read-only and modification state, perform some actions and
;; restore state

(defmacro ems-set-personality-temporarily (start end value &rest body)
  "Temporarily set personality.
Arguments start and end specify the region.
Argument VALUE is the personality to set temporarily
Argument BODY specifies forms to execute."
  (declare (indent 1) (debug t))
  `(let ((saved-personality (get-text-property ,start 'personality)))
     (with-silent-modifications
       (unwind-protect
           (progn
             (put-text-property
              (max (point-min) ,start)
              (min (point-max) ,end)
              'personality ,value)
             ,@body)
         (put-text-property
          (max (point-min) ,start)
          (min (point-max) ,end) 'personality saved-personality)))))

(defmacro ems-set-pause-temporarily (start end duration &rest body)
  "Temporarily set property pause.
Arguments start and end specify region.
Argument duration specifies duration in milliseconds.
Argument BODY specifies forms to execute."
  (declare (indent 1) (debug t))
  `(let ((saved-pause (get-text-property ,start 'pause)))
     (with-silent-modifications
       (unwind-protect
           (progn
             (put-text-property
              (max (point-min) ,start)
              (min (point-max) ,end)
              'pause ,duration)
             ,@body)
         (put-text-property
          (max (point-min) ,start)
          (min (point-max) ,end) 'pause saved-pause)))))

(defmacro ems-with-errors-silenced (&rest body)
  "Evaluate body  after temporarily silencing auditory error feedback."
  (declare (indent 1) (debug t))
  `(let ((emacspeak-speak-messages nil))
     ,@body))

;;}}}
;;{{{  Apply audio annotations

(defun emacspeak-audio-annotate-paragraphs ()
  "Set property auditory-icon at front of all paragraphs."
  (save-excursion
    (goto-char (point-max))
    (with-silent-modifications
      (let ((sound-cue 'paragraph))
        (while (not (bobp))
          (backward-paragraph)
          (put-text-property (point) (+ 2 (point))
                             'auditory-icon sound-cue))))))

(defvar emacspeak-speak-paragraph-personality voice-animate
  "Personality used to mark start of paragraph.")

(defvar-local  emacspeak-speak-voice-annotated-paragraphs nil
  "Records if paragraphs in this buffer have been voice annotated.")

(defun emacspeak-speak-voice-annotate-paragraphs ()
  "Locate paragraphs and voice annotate the first word.
Here, paragraph is taken to mean a chunk of text preceded by a blank line.
Useful to do this before you listen to an entire buffer."
  (interactive)
  (cl-declare (special emacspeak-speak-paragraph-personality
                       emacspeak-speak-voice-annotated-paragraphs))
  (when
      (and  emacspeak-speak-paragraph-personality
            (null emacspeak-speak-voice-annotated-paragraphs)) ; memoized
    (save-excursion
      (goto-char (point-min))
      (condition-case nil
          (let ((start nil)
                (blank-line "\n[ \t\n\r]*\n")
                (inhibit-modification-hooks t)
                (deactivate-mark nil))
            (with-silent-modifications
              (while (re-search-forward blank-line nil t)
                (skip-syntax-forward " ")
                (setq start (point))
                (unless (get-text-property start 'personality)
                  (skip-syntax-forward "^ ")
                  (put-text-property
                   start (point)
                   'personality emacspeak-speak-paragraph-personality)))))
        (error nil))
      (setq emacspeak-speak-voice-annotated-paragraphs t))))

;;}}}
;;{{{ Showing the point:

(defvar emacspeak-show-point nil
  " If T, command `emacspeak-speak-line' \\[emacspeak-speak-line]
indicates position of point by an aural highlight.  
Command `emacspeak-toggle-show-point' bound to
\\[emacspeak-toggle-show-point]  toggles this setting.")

(ems-generate-switcher 'emacspeak-toggle-show-point
                       'emacspeak-show-point
                       "Toggle state of  Emacspeak-show-point.
Interactive PREFIX arg means toggle  the global default value, and then set the
current local  value to the result.")

;;}}}
;;{{{ compute percentage into the buffer:

(defsubst emacspeak-get-current-percentage-into-buffer ()
  "Return percentage of position into current buffer."
  (let* ((pos (point))
         (total (buffer-size))
         (percent (if (> total 50000)
                      ;; Avoid overflow from multiplying by 100!
                      (/ (+ (/ total 200) (1- pos)) (max (/ total 100) 1))
                    (/ (+ (/ total 2) (* 100 (1- pos))) (max total 1)))))
    percent))

(defun emacspeak-get-current-percentage-verbously ()
  "Return percentage of position into current buffer as a string."
  (let ((percent (emacspeak-get-current-percentage-into-buffer)))
    (propertize
     (cond
      ((= 0 percent) " top ")
      ((= 100 percent) " bottom ")
      (t (format " %d%% " percent)))
     'personality voice-monotone-extra)))

(defun emacspeak-goto-percent (percent)
  "Move to end  PERCENT of buffer like in View mode.
Display is centered at point.
Also set the mark at the position where point was."
  (interactive "nPercent:")
  (push-mark)
  (goto-char
   (if percent
       (+ (point-min)
          (floor (* (- (point-max) (point-min)) 0.01
                    (max 0 (min 100 (prefix-numeric-value percent))))))
     (point-max)))
  (recenter)
  (emacspeak-auditory-icon 'large-movement)
  (emacspeak-speak-line))

;;}}}
;;{{{  indentation:

(defcustom emacspeak-audio-indentation nil
  "Option indicating if line indentation is cued.
You can use  command `emacspeak-toggle-audio-indentation' bound
to \\[emacspeak-toggle-audio-indentation] to toggle this
setting."
  :group 'emacspeak
  :type 'boolean)

(make-variable-buffer-local 'emacspeak-audio-indentation)

;; Indicate indentation.
;; Argument indent   indicates number of columns to indent.

;;}}}
;;{{{ filtering columns

(defvar-local emacspeak-speak-line-column-filter nil
  "List that specifies columns to be filtered.
The list when set holds pairs of start-col.end-col pairs
that specifies the columns that should not be spoken.
Each column contains a single character --this is inspired
by cut -c on UNIX.")

(defvar emacspeak-speak-filter-table (make-hash-table)
  "Hash table holding persistent filters.")

(defvar-local emacspeak-speak-line-invert-filter nil
  "Non-nil means the sense of `filter' is inverted when filtering
columns in a line --see
command emacspeak-speak-line-set-column-filter.")

(ems-generate-switcher 'emacspeak-toggle-speak-line-invert-filter
                       'emacspeak-speak-line-invert-filter
                       "Toggle state of   how column filter is interpreted.
Interactive PREFIX arg means toggle  the global default value, and then set the
current local  value to the result.")

(defun emacspeak-speak-line-apply-column-filter (line &optional invert-filter)
  (cl-declare (special emacspeak-speak-line-column-filter))
  (let ((filter emacspeak-speak-line-column-filter)
        (l (length line))
        (pair nil)
        (personality (if invert-filter nil
                       'inaudible)))
    (with-silent-modifications
      (when invert-filter
        (put-text-property 0 l
                           'personality 'inaudible line))
      (while filter
        (setq pair (pop filter))
        (when (and (<= (cl-first pair) l)
                   (<= (cl-second pair) l))
          (put-text-property (cl-first pair)
                             (cl-second pair)
                             'personality personality
                             line))))
    line))

(defun emacspeak-speak-persist-filter-entry (k v)
  (insert
   (format
    "(puthash
(intern \"%s\")
'%s
emacspeak-speak-filter-table)\n" k v)))

(defcustom emacspeak-speak-filter-persistent-store
  (expand-file-name ".filters"
                    emacspeak-user-directory)
  "File where emacspeak filters are persisted."
  :type 'file
  :group 'emacspeak)

(defvar emacspeak-speak-filters-loaded-p nil
  "Records if we    have loaded filters in this session.")

(defun emacspeak-speak-lookup-persistent-filter (key)
  "Lookup a filter setting we may have persisted."
  (cl-declare (special emacspeak-speak-filter-table))
  (or
   (gethash (intern key) emacspeak-speak-filter-table)
   (list (list 0 (current-column)))))

(defun emacspeak-speak-set-persistent-filter (key value)
  "Persist filter setting for future use."
  (cl-declare (special emacspeak-speak-filter-table))
  (setf (gethash (intern key) emacspeak-speak-filter-table)
        value))

(defun emacspeak-speak-persist-filter-settings ()
  "Persist emacspeak filter settings for future sessions."
  (interactive)
  (cl-declare (special emacspeak-speak-filter-persistent-store
                       emacspeak-speak-filter-table))
  (emacspeak--persist-variable
   'emacspeak-speak-filter-table
   emacspeak-speak-filter-persistent-store))

(defun emacspeak-speak-load-filter-settings ()
  "Load emacspeak filter settings."
  (interactive)
  (cl-declare (special emacspeak-speak-filter-persistent-store
                       emacspeak-speak-filter-table
                       emacspeak-speak-filters-loaded-p))
  (unless emacspeak-speak-filters-loaded-p
    ;; `ems--fastload' is defined in `emacspeak-preamble' which requires
    ;; us, so we can't require it at top-level.
    (require 'emacspeak-preamble)
    (declare-function ems--fastload "emacspeak-preamble" (file))
    (ems--fastload emacspeak-speak-filter-persistent-store)
    (setq emacspeak-speak-filters-loaded-p t)
    (add-hook 'kill-emacs-hook 'emacspeak-speak-persist-filter-settings)))

(defun emacspeak-speak-line-set-column-filter (filter)
  "Set up filter for selectively speaking or ignoring portions of lines.
The filter is specified as a list of pairs.
For example, to filter  columns 1 -- 10 and 20 -- 25,
specify filter as
((0 9) (20 25)). Filter settings are persisted across sessions.  A
persisted filter is used as the default when prompting for a filter.
This allows one to accumulate a set of filters for specific files like
/var/adm/messages and /var/adm/maillog over time.
Option emacspeak-speak-line-invert-filter determines
the sense of the filter. "
  (interactive
   (list
    (progn
      (emacspeak-speak-load-filter-settings)
      (read-minibuffer
       (format
        "Specify columns to %s: "
        (if emacspeak-speak-line-invert-filter
            " speak"
          "filter out"))
       (format "%s"
               (if (buffer-file-name)
                   (emacspeak-speak-lookup-persistent-filter 
                    (buffer-file-name))
                 ""))))))
  (cond
   ((and (listp filter)
         (cl-every
          #'(lambda (l)
              (and (listp l)
                   (= 2 (length l))))
          filter))
    (setq emacspeak-speak-line-column-filter filter)
    (when (buffer-file-name)
      (emacspeak-speak-set-persistent-filter (buffer-file-name) filter)))
   (t
    (setq emacspeak-speak-line-column-filter nil))))

;;}}}
;;{{{  Actions

;; Setting value of property 'emacspeak-action to a list
;; of the form (before | after function)
;; function to be executed before or after the unit of text at that
;; point is spoken.
(defvar-local emacspeak-action-mode nil
  "Determines if action mode is active.
Non-nil value means that any function that is set as the
value of property action is executed when the text at that
point is spoken.")

;; Record in the mode line
(or
 (assq 'emacspeak-action-mode minor-mode-alist)
 (setq minor-mode-alist
       (append minor-mode-alist
               '((emacspeak-action-mode " Action")))))

;; Return the appropriate action hook variable that defines actions
;; for this mode.

(defun emacspeak-action-get-action-hook (mode)
  "Retrieve action hook.
Argument MODE defines action mode."
  (intern (format "emacspeak-%s-actions-hook" mode)))

;; Execute action at point
(defun emacspeak-handle-action-at-point (&optional pos)
  "Execute action specified at point."
  (cl-declare (special emacspeak-action-mode))
  (setq pos (or pos (point)))
  (let ((action-spec (get-text-property (point) 'emacspeak-action)))
    (when (and emacspeak-action-mode action-spec)
      (condition-case nil
          (funcall action-spec)
        (error (message "Invalid actionat %s" (point)))))))

(ems-generate-switcher 'emacspeak-toggle-action-mode
                       'emacspeak-action-mode
                       "Toggle state of  Emacspeak  action mode.
Interactive PREFIX arg means toggle  the global default value, and then set the
current local  value to the result.")

;;}}}
;;{{{  Speak units of text

(defun emacspeak-speak-region (start end)
  "Speak region bounded by start and end. "
  (interactive "r")
  (cl-declare (special emacspeak-speak-voice-annotated-paragraphs
                       emacspeak-action-mode))
  (let ((inhibit-modification-hooks t)
        (deactivate-mark nil))
    (when (not emacspeak-speak-voice-annotated-paragraphs)
      (save-restriction
        (narrow-to-region start end)
        (emacspeak-speak-voice-annotate-paragraphs)))
    (when emacspeak-action-mode  (emacspeak-handle-action-at-point))
    (dtk-speak (buffer-substring start end))))

(defconst emacspeak-horizontal-rule "^\\([=_-]\\)\\1+$"
  "Regular expression to match horizontal rules in ascii text.")

(defconst emacspeak-decoration-rule
  "^[ \t!@#$%^&*()<>|_=+/\\,.;:-]+$"
  "Regular expressions to match lines that are purely decorative ascii.")

(defconst emacspeak-unspeakable-rule
  "^[^[:alnum:]]+$"
  "Pattern to match lines of special chars.
This is a regular expression that matches lines containing only
non-alphanumeric characters for the current locale.
emacspeak will generate a tone
instead of speaking such lines when punctuation mode is set
to some.")

(defcustom ems--speak-max-line 512
  "Threshold for determining `long' lines.
Emacspeak will ask for confirmation before speaking lines
that are longer than this length.  This is to avoid accidentally
opening a binary file and torturing the speech synthesizer
with a long string of gibberish."
  :group 'emacspeak
  :type 'number)

(make-variable-buffer-local 'ems--speak-max-line)

(defconst emacspeak-speak-blank-line-regexp
  "^[[:space:]]+$"
  "Pattern that matches white space.")

(ems-generate-switcher 'emacspeak-toggle-audio-indentation
                       'emacspeak-audio-indentation
                       "Toggle state of  Emacspeak  audio indentation.
Interactive PREFIX arg means toggle  the global default value, and then set the
current local  value to the result.
Specifying the method of indentation as `tones'
results in the Dectalk producing a tone whose length is a function of the
line's indentation.  Specifying `speak'
results in the number of initial spaces being spoken.")

(defun emacspeak-speak-line (&optional arg)
  "Speaks current line.  With prefix ARG, speaks the rest of the
line from point.  Negative prefix optional arg speaks from start
of line to point.  Indicates indentation with a spoken message if
audio indentation is on see `emacspeak-toggle-audio-indentation'
bound to \\[emacspeak-toggle-audio-indentation].  Indicates
position of point with an aural highlight if option
`emacspeak-show-point' is on --see command
`emacspeak-toggle-show-point' bound to
\\[emacspeak-toggle-show-point].  Lines that start hidden blocks
of text, e.g.  outline header lines, or header lines of blocks
created by command `emacspeak-hide-or-expose-block' are indicated
with auditory icon ellipses. Presence of additional
presentational overlays (created via property display,
before-string, or after-string) is indicated with auditory icon
`left', `right', or `more' as appropriate.  These can then be
spoken using command \\[emacspeak-speak-overlay-properties]."
  (interactive "P")
  (cl-declare (special
               voice-animate voice-indent linum-mode
               dtk-punctuation-mode dtk-cleanup-repeats
               emacspeak-speak-line-invert-filter
               emacspeak-speak-blank-line-regexp
               ems--speak-max-line emacspeak-show-point
               emacspeak-decoration-rule emacspeak-horizontal-rule
               emacspeak-unspeakable-rule
               emacspeak-audio-indentation))
  (dtk-stop)
  (when (listp arg) (setq arg (car arg)))
  (let* ((inhibit-field-text-motion t)
         (inhibit-read-only t)
         (inhibit-modification-hooks t)
         (icon (get-char-property (point) 'auditory-icon))
         (before (get-char-property (point) 'before-string))
         (after (get-char-property (point) 'after-string))
         (display (get-char-property (point) 'display))
         (start (line-beginning-position))
         (end (line-end-position))
         (line nil)
         (orig (point))
         (dtk-cleanup-repeats
          (cond
           ((and emacspeak-show-point
                 (= ?\) (char-syntax (following-char)))))
           (t dtk-cleanup-repeats)))
         (linenum
          (when
              (or (bound-and-true-p display-line-numbers)
                  (bound-and-true-p linenum-mode))
            (line-number-at-pos)))
         (indent nil))
    ;; determine what to speak based on prefix arg
    (cond
     ((null arg))
     ((> arg 0) (setq start orig))
     (t (setq end orig)))
    (when icon (emacspeak-auditory-icon icon))
    (when emacspeak-show-point
      (emacspeak-auditory-icon
       (cond
        ((bolp) 'left)
        ((eolp) 'right)
        (t 'tick-tick))))
    (setq line
          (if emacspeak-show-point
              (ems-set-pause-temporarily
               orig (1+ orig) 5
               (ems-set-personality-temporarily
                orig (1+ orig) voice-animate
                (buffer-substring start end)))
            (buffer-substring start end)))
    (when (and (null arg) emacspeak-speak-line-column-filter)
      (setq
       line
       (emacspeak-speak-line-apply-column-filter
        line emacspeak-speak-line-invert-filter)))
    (when emacspeak-audio-indentation (setq indent (current-indentation)))
    (when (or (invisible-p end)
              (get-text-property start 'emacspeak-hidden-block))
      (emacspeak-auditory-icon 'ellipses))
    (when (or display before after)
      (emacspeak-auditory-icon
       (cond
        (before 'left)
        (after 'right)
        (t 'more))))
    (cond
     ;; C1..C5
     ((string-equal "" line)
      (dtk-tone 130.8 150 'force))
     ((string-match emacspeak-speak-blank-line-regexp line) ;only white space
      (dtk-tone 261.6 150 'force))
     ((and (not (eq 'all dtk-punctuation-mode))
           (string-match emacspeak-horizontal-rule line))
      (dtk-tone 523.3 150 t))
     ((and (not (eq 'all dtk-punctuation-mode))
           (string-match emacspeak-decoration-rule line))
      (dtk-tone 1047 150 t))
     ((and (not (eq 'all dtk-punctuation-mode))
           (string-match emacspeak-unspeakable-rule line))
      (dtk-tone 2093 150 t))
     (t
      (let*
          ((l (length line))
           (speakable ;; should we speak this line?
            (cond
             ((or selective-display
                  (< l ems--speak-max-line)
                  (get-text-property start 'speak-line))
              t)
             ((y-or-n-p (format "Speak  this  %s long line? " l))
              (setq ems--speak-max-line (1+ l))
              (with-silent-modifications
                (put-text-property start end 'speak-line t))
              t))))
        (when speakable
          (when
              (and (null arg) indent (> indent 0))
            (setq indent
                  (propertize
                   (format "indent %d" indent)
                   'personality voice-indent))
            (setq line (concat indent line)))
          (when linenum
            (setq linenum (format "%d" linenum))
            (setq linenum (propertize linenum 'personality voice-lighten))
            (setq line (concat linenum line)))
          (dtk-speak line)))))))

(defun ems--display-props-get ()
  "Return  speakable display, before-string or after-string property if any."
  (let ((before (get-char-property (point) 'before-string))
        (after (get-char-property (point) 'after-string))
        (display (get-char-property (point) 'display))
        (result nil))
    (setq result
          (concat
           (when (stringp display) display)
           (when (stringp before) before)
           (when (stringp after) after)))
    result))

(defun emacspeak-speak-overlay-properties ()
  "Speak display, before-string or after-string property if any."
  (interactive)
  (let ((icon
         (cond
          ((get-char-property (point) 'before-string) 'left)
          ((get-char-property (point) 'after-string) 'right)
          ((get-char-property (point) 'display) 'more)))
        (result (ems--display-props-get)))
    (cond
     ((or (null result) (= 0 (length result)))
      (emacspeak-auditory-icon 'warn-user)
      (message "No speakable overlay properties here."))
     (t
      (emacspeak-auditory-icon icon)
      (dtk-speak result)))))

(defun emacspeak-speak-visual-line ()
  "Speaks current visual line.
Cues the start of a physical line with auditory icon `left'."
  (interactive)
  (cl-declare (special  emacspeak-show-point))
  (let ((inhibit-field-text-motion t)
        (inhibit-read-only t)
        (start nil)
        (end nil)
        (inhibit-modification-hooks t)
        (line nil)
        (orig (point)))
    (cond
     ((looking-at "^ *") (emacspeak-auditory-icon 'left))
     ((looking-at " *$") (emacspeak-auditory-icon 'right)))
    (save-excursion
      (beginning-of-visual-line)
      (setq start (point))
      (end-of-visual-line)
      (setq end (point))
      (setq line
            (if emacspeak-show-point
                (ems-set-personality-temporarily
                 orig (1+ orig)
                 voice-animate (buffer-substring start end))
              (buffer-substring start end)))
      (dtk-speak line))))

(defvar-local emacspeak-speak-last-spoken-word-position nil
  "Records position of the last word spoken  .
Local to each buffer.  Used to decide if we  spell or speak the word. ")

(defun emacspeak-speak-spell-word (word)
  "Spell WORD."
  (cl-declare (special voice-animate))
  (let ((result "")
        (char-string ""))
    (cl-loop for char across word
             do
             (setq char-string (format "%c " char))
             (when (and (<= ?A char)
                        (<= char ?Z))
               (put-text-property 0 1
                                  'personality voice-animate
                                  char-string)
               (setq char-string (format "cap %s " char-string)))
             (setq result
                   (concat result
                           char-string)))
    (dtk-speak result)))

(defun emacspeak-speak-spell-current-word ()
  "Spell word at  point."
  (interactive)
  (emacspeak-speak-spell-word (word-at-point)))

(defun emacspeak-speak-word (&optional arg)
  "Speak current word.
With prefix ARG, speaks the rest of the word from point.
Negative prefix arg speaks from start of word to point.
If executed  on the same buffer position a second time, the word is
spelled out  instead of being spoken."
  (interactive "P")
  (cl-declare (special emacspeak-speak-last-spoken-word-position
                       emacspeak-action-mode))
  (when (listp arg) (setq arg (car arg)))
  (when emacspeak-action-mode  (emacspeak-handle-action-at-point))
  (save-excursion
    (let ((orig (point))
          (inhibit-modification-hooks t)
          (inhibit-field-text-motion  t)
          (start nil)
          (end nil)
          (speaker 'dtk-speak))
      (forward-word 1)
      (setq end (point))
      (backward-word 1)
      (setq start (min orig (point)))
      (cond
       ((null arg))
       ((> arg 0) (setq start orig))
       ((< arg 0) (setq end orig)))
      ;; select speak or spell
      (cond
       ((and (called-interactively-p 'interactive)
             (eq emacspeak-speak-last-spoken-word-position orig))
        (setq speaker 'emacspeak-speak-spell-word)
        (setq emacspeak-speak-last-spoken-word-position nil))
       (t (setq emacspeak-speak-last-spoken-word-position orig)))
      (funcall speaker (buffer-substring start end)))))

(defun emacspeak-is-alpha-p (c)
  "Check if `C' is an alphabetic char."
  (and (= ?w (char-syntax c))
       (dtk-unicode-char-untouched-p c)))

;;{{{  phonemic table

(defvar emacspeak-char-to-phonetic-table
  '(
    ("1" . "one")
    ("2" . "two")
    ("3" . "three")
    ("4" . "four")
    ("5" . "five")
    ("6" . "six")
    ("7" . "seven")
    ("8" . "eight")
    ("9" . "nine")
    ("0" .  "zero")
    ("a" . "alpha")
    ("b" . "bravo")
    ("c" . "charlie")
    ("d" . "delta")
    ("e" . "echo")
    ("f" . "foxtrot")
    ("g" . "golf")
    ("h" . "hotel")
    ("i" . "india")
    ("j" . "juliet")
    ("k" . "kilo")
    ("l" . "lima")
    ("m" . "mike")
    ("n" . "november")
    ("o" . "oscar")
    ("p" . "poppa")
    ("q" . "quebec")
    ("r" . "romeo")
    ("s" . "sierra")
    ("t" . "tango")
    ("u" . "uniform")
    ("v" . "victor")
    ("w" . "whisky")
    ("x" . "xray")
    ("y" . "yankee")
    ("z" . "zulu")
    ("A" . "cap alpha")
    ("B" . "cap bravo")
    ("C" . "cap charlie")
    ("D" . "cap delta")
    ("E" . "cap echo")
    ("F" . "cap foxtrot")
    ("G" . "cap golf")
    ("H" . "cap hotel")
    ("I" . "cap india")
    ("J" . "cap juliet")
    ("K" . "cap kilo")
    ("L" . "cap lima")
    ("M" . "cap mike")
    ("N" . "cap november")
    ("O" . "cap oscar")
    ("P" . "cap poppa")
    ("Q" . "cap quebec")
    ("R" . "cap romeo")
    ("S" . "cap sierra")
    ("T" . "cap tango")
    ("U" . "cap uniform")
    ("V" . "cap victor")
    ("W" . "cap whisky")
    ("X" . "cap xray")
    ("Y" . "cap yankee")
    ("Z" . "cap zulu"))
  "Mapping from characters to their phonemic equivalents.")

(defun emacspeak-get-phonetic-string (char)
  "Return the phonetic string for this CHAR or its upper case equivalent.
char is assumed to be one of a--z."
  (cl-declare (special emacspeak-char-to-phonetic-table))
  (let ((char-string (char-to-string char)))
    (or (cdr
         (assoc char-string emacspeak-char-to-phonetic-table))
        (dtk-unicode-full-name-for-char char)
        char-string)))

;;}}}
;;{{{ Speak Chars:

(defun emacspeak-speak-this-char (char)
  "Speak this CHAR."
  (when char
    (cond
     ((emacspeak-is-alpha-p char) (dtk-letter (char-to-string char)))
     ((> char 128) (emacspeak-speak-char-name char))
     (t (dtk-dispatch (dtk-char-to-speech char))))))
(defun emacspeak-speak-char (&optional prefix)
  "Speak character under point.
Pronounces character phonetically unless  called with a PREFIX arg."
  (interactive "P")
  (let ((char (following-char))
        (display (get-char-property (point) 'display))
        (icon (get-char-property (point) 'auditory-icon)))
    (when icon (emacspeak-auditory-icon icon))
    (when display
      (emacspeak-auditory-icon 'ellipses)
      (and (listp display) (message "%s" (car display))))
    (when char
      (cond
       ((stringp display) (dtk-speak display))
       ((> char 128) (emacspeak-speak-char-name char))
       ((and (not prefix)
             (emacspeak-is-alpha-p char))
        (dtk-speak (emacspeak-get-phonetic-string char)))
       (t (emacspeak-speak-this-char char))))))

(defun emacspeak-speak-preceding-char ()
  "Speak character before point."
  (interactive)
  (let ((char (preceding-char))
        (display (get-char-property (1- (point)) 'display)))
    (when char
      (cond
       ((stringp display) (dtk-speak display))
       ((> char 128) (emacspeak-speak-char-name char))
       (t (emacspeak-speak-this-char char))))))

(defun emacspeak-speak-char-name (char)
  "tell me what this is"
  (interactive)
  (dtk-speak (dtk-unicode-name-for-char char)))

;;}}}
(defun emacspeak-speak-sentence (&optional arg)
  "Speak current sentence.
With prefix ARG, speaks the rest of the sentence  from point.
Negative prefix arg speaks from start of sentence to point."
  (interactive "P")
  (cl-declare (special emacspeak-action-mode))
  (when (listp arg) (setq arg (car arg)))
  (save-excursion
    (let ((orig (point))
          (inhibit-modification-hooks t)
          (start nil)
          (end nil))
      (forward-sentence 1)
      (setq end (point))
      (backward-sentence 1)
      (setq start (point))
      (when emacspeak-action-mode  (emacspeak-handle-action-at-point))
      (cond
       ((null arg))
       ((> arg 0) (setq start orig))
       ((< arg 0) (setq end orig)))
      (dtk-speak (buffer-substring start end)))))

(defun emacspeak-speak-sexp (&optional arg)
  "Speak current sexp.
With prefix ARG, speaks the rest of the sexp  from point.
Negative prefix arg speaks from start of sexp to point. "
  (interactive "P")
  (when (listp arg) (setq arg (car arg)))
  (save-excursion
    (let ((orig (point))
          (inhibit-modification-hooks t)
          (start nil)
          (end nil))
      (condition-case nil
          (forward-sexp 1)
        (error nil))
      (setq end (point))
      (condition-case nil
          (backward-sexp 1)
        (error nil))
      (setq start (point))
      (cond
       ((null arg))
       ((> arg 0) (setq start orig))
       ((< arg 0) (setq end orig)))
      (emacspeak-auditory-icon 'select-object)
      (dtk-speak (buffer-substring start end)))))

(defun emacspeak-speak-page (&optional arg)
  "Speak a page.
With prefix ARG, speaks rest of current page.
Negative prefix arg will read from start of current page to point. "
  (interactive "P")
  (cl-declare (special emacspeak-action-mode))
  (when (listp arg) (setq arg (car arg)))
  (save-excursion
    (let ((orig (point))
          (start nil)
          (end nil))
      (mark-page)
      (setq start (point))
      (when emacspeak-action-mode  (emacspeak-handle-action-at-point))
      (setq end (mark))
      (cond
       ((null arg))
       ((> arg 0) (setq start orig))
       ((< arg 0) (setq end orig)))
      (dtk-speak (buffer-substring start end)))))

(defun emacspeak-speak-paragraph (&optional arg)
  "Speak paragraph.
With prefix arg, speaks rest of current paragraph.
Negative prefix arg will read from start of current paragraph to point. "
  (interactive "P")
  (cl-declare (special emacspeak-action-mode))
  (when (listp arg) (setq arg (car arg)))
  (save-excursion
    (let ((orig (point))
          (start nil)
          (end nil))
      (forward-paragraph 1)
      (setq end (point))
      (backward-paragraph 1)
      (setq start (point))
      (when emacspeak-action-mode  (emacspeak-handle-action-at-point))
      (cond
       ((null arg))
       ((> arg 0) (setq start orig))
       ((< arg 0) (setq end orig)))
      (dtk-speak (buffer-substring start end)))))

;;}}}
;;{{{  Speak buffer objects such as help, completions minibuffer etc

(defun emacspeak-speak-buffer (&optional arg)
  "Speak current buffer  contents.
With prefix ARG, speaks the rest of the buffer from point.
Negative prefix arg speaks from start of buffer to point. "
  (interactive "P")
  (cl-declare (special emacspeak-speak-voice-annotated-paragraphs))
  (let () (when (not emacspeak-speak-voice-annotated-paragraphs)
      (emacspeak-speak-voice-annotate-paragraphs))
    (when (listp arg) (setq arg (car arg)))
    (dtk-stop)
    (let ((start nil)
          (end nil))
      (cond
       ((null arg)
        (setq start (point-min)
              end (point-max)))
       ((> arg 0)
        (setq start (point)
              end (point-max)))
       (t (setq start (point-min)
                end (point))))
      (dtk-speak (buffer-substring start end)))))

(defun emacspeak-speak-other-buffer (buffer)
  "Speak specified buffer.
Useful to listen to a buffer without switching  contexts."
  (interactive
   (list
    (read-buffer "Speak buffer: "
                 nil t)))
  (save-current-buffer
    (set-buffer buffer)
    (emacspeak-speak-buffer)))

(defsubst emacspeak-speak-rest-of-buffer ()
  "Speak remainder of the buffer starting at point"
  (interactive)
  (emacspeak-auditory-icon 'select-object)
  (emacspeak-speak-buffer 1))

(defun emacspeak-speak-help (&optional arg)
  "Speak help buffer if one present.
With prefix arg, speaks the rest of the buffer from point.
Negative prefix arg speaks from start of buffer to point."
  (interactive "P")
  (let ((help-buffer (get-buffer "*Help*")))
    (cond
     (help-buffer
      (emacspeak-auditory-icon 'help)
      (save-current-buffer
        (set-buffer help-buffer)
        (emacspeak-speak-buffer arg)))
     (t (emacspeak-auditory-icon 'button)
        (dtk-speak "First ask for help")))))

(defun emacspeak-get-current-completion ()
  "Return the completion string under point in the *Completions* buffer."
  (let (beg end)
    (if (and (not (eobp)) (get-text-property (point) 'mouse-face))
        (setq end (point) beg (1+ (point))))
    (if (and (not (bobp)) (get-text-property (1- (point)) 'mouse-face))
        (setq end (1- (point)) beg (point)))
    (if (null beg)
        (error "No current  completion "))
    (setq beg (or
               (previous-single-property-change beg 'mouse-face)
               (point-min)))
    (setq end (or (next-single-property-change end 'mouse-face) (point-max)))
    (buffer-substring beg end)))

;;}}}
;;{{{ mail check

(defcustom emacspeak-mail-spool-file
  (expand-file-name
   (user-login-name)
   (if (boundp 'rmail-spool-directory)
       rmail-spool-directory
     "/usr/spool/mail/"))
  "Mail spool file examined  to alert you about newly
arrived mail."
  :type '(choice
          (const :tag "None" nil)
          (file :tag "Mail drop location"))
  :group 'emacspeak)

(defsubst emacspeak-get-file-size (filename)
  "Return file size for file FILENAME."
  (or (nth 7 (file-attributes filename)) 0))

(defvar emacspeak-mail-last-alerted-time (list 0 0)
  "Least  significant 16 digits of the time when mail alert was last issued. ")

(defun emacspeak-mail-get-last-mail-arrival-time (f)
  "Return time when mail  last arrived."
  (if (file-exists-p f)
      (nth 5 (file-attributes f))
    0))

(defcustom emacspeak-mail-alert-interval 300
  "Interval in seconds between mail alerts for the same pending
  message."
  :type 'integer
  :group 'emacspeak)

(defun emacspeak-mail-alert-user-p (f)
  "Predicate to check if we need to play an alert for the specified spool."
  (cl-declare (special emacspeak-mail-last-alerted-time
                       emacspeak-mail-alert-interval))
  (let* ((mod-time (emacspeak-mail-get-last-mail-arrival-time f))
         (size (emacspeak-get-file-size f))
         (result 
          (and (> size 0)
               (or
                (null emacspeak-mail-last-alerted-time)
                (time-less-p emacspeak-mail-last-alerted-time mod-time)
                (time-less-p            ;unattended mail
                 (time-add emacspeak-mail-last-alerted-time
                           (list 0 emacspeak-mail-alert-interval))
                 (current-time))))))
    (when result
      (setq emacspeak-mail-last-alerted-time (current-time)))
    result))

(defun emacspeak-mail-alert-user ()
  "Alerts user about the arrival of new mail."
  (cl-declare (special emacspeak-mail-spool-file))
  (when (and emacspeak-mail-spool-file
             (emacspeak-mail-alert-user-p emacspeak-mail-spool-file))
    (emacspeak-auditory-icon 'new-mail)))

(defcustom emacspeak-mail-alert t
  " If t, emacspeak will alert you about newly arrived mail
with an auditory icon when
displaying the mode line.
You can use command
`emacspeak-toggle-mail-alert' bound to
\\[emacspeak-toggle-mail-alert] to set this option. "
  :group 'emacspeak
  :type 'boolean)

(ems-generate-switcher 'emacspeak-toggle-mail-alert
                       'emacspeak-mail-alert
                       "Toggle state of  Emacspeak  mail alert.
Interactive PREFIX arg means toggle  the global default value, and then set the
current local  value to the result.
Turning on this option results in Emacspeak producing an auditory icon
indicating the arrival  of new mail when displaying the mode line.")

;;}}}
;;{{{ Mode line info collectors 

(defsubst emacspeak-get-voicefied-recursion-info (level)
  "Return voicefied version of this recursive-depth level."
  (cond
   ((zerop level) nil)
   (t
    (propertize
     (format " Recursive Edit %d " level) 'personality voice-smoothen))))

(defsubst emacspeak-get-voicefied-frame-info (frame)
  "Return voicefied version of this frame name."
  (cond
   ((= (length (frame-list)) 1) nil)
   (t
    (propertize
     (format " %s " (frame-parameter frame 'name))
     'personality voice-lighten-extra ))))

;;}}}
;;{{{  Speak mode line information

;; compute current line number
(defsubst emacspeak-get-current-line-number ()
  (let ((start (point)))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (+ 1 (count-lines start (point)))))))

(defun emacspeak-speak-which-function ()
  "Speak which function we are on.  Uses which-function from
which-func without turning that mode on.  "
  (interactive)
  (require 'which-func)
  (message (or (which-function) "Not inside a function.")))

(defun emacspeak-speak-buffer-info ()
  "Speak buffer information."
  (message "Buffer has %s lines and %s characters %s "
           (count-lines (point-min) (point-max))
           (- (point-max) (point-min))
           (if (= 1 (point-min))
               ""
             "with narrowing in effect. ")))
(voice-setup-set-voice-for-face 'header-line 'voice-bolden)

(defun emacspeak--sox-multiwindow ()
  "Use `window-edges' and plays a sound cuew."
  (let ((corners (window-edges))
        (tr 0)
        (mr (/ (frame-height) 2))
        (br (1- (frame-height)))
        (lc 0)
        (mc (/ (frame-width) 2))
        (rc (frame-width)))
    (cond
     ((equal corners `(,lc ,tr ,mc ,br))
      (sox-multiwindow 1 2 "pluck")
      'left-half)
     ((equal corners `(,mc ,tr ,rc ,br))
      (sox-multiwindow 1 2  "pluck")
      'right-half)
     ((equal corners `(,lc ,tr ,rc ,mr))
      (sox-multiwindow nil 2)
      'top-half)
     ((equal corners `(,lc ,mr ,rc ,br))
      (sox-multiwindow nil 1.3)
      'bottom-half)
     ((equal corners `(,lc ,tr ,mc ,mr))
      (sox-multiwindow nil 2.5)
      'top-left)
     ((equal corners `(,mc ,tr ,rc ,mr))
      (sox-multiwindow t 2.5)
      'top-right)
     ((equal corners `(,lc ,mr ,mc ,br))
      (sox-multiwindow nil 0.9)
      'bottom-left)
     ((equal corners `(,mc ,mr ,rc ,br))
      (sox-multiwindow 'swap 0.9)
      'bottom-right)
     ((and (zerop (cl-first corners))
           (zerop (cl-second corners))
           (= rc (cl-third corners)))
      (sox-multiwindow nil 2)
      'top-half)
     ((and (zerop (cl-first corners))
           (= rc (cl-third corners))
           (= br (cl-fourth corners)))
      (sox-multiwindow nil 1.3)
      'bottom-half)
     ((and (zerop (cl-first corners))
           (zerop (cl-second corners))
           (= br (cl-fourth corners)))
      (sox-multiwindow)
      'left-half)
     ((and (zerop (cl-second corners))
           (= rc (cl-third corners))
           (= br (cl-fourth corners)))
      (sox-multiwindow 'swap)
      'right-half)
     (t ""))))

(defun emacspeak-speak-mode-line (&optional buffer-info)
  "Speak the mode-line.
Speaks header-line if that is set when called non-interactively.
Interactive prefix arg speaks buffer info."
  (interactive "P")
  (cl-declare (special mode-name major-mode vc-mode
                       global-visual-line-mode visual-line-mode
                       header-line-format global-mode-string
                       folding-mode column-number-mode line-number-mode
                       emacspeak-mail-alert mode-line-format))
  (with-current-buffer (window-buffer (selected-window))
    (force-mode-line-update)
    (when (bound-and-true-p folding-mode) (emacspeak-auditory-icon 'ellipses))
    (when (and visual-line-mode (not global-visual-line-mode)) (sox-chime 2 2))
    (when emacspeak-mail-alert (emacspeak-mail-alert-user))
    (cond
     ((and header-line-format (not (called-interactively-p 'interactive)))
      (emacspeak-speak-header-line))
     (buffer-info (emacspeak-speak-buffer-info))
     (t                                 ; main branch
      (let ((global-info (downcase (format-mode-line global-mode-string)))
            (window-count (length (window-list)))
            (vc-state 
             (when (and vc-mode (buffer-file-name))
               (vc-state (buffer-file-name))))
            (frame-info (emacspeak-get-voicefied-frame-info (selected-frame)))
            (recursion-info 
             (emacspeak-get-voicefied-recursion-info (recursion-depth)))
            (dir-info
             (when (or (eq major-mode 'shell-mode)
                       (eq major-mode 'comint-mode))
               (abbreviate-file-name default-directory))))
        (when (> window-count 1) (emacspeak--sox-multiwindow))
        (setq window-count ;;; int->string
              (if (> window-count 1) (format " %s " window-count) nil))
        (cond
         ((stringp mode-line-format) (dtk-speak (downcase mode-line-format)))
         (t                             ;process modeline
          (unless (zerop (length global-info))
            (put-text-property
             0 (length global-info) 'personality voice-bolden-medium
             global-info))
          ;;; avoid pathological case
          (unless (and buffer-read-only (buffer-modified-p)) 
            (when (and buffer-file-name (buffer-modified-p))
              (emacspeak-auditory-icon 'modified-object))
            (when buffer-read-only
              (emacspeak-auditory-icon 'unmodified-object)))
          (tts-with-punctuations
           'all
           (dtk-speak
            (concat
             dir-info
             (propertize (buffer-name) 'personality voice-lighten-medium)
             (when window-count 
               (propertize window-count 'personality voice-smoothen))
             (when vc-mode 
               (propertize (downcase vc-mode) 'personality voice-smoothen))
             (when vc-state (format " %s " vc-state))
             (when line-number-mode
               (format "line %d" (emacspeak-get-current-line-number)))
             (when column-number-mode
               (format "column %d" (current-column)))
             (propertize
              (downcase
               (format-mode-line mode-name)) 'personality voice-animate)
             (emacspeak-get-current-percentage-verbously) 
             global-info frame-info recursion-info))))))))))

(defun emacspeak-speak-current-buffer-name ()
  "Speak name of current buffer."
  (tts-with-punctuations 'all
                         (dtk-speak
                          (buffer-name))))


(defcustom emacspeak-speak-show-volume nil
  "Show volume as part of minor-mode-line."
  :type 'boolean
  :group 'emacspeak-speak)

(defconst ems--vol-cmd
  (eval-when-compile
   (concat
    "pacmd list-sinks | grep -A 8 '  \\* index' | grep volume"
    "|  cut -d ',' -f 1"
    "| cut -d ':' -f 3"
    "| cut -d '/' -f 2"))
  "Shell pipeline for getting volume.")

(defsubst ems--show-current-volume ()
  "volume display in minor-mode-line"
  (cl-declare (special ems--vol-cmd))
  (cond
    ((executable-find "pactl")
     (propertize 
      (format
       " 🎧 %s"
       (substring
        (string-trim (shell-command-to-string ems--vol-cmd))
        0 -1))
      'personality 'voice-bolden))
    (t "")))

(defun emacspeak-speak-minor-mode-line (&optional log-msg)
  "Speak the minor mode-information.
Optional interactive prefix arg `log-msg' logs spoken info to
*Messages*."
  (interactive "P")
  (cl-declare (special minor-mode-alist))
  (let ((info (format-mode-line minor-mode-alist)))
    (when log-msg (ems--log-message info))
    (tts-with-punctuations 'some
     (dtk-speak  info))))

(defun emacspeak-speak-buffer-filename (&optional filename)
  "Speak name of file being visited in current buffer.
Speak default directory if invoked in a dired buffer, or when the
buffer is not visiting any file.  Interactive prefix arg
`filename' speaks only the final path component.  The result is
put in the kill ring for convenience."
  (interactive "P")
  (let ((dtk-caps t)
        (location (or (buffer-file-name) default-directory)))
    (when filename
      (setq location (file-name-nondirectory location)))
    (kill-new location)
    (dtk-speak location)))

;;}}}
;;{{{ Speak header-line

(defvar emacspeak-use-header-line t
  "Use default header line defined  by Emacspeak for buffers that
dont customize the header.")

(defvar emacspeak-header-line-format
  '((:eval (buffer-name)))
  "Default header-line-format defined by Emacspeak.
Displays name of current buffer.")

(defun emacspeak-speak-header-line ()
  "Speak header line if set."
  (interactive)
  (cl-declare (special header-line-format))
  (cond
   (header-line-format
    (let ((window-count (length (window-list))))
      (emacspeak-auditory-icon 'item)
      (when (> window-count 1) (emacspeak--sox-multiwindow))
      (dtk-speak (format-mode-line header-line-format))))
   (t (dtk-speak "No header line."))))

(defun emacspeak-toggle-header-line ()
  "Toggle Emacspeak's default header line."
  (interactive)
  (cl-declare (special emacspeak-header-line-format
                       header-line-format))
  (if header-line-format
      (setq header-line-format nil)
    (setq header-line-format emacspeak-header-line-format))
  (emacspeak-auditory-icon (if header-line-format 'on 'off))
  (message "Turned %s default header line."
           (if header-line-format 'on 'off)))

;;}}}
;;{{{  Speak text without moving point

;; Functions to browse without moving:
(defun emacspeak-read-line-internal (arg)
  "Read a line without moving.
Line to read is specified relative to the current line, prefix args gives the
offset. Default  is to speak the previous line. "
  (save-excursion
    (cond
     ((zerop arg) (emacspeak-speak-line))
     ((zerop (forward-line arg))
      (emacspeak-speak-line))
     (t (dtk-speak "Not that many lines in buffer ")))))

(defun emacspeak-read-previous-line (&optional arg)
  "Read previous line, specified by an offset, without moving.
Default is to read the previous line. "
  (interactive "p")
  (emacspeak-read-line-internal (- (or arg 1))))

(defun emacspeak-read-next-line (&optional arg)
  "Read next line, specified by an offset, without moving.
Default is to read the next line. "
  (interactive "p")
  (emacspeak-read-line-internal (or arg 1)))

(defun emacspeak-read-word-internal (arg)
  "Read a word without moving.
word  to read is specified relative to the current word, prefix args gives the
offset. Default  is to speak the previous word. "
  (save-excursion
    (cond
     ((= arg 0) (emacspeak-speak-word))
     ((forward-word arg)
      (skip-syntax-forward " ")
      (emacspeak-speak-word 1))
     (t (dtk-speak "Not that many words ")))))

;;}}}
;;{{{  Speak misc information e.g. time, version, current-kill  etc

(defcustom emacspeak-speak-time-format-string
  "%H:%M   on %A, %B %_e, %Y "
  "Format string that specifies how the time should be spoken.
See the documentation for function
`format-time-string'"
  :group 'emacspeak
  :type 'string)

(defcustom emacspeak-speak-zoneinfo-directory
  "/usr/share/zoneinfo/"
  "Directory containing timezone data."
  :type 'directory
  :group 'emacspeak)

(defun emacspeak-speak-world-clock (zone &optional set)
  "Display current date and time  for specified zone.
Optional second arg `set' sets the TZ environment variable as well."
  (interactive
   (list
    (let ((completion-ignore-case t)
          (ido-case-fold t)
          (read-file-name-completion-ignore-case t))
      (read-file-name "Timezone: " emacspeak-speak-zoneinfo-directory))
    current-prefix-arg))
  (cl-declare (special emacspeak-speak-time-format-string
                       ido-case-fold emacspeak-speak-zoneinfo-directory))
  (when (and set
             (= 16 (car set)))
    ;; two interactive prefixes from caller
    (setenv "TZ" zone))
  (emacspeak-shell-command
   (format "export TZ=%s; date +\"%s\""
           zone
           (concat emacspeak-speak-time-format-string
                   (format
                    " in %s, %%Z, %%z "
                    (substring 
                     zone 
                     (length emacspeak-speak-zoneinfo-directory)))))))

(defun emacspeak-speak-time (&optional world)
  "Speak the time.
Spoken time  is available via \\[emacspeak-view-notifications].
Optional interactive prefix arg `C-u'invokes world clock.
Timezone is specified using minibuffer completion.
Second interactive prefix sets clock to new timezone."
  (interactive "P")
  (cl-declare (special emacspeak-speak-time-format-string))
  (emacspeak-auditory-icon 'time)
  (cond
   (world (call-interactively 'emacspeak-speak-world-clock))
   (t
    (let ((time-string
           (format-time-string emacspeak-speak-time-format-string
                               (current-time) (getenv "TZ"))))
      (tts-with-punctuations 'some (dtk-notify-speak time-string))))))

(defun emacspeak-speak-seconds-since-epoch (seconds)
  "Speaks time value specified as seconds  since epoch."
  (interactive
   (list (read-minibuffer "Seconds: " (word-at-point))))
  (cl-declare (special emacspeak-speak-time-format-string))
  (message
   (format-time-string
    emacspeak-speak-time-format-string (seconds-to-time seconds))))

(defun emacspeak-speak-microseconds-since-epoch (ms)
  "Speaks time value specified as microseconds  since epoch."
  (interactive
   (list (read-minibuffer "MicroSeconds: " (word-at-point))))
  (let ((seconds (/ ms 1000000)))
    (emacspeak-speak-seconds-since-epoch seconds)))

(defun emacspeak-speak-milliseconds-since-epoch (ms)
  "Speaks time value specified as milliseconds  since epoch.."
  (interactive
   (list (read-minibuffer "MilliSeconds: " (word-at-point))))
  (let ((seconds (/ ms 1000)))
    (emacspeak-speak-seconds-since-epoch seconds)))

(defun emacspeak-speak-date-as-seconds (time)
  "Read time value as a human-readable string, return seconds.
Seconds value is also placed in the kill-ring."
  (interactive "sTime: ")
  (let ((result (float-time (apply 'encode-time (parse-time-string time)))))
    (message "%s" result)
    (kill-new result)
    result))

;;}}}
;;{{{ Codenames etc.
(defvar emacspeak-codename
  (propertize "Tilden" 'face 'bold)
  "Code name of present release.")

(defun emacspeak-setup-get-revision ()
  "Get SHA checksum of current revision that is suitable for spoken output."
  (let ((default-directory emacspeak-directory))
    (if (and (executable-find "git")
             (file-exists-p (expand-file-name ".git" emacspeak-directory)))
        (propertize
         (shell-command-to-string "git show -s --pretty=format:%h HEAD ")
         'personality voice-smoothen)
      "")))

(defvar emacspeak-version
  (concat "57.0,   " emacspeak-codename)
  "Version number for Emacspeak.")

(defun emacspeak-speak-version (&optional speak-rev)
  "Announce version information for running emacspeak.
Optional interactive prefix arg `speak-rev' speaks only the Git revision."
  (interactive "P")
  (cl-declare (special emacspeak-version emacspeak-sounds-directory
                       emacspeak-m-player-program
                       emacspeak-use-auditory-icons))
  (let ((signature "Emacspeak "))
    (when
        (and (null speak-rev) emacspeak-use-auditory-icons
             emacspeak-m-player-program)
      (start-process
       "mp3" nil "mplayer"
       (expand-file-name "emacspeak.mp3" emacspeak-sounds-directory)))
    (tts-with-punctuations
     'some
     (dtk-speak-and-echo
      (concat
       signature
       (if speak-rev
           (emacspeak-setup-get-revision)
         (concat emacspeak-version " " (emacspeak-setup-get-revision))))))))

(defun emacspeak-speak-current-kill (&optional count)
  "Speak the current kill.
This is what will be yanked by the next \\[yank]. Prefix numeric
arg, COUNT, specifies that the text that will be yanked as a
result of a \\[yank] followed by count-1 \\[yank-pop] be
spoken. The kill number that is spoken says what numeric prefix
arg to give to command yank."
  (interactive "p")
  (let ((context
         (format "kill %s "
                 (if current-prefix-arg (+ 1 count) 1))))
    (put-text-property 0 (length context) 'personality voice-annotate context)
    (dtk-speak 
     (concat context (current-kill (if current-prefix-arg count 0) t)))))

(defun emacspeak-zap-tts ()
  "Send this command to the TTS directly."
  (interactive)
  (dtk-dispatch
   (read-from-minibuffer "Enter TTS command string: ")))

(defun emacspeak-speak-string-to-phone-number (string)
  "Convert alphanumeric phone number to true phone number.
Argument STRING specifies the alphanumeric phone number."
  (setq string (downcase string))
  (let ((i 0))
    (cl-loop for character across string
             do
             (aset string i
                   (cl-case character
                     (?a ?2)
                     (?b ?2)
                     (?c ?2)
                     (?d ?3)
                     (?e ?3)
                     (?f ?3)
                     (?g ?4)
                     (?h ?4)
                     (?i ?4)
                     (?j ?5)
                     (?k ?5)
                     (?l ?5)
                     (?m ?6)
                     (?n ?6)
                     (?o ?6)
                     (?p ?7)
                     (?r ?7)
                     (?s ?7)
                     (?t ?8)
                     (?u ?8)
                     (?v ?8)
                     (?w ?9)
                     (?x ?9)
                     (?y ?9)
                     (?q ?1)
                     (?z ?1)
                     (otherwise character)))
             (cl-incf i))
    string))

;;}}}
;;{{{ speaking marks

;; Intelligent mark feedback for emacspeak:
;; 

(defun emacspeak-speak-current-mark (count)
  "Speak the line containing the mark.
With no argument, speaks the line containing the mark--this is
where \\[exchange-point-and-mark] would
jump.  Numeric prefix arg  `COUNT' speaks line containing mark  `n'
where  `n' is one less than the number of times one has to jump
using `set-mark-command' to get to this marked position.  The
location of the mark is indicated by an aural highlight. "
  (interactive "p")
  (unless (mark) (error "No marks set in this buffer"))
  (when (and current-prefix-arg (> count (length mark-ring)))
    (error "Not that many marks in this buffer"))
  (let ((line nil)
        (pos nil)
        (context
         (format "mark %s " (if current-prefix-arg count 0))))
    (put-text-property 0 (length context)
                       'personality voice-annotate context)
    (setq pos
          (if current-prefix-arg
              (elt mark-ring (1- count))
            (mark)))
    (save-excursion
      (goto-char pos)
      (ems-set-personality-temporarily
       pos (1+ pos) voice-animate
       (setq line (ems--this-line)))
      (dtk-speak
       (concat context line)))))

;;}}}
;;{{{ speaking personality chunks

;; Block navigation

;;}}}
;;{{{Face Ranges:

(defun emacspeak-speak-face-browse ()
  "Use C-f and C-b to browse by current face."
  (interactive )
  (call-interactively #'emacspeak-speak-range)
  (while t
    (let ((key (read-key-sequence "")))
      (cond
       ((string= key "\C-f")
        (funcall-interactively #'emacspeak-speak-face-forward))
       ((string= key "\C-b")
        (funcall-interactively #'emacspeak-speak-face-backward))
       (t (keyboard-quit))))))

(defun emacspeak-speak-range (&optional prop)
  "Speak and return  range at point"
  (interactive )
  (setq prop (or prop 'face))
  (let*
      ((start (previous-single-property-change (1+ (point)) prop))
       (pre-start (previous-single-property-change (point) prop))
       (end (next-single-property-change (point) prop))
       (beg (or start pre-start)))
    (when (and  beg end)
      (emacspeak-speak-region beg end)
      (buffer-substring beg end))))

(defun emacspeak-speak-face-forward ()
  "Property search for face --- see \\[text-property-search-forward]"
  (interactive)
  (when-let
      ((match
        (funcall-interactively
         #'text-property-search-forward
         'face (get-text-property (point) 'face)
         t t)))
    (goto-char (prop-match-beginning match))))

(defun emacspeak-speak-face-backward ()
  "Property search for face at point see \\[text-property-search-backward]"
  (interactive)
  (when-let
      ((match
        (funcall-interactively
         #'text-property-search-backward
         'face (get-text-property (point) 'face) t t)))
    (goto-char (prop-match-beginning match))))

;;}}}
;;{{{  Execute command repeatedly:

(defvar emacspeak-execute-repeatedly-key 32
  "Key to use to repeat command.")

(defun emacspeak-execute-repeatedly (command)
  "Execute COMMAND repeatedly."
  (interactive (list (read-command "Command to execute repeatedly:")))
  (cl-declare (special emacspeak-execute-repeatedly-key))
  (let ((key "")
        (pos (point))
        (continue t)
        (message (format "Press %s to execute %s again"
                         (if (= 32 emacspeak-execute-repeatedly-key)
                             "space"
                           (char-to-string emacspeak-execute-repeatedly-key))
                         command)))
    (while continue
      (call-interactively command)
      (cond
       ((= (point) pos) (setq continue nil))
       (t (setq pos (point))
          (setq key (read-key-sequence message))
          (when (and (stringp key)
                     (not 
                      (= emacspeak-execute-repeatedly-key
                         (string-to-char key))))
            (dtk-stop)
            (setq continue nil)))))
    (dtk-speak "Exited continuous mode ")))

(defun emacspeak-speak-continuously ()
  "Speak a buffer continuously.
First prompts using the minibuffer for the kind of action to
perform after speaking each chunk.  E.G.  speak a line at a time
etc.  Speaking commences at current buffer position.  Pressing
\\[keyboard-quit] breaks out, leaving point on last chunk that
was spoken.  Any other key continues to speak the buffer."
  (interactive)
  (let ((command
         (key-binding (read-key-sequence "Press navigation key to repeat: "))))
    (unless command (error "You specified an invalid key sequence.  "))
    (emacspeak-execute-repeatedly command)))

;;}}}
;;{{{  skimming

(defun emacspeak-speak-skim-buffer ()
  "Skim the current buffer  a paragraph at a time."
  (interactive)
  (emacspeak-execute-repeatedly 'forward-paragraph))

;;}}}
;;{{{   quieten messages

(ems-generate-switcher 'emacspeak-toggle-speak-messages
                       'emacspeak-speak-messages
                       "Toggle  state of whether emacspeak echoes messages.")

;;}}}
;;{{{  Moving across fields:

;; Fields are defined by property 'field

;; helper function: speak a field

(defun emacspeak-speak-field ()
  "Speak current field."
  (interactive)
  (dtk-speak (field-string (point))))

(defun emacspeak-speak-next-field ()
  "Move to and speak next field."
  (interactive)
  (cl-declare (special inhibit-field-text-motion))
  (let ((inhibit-field-text-motion t))
    (when 
        (goto-char (next-single-property-change (point) 'field))
      (emacspeak-speak-field))))

(defun emacspeak-speak-previous-field ()
  "Move to previous field and speak it."
  (interactive)
  (cl-declare (special inhibit-field-text-motion))
  (let ((inhibit-field-text-motion t))
    (when 
        (goto-char (previous-single-property-change (point) 'field))
      (emacspeak-speak-field))))

(defun emacspeak-speak-current-column ()
  "Speak the current column."
  (interactive)
  (message "Point at column %d" (current-column)))

(defun emacspeak-speak-current-percentage ()
  "Announce the percentage into the current buffer."
  (interactive)
  (message "Point is  %d%% into  the current buffer"
           (emacspeak-get-current-percentage-into-buffer)))

;;}}}
;;{{{  Speak the last message again:

(defvar ems--message-filter-pattern nil
  "Internal variable holding  pattern used to filter spoken messages.")

(defun emacspeak-speak-message-again (&optional from-message-cache)
  "Speak the last message from Emacs once again.
The message is also placed in the kill ring for convenient yanking "
  (interactive "P")
  (cl-declare (special emacspeak-last-message))
  (when  (and emacspeak-last-message (called-interactively-p 'interactive))
    (kill-new emacspeak-last-message))
  (cond
   (from-message-cache (dtk-speak emacspeak-last-message))
   (t
    (save-current-buffer
      (set-buffer "*Messages*")
      (goto-char (point-max))
      (skip-syntax-backward " >")
      (emacspeak-speak-line)
      (when  (called-interactively-p 'interactive)
        (kill-new (ems--this-line)))))))

;;}}}
;;{{{  Using emacs's windows usefully:

;;Return current window contents
(defsubst emacspeak-get-window-contents ()
  "Return window contents."
  (save-excursion
    (buffer-substring
     (window-start (selected-window))
     (window-end (selected-window)  'update ))))

(defun emacspeak-speak-windowful ()
  "Line to top, then Speak window contents."
  (interactive)
  (recenter 0)
  (emacspeak-auditory-icon 'scroll)
  (dtk-speak (emacspeak-get-window-contents)))

(defun emacspeak-speak-window-information ()
  "Speaks information about current window."
  (interactive)
  (message "Current window has %s lines and %s columns with
top left %s %s "
           (window-height)
           (window-width)
           (cl-first (window-edges))
           (cl-second (window-edges))))

(defun emacspeak-speak-current-window ()
  "Speak contents of current window.
Speaks entire window irrespective of point."
  (interactive)
  (emacspeak-speak-region
   (window-start (selected-window))
   (window-end (selected-window) 'update)))

(defun emacspeak-owindow-scroll-up ()
  "Scroll up the window that command `other-window' would move to.
Speak the window contents after scrolling."
  (interactive)
  (save-window-excursion
    (other-window 1)
    (call-interactively 'scroll-up)))

(defun emacspeak-owindow-scroll-down ()
  "Scroll down  the window that command `other-window' would move to.
Speak the window contents after scrolling."
  (interactive)
  (save-window-excursion
    (other-window 1)
    (call-interactively 'scroll-down)))

(defun emacspeak-owindow-next-line (count)
  "Move to the next line in the other window and speak it.
Numeric prefix arg COUNT can specify number of lines to move."
  (interactive "p")
  (setq count (or count 1))
  (let ((residue nil))
    (save-current-buffer
      (set-buffer (window-buffer (next-window)))
      (end-of-line)
      (setq residue (forward-line count))
      (cond
       ((> residue 0) (message "At bottom of other window "))
       (t (set-window-point (get-buffer-window (current-buffer))
                            (point))
          (emacspeak-speak-line))))))

(defun emacspeak-owindow-previous-line (count)
  "Move to the next line in the other window and speak it.
Numeric prefix arg COUNT specifies number of lines to move."
  (interactive "p")
  (setq count (or count 1))
  (let ((residue nil))
    (save-current-buffer
      (set-buffer (window-buffer (next-window)))
      (end-of-line)
      (setq residue (forward-line (- count)))
      (cond
       ((> 0 residue) (message "At top of other window "))
       (t (set-window-point (get-buffer-window (current-buffer))
                            (point))
          (emacspeak-speak-line))))))

(defun emacspeak-owindow-speak-line ()
  "Speak the current line in the other window."
  (interactive)
  (save-current-buffer
    (set-buffer (window-buffer (next-window)))
    (goto-char (window-point))
    (emacspeak-speak-line)))

(defun emacspeak-speak-predefined-window (&optional arg)
  "Speak one of the first 10 windows on the screen, 0 is current window.
Speaks entire window irrespective of point.  Semantics of `other'
is the same as for the Emacs builtin `other-window'."
  (interactive "P")
  (cl-declare (special last-input-event))
  (let* ((window
          (cond
           ((not (called-interactively-p 'interactive)) arg)
           (t
            (read (format "%c" last-input-event))))))
    (or (numberp window)
        (setq window  (read-number "Window   between 1 and 9:" 1)))
    (save-window-excursion
      (other-window window)
      (emacspeak-speak-region
       (window-start (selected-window))
       (window-end  (selected-window) 'update)))))

;;}}}
;;{{{  Intelligent interactive commands for reading:

;; Prompt the user if asked to prompt.
;; Prompt is:
;; press 'b' for beginning of unit,
;; 'r' for rest of unit,
;; any other key for entire unit
;; returns 1, -1, or nil accordingly.
;; If prompt is nil, does not prompt: just gets the input

(defun emacspeak-ask-how-to-speak (unit-name prompt)
  "Argument UNIT-NAME specifies kind of unit that is being spoken.
Argument PROMPT specifies the prompt to display."
  (if prompt
      (message
       (format "Press s to speak start of %s, r for rest of  %s. \
 Any  key for entire %s "
               unit-name unit-name unit-name)))
  (let ((char (read-char)))
    (cond
     ((= char ?s) -1)
     ((= char ?r) 1)
     (t nil))))

(defun emacspeak-speak-buffer-interactively ()
  "Speak the start of, rest of, or the entire buffer.
 `s' to speak the start.
 `r' to speak the rest.
any other key to speak entire buffer."
  (interactive)
  (emacspeak-speak-buffer
   (emacspeak-ask-how-to-speak "buffer" (sit-for 1))))

(defun emacspeak-speak-help-interactively ()
  "Speak the start of, rest of, or the entire help.
 `s' to speak the start.
 `r' to speak the rest.
any other key to speak entire help."
  (interactive)
  (emacspeak-speak-help
   (emacspeak-ask-how-to-speak "help" (sit-for 1))))

(defun emacspeak-speak-line-interactively ()
  "Speak the start of, rest of, or the entire line.
 `s' to speak the start.
 `r' to speak the rest.
any other key to speak entire line."
  (interactive)
  (emacspeak-speak-line
   (emacspeak-ask-how-to-speak "line" (sit-for 1))))

(defun emacspeak-speak-paragraph-interactively ()
  "Speak the start of, rest of, or the entire paragraph.
 `s' to speak the start.
 `r' to speak the rest.
any other key to speak entire paragraph."
  (interactive)
  (emacspeak-speak-paragraph
   (emacspeak-ask-how-to-speak "paragraph" (sit-for 1))))

(defun emacspeak-speak-page-interactively ()
  "Speak the start of, rest of, or the entire page.
 `s' to speak the start.
 `r' to speak the rest.
any other key to speak entire page."
  (interactive)
  (emacspeak-speak-page
   (emacspeak-ask-how-to-speak "page" (sit-for 1))))

(defun emacspeak-speak-word-interactively ()
  "Speak the start of, rest of, or the entire word.
 `s' to speak the start.
 `r' to speak the rest.
any other key to speak entire word."
  (interactive)
  (emacspeak-speak-word
   (emacspeak-ask-how-to-speak "word" (sit-for 1))))

(defun emacspeak-speak-sexp-interactively ()
  "Speak the start of, rest of, or the entire sexp.
 `s' to speak the start.
 `r' to speak the rest.
any other key to speak entire sexp."
  (interactive)
  (emacspeak-speak-sexp
   (emacspeak-ask-how-to-speak "sexp" (sit-for 1))))

;;}}}
;;{{{  emacs rectangles and regions:

;; These help you listen to columns of text. Useful for tabulated data
(defun emacspeak-speak-rectangle (start end)
  "Speak a rectangle of text.
Rectangle is delimited by point and mark.  When call from a
program, arguments specify the START and END of the rectangle."
  (interactive "r")
  (require 'rect)
  (dtk-speak-list (extract-rectangle start end)))

;;}}}
;;{{{  Matching delimiters:

;; A modified blink-matching-open that always displays the matching line
;; in the minibuffer so emacspeak can speak it.
;; Helper: emacspeak-speak-blinkpos-message

(defun emacspeak-speak-blinkpos-message (blinkpos)
  "Speak message about matching blinkpos."
  (ems-set-pause-temporarily
   blinkpos (1+ blinkpos) 5
   (ems-set-personality-temporarily
    blinkpos (1+ blinkpos) voice-animate
    (tts-with-punctuations
     'all
     (dtk-speak-and-echo
      (concat
       "Matches "
       (cond
        ;; Show what precedes the open in its line, if anything.
        ((save-excursion
           (skip-chars-backward " \t")
           (not (bolp)))
         (buffer-substring (line-beginning-position) (1+ blinkpos)))
        ;; Show what follows the open in its line, if anything.
        ((save-excursion
           (forward-char 1)
           (skip-chars-forward " \t")
           (not (eolp)))
         (buffer-substring blinkpos (line-end-position)))
        ;; Otherwise show the previous nonblank line.
        (t
         (concat
          (buffer-substring
           (progn
             (backward-char 1)
             (skip-chars-backward "\n \t")
             (line-beginning-position))
           (progn (end-of-line)
                  (skip-chars-backward " \t")
                  (point)))
          ;; Replace the newline and other whitespace with `...'.
          "..."
          (buffer-substring blinkpos (1+ blinkpos)))))))))))

;; The only change to emacs' default blink-matching-paren is the
;; addition of the call to helper emacspeak-speak-blinkpos-message
;; This matcher if from emacs 19 from memory.

(defun emacspeak-blink-matching-open ()
  "Move cursor momentarily to the beginning of the sexp before point.
Also display match context in minibuffer."
  (interactive)
  (when (and (> (point) (point-min))
             blink-matching-paren
             ;; Verify an even number of quoting characters precede the close.
             (= 1 (logand 1 (- (point)
                               (save-excursion
                                 (forward-char -1)
                                 (skip-syntax-backward "/\\")
                                 (point))))))
    (let* ((oldpos (point))
           (blink-matching-delay 5)
           blinkpos
           message-log-max  ; Don't log messages about paren matching.
           matching-paren
           open-paren-line-string)
      (save-excursion
        (save-restriction
          (if blink-matching-paren-distance
              (narrow-to-region (max (minibuffer-prompt-end)
                                     (- (point) blink-matching-paren-distance))
                                oldpos))
          (condition-case ()
              (let ((parse-sexp-ignore-comments
                     (and parse-sexp-ignore-comments
                          (not blink-matching-paren-dont-ignore-comments))))
                (setq blinkpos (scan-sexps oldpos -1)))
            (error nil)))
        (and blinkpos
             ;; Not syntax '$'.
             (not (eq (syntax-class (syntax-after blinkpos)) 8))
             (setq matching-paren
                   (let ((syntax (syntax-after blinkpos)))
                     (and (consp syntax)
                          (eq (syntax-class syntax) 4)
                          (cdr syntax)))))
        (cond
         ((not (or (eq matching-paren (char-before oldpos))
                   ;; The cdr might hold a new paren-class info rather than
                   ;; a matching-char info, in which case the two CDRs
                   ;; should match.
                   (eq matching-paren (cdr (syntax-after (1- oldpos))))))
          (message "Mismatched parentheses"))
         ((not blinkpos)
          (if (not blink-matching-paren-distance)
              (message "Unmatched parenthesis")))
         ((pos-visible-in-window-p blinkpos)
          ;; Matching open within window, temporarily move to blinkpos but only
          ;; if `blink-matching-paren-on-screen' is non-nil.
          (and blink-matching-paren-on-screen
               (save-excursion
                 (goto-char blinkpos)
                 (emacspeak-speak-blinkpos-message blinkpos)
                 (sit-for blink-matching-delay))))
         (t
          (save-excursion
            (goto-char blinkpos)
            (setq open-paren-line-string
                  ;; Show what precedes the open in its line, if anything.
                  (if (save-excursion
                        (skip-chars-backward " \t")
                        (not (bolp)))
                      (buffer-substring (line-beginning-position)
                                        (1+ blinkpos))
                    ;; Show what follows the open in its line, if anything.
                    (if (save-excursion
                          (forward-char 1)
                          (skip-chars-forward " \t")
                          (not (eolp)))
                        (buffer-substring blinkpos
                                          (line-end-position))
                      ;; Otherwise show the previous nonblank line,
                      ;; if there is one.
                      (if (save-excursion
                            (skip-chars-backward "\n \t")
                            (not (bobp)))
                          (concat
                           (buffer-substring (progn
                                               (skip-chars-backward "\n \t")
                                               (line-beginning-position))
                                             (progn (end-of-line)
                                                    (skip-chars-backward " \t")
                                                    (point)))
                           ;; Replace the newline and  whitespace with `...'.
                           "..."
                           (buffer-substring blinkpos (1+ blinkpos)))
                        ;; There is nothing to show except the char itself.
                        (buffer-substring blinkpos (1+ blinkpos)))))))
          (message "Matches %s"
                   (substring-no-properties
                    open-paren-line-string))
          (sit-for blink-matching-delay)))))))

;;}}}
;;{{{  Auxiliary functions:

(defun emacspeak-kill-buffer-carefully (buffer)
  "Kill BUFFER BUF if it exists."
  (and buffer
       (get-buffer buffer)
       (kill-buffer buffer)))

(defun emacspeak-overlay-get-text (o)
  "Return text under overlay OVERLAY.
Argument O specifies overlay."
  (save-current-buffer
    (set-buffer (overlay-buffer o))
    (buffer-substring (overlay-start o) (overlay-end o))))

;;}}}
;;{{{ Speaking spaces

(defun emacspeak-speak-spaces-at-point ()
  "Speak the white space at point."
  (interactive)
  (cond
   ((not (= 32 (char-syntax (following-char))))
    (message "Not on white space"))
   (t
    (let ((orig (point))
          (start (save-excursion
                   (skip-syntax-backward " ")
                   (point)))
          (end (save-excursion
                 (skip-syntax-forward " ")
                 (point))))
      (message "Space %s of %s"
               (1+ (- orig start)) (- end start))))))

;;}}}
;;{{{  completion helpers

;; switching to completions window from minibuffer:

(defun emacspeak-get-minibuffer-contents ()
  "Return contents of the minibuffer."
  (save-current-buffer
    (set-buffer (window-buffer (minibuffer-window)))
    (minibuffer-contents)))

;; Make all occurrences of string inaudible
(defun emacspeak-make-string-inaudible (string)
  (unless (string-match "^ *$" string)
    (with-silent-modifications
      (save-excursion
        (goto-char (point-min))
        (while (search-forward string nil t)
          (put-text-property
           (match-beginning 0) (match-end 0)
           'personality 'inaudible))))))

(defun emacspeak-switch-to-reference-buffer ()
  "Switch back to buffer that generated completions."
  (interactive)
  (cl-declare (special completion-reference-buffer))
  (if completion-reference-buffer
      (switch-to-buffer completion-reference-buffer)
    (error "Reference buffer not found."))
  (when (called-interactively-p 'interactive)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defun emacspeak-completions-move-to-completion-group ()
  "Move to group of choices beginning with character last
typed. If no such group exists, then we try to search for that
char, or dont move. "
  (interactive)
  (cl-declare (special last-input-event))
  (let ((pattern
         (format
          "[ \t\n]%s%c"
          (or (emacspeak-get-minibuffer-contents) "")
          last-input-event))
        (input (format "%c" last-input-event))
        (case-fold-search t))
    (when (or (re-search-forward pattern nil t)
              (re-search-backward pattern nil t)
              (search-forward input nil t)
              (search-backward input nil t))
      (skip-syntax-forward " ")
      (emacspeak-auditory-icon 'search-hit))
    (dtk-speak (emacspeak-get-current-completion))))

(defun emacspeak-completion-setup-hook ()
  "Set things up for emacspeak."
  (with-current-buffer standard-output
    (goto-char (point-min))
    (emacspeak-make-string-inaudible (emacspeak-get-minibuffer-contents))
    (emacspeak-auditory-icon 'help)))

(add-hook 'completion-setup-hook 'emacspeak-completion-setup-hook)

(cl-declaim (special completion-list-mode-map))
(define-key completion-list-mode-map
            "\C-o" 'emacspeak-switch-to-reference-buffer)
(define-key completion-list-mode-map
            (ems-kbd "<backspace>") 'previous-completion)
(define-key completion-list-mode-map " " 'next-completion)
(define-key completion-list-mode-map "\C-m" 'choose-completion)
(let ((chars
       "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
  (cl-loop for char across chars
           do
           (define-key completion-list-mode-map
                       (format "%c" char)
                       'emacspeak-completions-move-to-completion-group)))

;;}}}
;;{{{ mark convenience commands

(defun emacspeak-mark-speak-mark-line ()
  "Helper to speak line containing mark."
  (cl-declare (special voice-animate))
  (emacspeak-auditory-icon 'mark-object)
  (ems-set-personality-temporarily (point) (1+ (point))
                                   voice-animate
                                   (emacspeak-speak-line)))

(defun emacspeak-mark-backward-mark ()
  "Cycle backward through the mark ring.
To cycle forward, use pop-to-mark-command bound to \\[pop-to-mark-command] "
  (interactive)
  (cl-declare (special mark-ring))
  (unless mark-ring (error "Mark ring is empty."))
  (let ((target (elt mark-ring (1- (length mark-ring)))))
    (when target
      (setq mark-ring
            (cons (copy-marker (mark-marker))
                  (nbutlast mark-ring 1)))
      (set-marker (mark-marker) (point) (current-buffer))
      (goto-char (marker-position target))
      (move-marker target nil)
      (when (called-interactively-p 'interactive)
        (emacspeak-mark-speak-mark-line)))))

;;}}}
;;{{{  speak message at time

(defun emacspeak-speak-message-at-time (time message)
  "Speak message at specified time.
Provides simple stop watch functionality.
See documentation for command run-at-time for details on time-spec."
  (interactive (list (read-from-minibuffer "Time specification:  ")
                     (read-from-minibuffer "Message: ")))
  (run-at-time
   time nil
   #'(lambda (m)
       (dtk-notify-speak m)
       (when emacspeak-use-auditory-icons
         (emacspeak-play-auditory-icon 'alarm))
       (sox-tones))
   message)
  (message "Set alarm for %s" time)
  (emacspeak-auditory-icon 'button))

;;}}}
;;{{{ Directory specific settings

(defvar emacspeak-speak-directory-settings
  ".espeak.el"
  "Name of file that holds directory specific settings.")

(defun emacspeak-speak-load-directory-settings (&optional dir)
  "Load a directory specific Emacspeak settings file.
This is typically used to load up settings that are specific to
an electronic book consisting of many files in the same
directory."
  (interactive "DDirectory")
  (cl-declare (special emacspeak-speak-directory-settings default-directory))
  (unless dir (setq dir default-directory))
  (ems-with-messages-silenced
    (let ((res (locate-dominating-file dir emacspeak-speak-directory-settings)))
      (when
          (and res
               (file-exists-p 
                (expand-file-name emacspeak-speak-directory-settings res)))
        (ems--fastload (expand-file-name
                        emacspeak-speak-directory-settings res))
        (message "loaded %s"
                 (expand-file-name emacspeak-speak-directory-settings res))
        (emacspeak-auditory-icon 'task-done)))))

;;}}}
;;{{{ silence:

(defcustom emacspeak-silence-hook nil
  "Functions run after emacspeak-silence is called."
  :type '(repeat function)
  :group 'emacspeak)

(defun emacspeak-silence ()
  "Silence is golden. Stop speech, and pause/resume any media
streams. Runs `emacspeak-silence-hook' which can be used to
configure which media players get silenced or paused/resumed."
  (interactive)
  (cl-declare (special emacspeak-silence-hook))
  (dtk-stop)
  (run-hooks 'emacspeak-silence-hook))

;;}}}
;;{{{ Smart date prompe:

(defun emacspeak-speak-collect-date (prompt time-format-string)
  "Smart date collector.
Prompts with `prompt'.
`time-format-string' is format argument for format-time-string.
This function is sensitive to calendar mode when prompting."
  (let ((default (format-time-string time-format-string))) ; today is default
    (when (eq major-mode 'calendar-mode)
                                        ;get smart default from calendar
      (let ((date (calendar-cursor-to-nearest-date)))
        (setq default (format-time-string time-format-string
                                          (apply 'encode-time 0 0
                                                 0
                                                 (cl-second date)
                                                 (cl-first date)
                                                 (list (cl-third date)))))))
    (read-from-minibuffer prompt
                          default
                          nil nil nil
                          default)))

(defun emacspeak-speak-read-date-year/month/date ()
  "Return today as yyyy/mm/dd"
  (emacspeak-speak-collect-date "Date:"
                                "%Y/%m/%d"))

(defun emacspeak-speak-date-YearMonthDate ()
  "Return today as yyyymmdd"
  (emacspeak-speak-collect-date "Date:"
                                "%Y%m%d"))

(defun emacspeak-speak-date-month/date ()
  "Return today as mm/dd"
  (emacspeak-speak-collect-date "Date:"
                                "%m/%d"))

(defun emacspeak-speak-year-month-date ()
  "Return today as yyyy-mm-dd"
  (emacspeak-speak-collect-date "Date:"
                                "%Y-%m-%d"))

;;}}}
;;{{{ Navigating completions:

(defun emacspeak-minibuffer-next-completion ()
  "Move to next available minibuffer completion."
  (interactive)
  (or (get-buffer "*Completions*") (minibuffer-completion-help))
  (when (get-buffer "*Completions*")
    (with-current-buffer (get-buffer "*Completions*")
      (let ((voice-lock-mode nil))
        (funcall-interactively #'next-completion 1)))))

(defun emacspeak-minibuffer-previous-completion ()
  "Move to previous available minibuffer completion."
  (interactive)
  (or (get-buffer "*Completions*") (minibuffer-completion-help))
  (when (get-buffer "*Completions*")
    (with-current-buffer (get-buffer "*Completions*")
      (let ((voice-lock-mode nil))
        (funcall-interactively #'previous-completion 1)))))

;; Hacked out of choose-completion
(defun emacspeak--choose-completion ()
  "Choose the completion at point."
  (interactive)
  (let ((buffer completion-reference-buffer)
        (base-position completion-base-position)
        (insert-function completion-list-insert-choice-function)
        (choice
         (save-excursion
           (let (beg end)
             (cond
              ((and (not (eobp)) (get-text-property (point) 'mouse-face))
               (setq end (point) beg (1+ (point))))
              ((and (not (bobp))
                    (get-text-property (1- (point)) 'mouse-face))
               (setq end (1- (point)) beg (point)))
              (t (error "No completion here")))
             (setq beg (previous-single-property-change beg 'mouse-face))
             (setq end (or (next-single-property-change end 'mouse-face)
                           (point-max)))
             (buffer-substring-no-properties beg end)))))
    (unless (buffer-live-p buffer) (error "Destination buffer is dead"))
    (with-current-buffer buffer
      (choose-completion-string choice buffer base-position insert-function))))

(defun emacspeak-minibuffer-choose-completion ()
  "Choose current completion."
  (interactive)
  (when (get-buffer "*Completions*")
    (with-current-buffer (get-buffer "*Completions*")
      (message "%s" (thing-at-point 'symbol))
      (emacspeak--choose-completion))))

(define-key 
 minibuffer-local-completion-map
 "\C-n" 'emacspeak-minibuffer-next-completion)
(define-key 
 minibuffer-local-completion-map
 "\C-p" 'emacspeak-minibuffer-previous-completion)
(define-key 
 minibuffer-local-completion-map
 (kbd "C-@")
 'emacspeak-minibuffer-choose-completion)
(define-key minibuffer-local-completion-map
            (kbd "C-SPC") 'emacspeak-minibuffer-choose-completion)

;;}}}
;;{{{ Open Emacspeak Info Pages:

(defun emacspeak-open-info ()
  "Open Emacspeak Info Manual."
  (interactive)
  (cl-declare (special emacspeak-info-directory))
  (funcall-interactively 
   #'info 
   (expand-file-name "emacspeak.info" emacspeak-info-directory)
   "*Emacspeak Info*"))

;;}}}
;;{{{ Describe help map:

(defun describe-help-keys ()
  "Show bindings under C-h."
  (interactive)
  (describe-bindings "\C-h")
  (emacspeak-auditory-icon 'help)
  (with-current-buffer (window-buffer (selected-window))
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{Utility: Persist variable to a file:
(defun emacspeak--persist-variable (var file)
  "Persist variable  `var' to file `FILE'.
Arranges for `VAR' to be restored when `file' is loaded."
  (interactive)
  (when (and (not noninteractive) (boundp var))
    (let ((buffer (find-file-noselect file))
          (print-length nil)
          (print-level nil))
      (with-current-buffer buffer
        (erase-buffer)
        (insert ";;; Auto-generated.\n\n")
        (insert (format "(setq %s \n" var))
        (if (listp (symbol-value var)) (insert "'"))
        (pp (symbol-value var) (current-buffer))
        (insert (format ") ;;; set %s\n\n" var))
        (save-buffer)))))

;;}}}
;;{{{Tapestry --Jump to window by name:

(defun emacspeak-describe-tapestry (&optional details)
  "Describe the current layout of visible buffers in current frame.
Use interactive prefix arg to get coordinate positions of the
displayed buffers."
  (interactive "P")
  (cl-declare (special voice-animate voice-bolden))
  (let* ((window-list (window-list))
         (count (length window-list))
         (windows nil)
         (description
          (propertize 
           (format
            "Displaying %s window%s "
            count 
            (if (> count 1) "s" ""))
           'personality voice-annotate)))
    (setq
     windows 
     (cond
      (details 
       (cl-loop
        for window in window-list
        collect
        (let ((w
               (propertize
                (format "%s "  (window-buffer window))
                'personality voice-animate))
              (corners  (window-edges window))
              (tl nil)
              (br nil))
          (setq tl (format  " %d %d " (cl-second corners) (cl-first corners))
                br  (format " %d %d " (cl-fourth corners) (cl-third corners)))
          (put-text-property 0 (length tl) 'personality voice-bolden tl)
          (put-text-property 0 (length br) 'personality voice-bolden br)
          (concat w " with top left " tl " and bottom right " br))))
      (t (mapcar #'buffer-name (mapcar #'window-buffer window-list)))))
    (emacspeak--sox-multiwindow )
    (dtk-speak (concat description (mapconcat #'identity windows " ")))))

(defun emacspeak-select-window-by-name (buffer-name)
  "Select window by the name of the buffer it displays.
This is useful when using modes like ECB or the new GDB UI where
  you want to preserve the window layout 
but quickly switch to a window by name."
  (interactive
   (list
    (completing-read 
     "Select window: "
     (mapcar 
      #'(lambda (w)
          (list (buffer-name (window-buffer w))))
      (window-list))
     nil 'must-match)))
  (pop-to-buffer buffer-name)
  (emacspeak-speak-line))

;;}}}
;;{{{Battery:
(require 'battery "battery" 'no-error)
(defvar emacspeak-battery-prev nil
  "Previous battery status.")

(defun emacspeak-battery-alarm (data)
  "Battery alarm when critical."
  (when
      (and emacspeak-battery-prev
           (string=  (alist-get ?L data) "off-line")
           (< (string-to-number (alist-get ?p data)) 10)
           (>= (string-to-number (alist-get ?p emacspeak-battery-prev)) 10))
    (emacspeak-prompt "battery-low")
    (setq emacspeak-battery-prev data)))
(when (boundp 'battery-update-functions)
  (add-to-list 'battery-update-functions 'emacspeak-battery-alarm))
;;}}}
;;{{{Repeat Mode:
;; See https://karthinks.com/software/it-bears-repeating/

(defvar emacspeak-repeat-was-active nil
  "Cache repeat-progress")

(defun emacspeak-repeat-check-hook ()
  "Play appropriate repeat icon."
  (cl-declare (special repeat-in-progress emacspeak-repeat-was-active))
  (cond
   ((and repeat-in-progress (not emacspeak-repeat-was-active))
    (setq emacspeak-repeat-was-active t)
    (emacspeak-auditory-icon 'repeat-start))
   ((and (not repeat-in-progress)  emacspeak-repeat-was-active)
    (setq emacspeak-repeat-was-active nil)
    (emacspeak-auditory-icon 'repeat-end))
   (repeat-in-progress (emacspeak-auditory-icon 'repeat-active))))

(defun ems--repeat-sentinel (process _state)
  "Process sentinel to disable repeat."
  (when
      (and repeat-mode
           (memq (process-status process) '(failed signal exit)))
    (repeat-exit)))

(defsubst emacspeak-repeat-mode-hook ()
  "Add or remove emacspeak-repeat-check-hook from post-command-hook"
  (cl-declare (special repeat-mode))
  (cond
   (repeat-mode
    (add-hook 'post-command-hook 'emacspeak-repeat-check-hook 'at-end))
   (t (remove-hook 'post-command-hook 'emacspeak-repeat-check-hook))))

(add-hook 'repeat-mode-hook 'emacspeak-repeat-mode-hook )

;;}}}
(provide 'emacspeak-speak)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}
