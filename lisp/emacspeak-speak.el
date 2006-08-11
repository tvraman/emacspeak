;;; emacspeak-speak.el --- Implements Emacspeak's core speech services
;;; $Id$
;;; $Author$
;;; Description:  Contains the functions for speaking various chunks of text
;;; Keywords: Emacspeak,  Spoken Output
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision: 24.6 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2004, T. V. Raman
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

;;; This module defines the core speech services used by emacspeak.
;;; It depends on the speech server interface modules
;;; It protects other parts of emacspeak
;;; from becoming dependent on the speech server modules

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(eval-when-compile
  (require 'backquote))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'custom)
(require 'time-date)
(require 'voice-setup)
(require 'thingatpt)
(eval-when-compile
  (require 'dtk-interp)
  (require 'shell)
  (require 'which-func nil)

  (require 'emacspeak-sounds))

;;}}}
;;{{{  custom group
(defgroup emacspeak-speak nil
  "Basic speech output commands."
  :group 'emacspeak)

;;}}}
;;{{{ inhibit-point-motion-hooks

(defsubst ems-inhibit-point-motion-hooks ()
  (declare (special inhibit-point-motion-hooks))
  (if (boundp 'inhibit-point-motion-hooks)
      inhibit-point-motion-hooks
    nil))

;;}}}
;;{{{  Macros

;;; Save read-only and modification state, perform some actions and
;;; restore state
(defmacro ems-modify-buffer-safely   (&rest body )
  "Allow BODY to temporarily modify read-only content."
  (`
   (progn
     (declare (special inhibit-point-motion-hooks))
     (let    ((save-read-only buffer-read-only)
              (buffer-read-only nil )
              (save-inhibit-read-only inhibit-read-only)
              (inhibit-read-only t)
              (save-inhibit-point-motion-hooks (ems-inhibit-point-motion-hooks))
              (inhibit-point-motion-hooks t)
              (modification-flag (buffer-modified-p)))
       (unwind-protect
           (,@ body )
         (setq buffer-read-only save-read-only
               inhibit-read-only save-inhibit-read-only
               inhibit-point-motion-hooks save-inhibit-point-motion-hooks)
         (set-buffer-modified-p modification-flag ))))))

(defmacro ems-set-personality-temporarily (start end value
                                                 &rest body)
  "Temporarily set personality.
Argument START   specifies the start of the region to operate on.
Argument END specifies the end of the region.
Argument VALUE is the personality to set temporarily
Argument BODY specifies forms to execute."
  (`
   (progn
     (declare (special voice-lock-mode ))
     (let ((save-voice-lock voice-lock-mode)
           (saved-personality (get-text-property
                               (, start) 'personality))
           (save-read-only buffer-read-only)
           (buffer-read-only nil )
           (save-inhibit-read-only inhibit-read-only)
           (inhibit-read-only t)
           (save-inhibit-point-motion-hooks (ems-inhibit-point-motion-hooks))
           (inhibit-point-motion-hooks t)
           (modification-flag (buffer-modified-p)))
       (unwind-protect
           (progn
             (setq voice-lock-mode t )
             (put-text-property
              (max (point-min) (, start))
              (min (point-max) (, end))
              'personality (, value))
             (,@ body))
         (put-text-property
          (max (point-min) (, start))
          (min (point-max)  (, end)) 'personality saved-personality)
         (setq buffer-read-only save-read-only
               inhibit-read-only save-inhibit-read-only
               inhibit-point-motion-hooks save-inhibit-point-motion-hooks
               voice-lock-mode save-voice-lock )
         (set-buffer-modified-p modification-flag ))))))

(defmacro ems-with-errors-silenced  (&rest body)
  "Evaluate body  after temporarily silencing auditory error feedback."
  `(progn
     (let ((emacspeak-speak-cue-errors nil))
       (ad-disable-advice  'error 'before 'emacspeak )
       (ad-deactivate 'error)
       ,@body
       (ad-enable-advice  'error 'before 'emacspeak )
       (ad-activate 'error))))

;;}}}
;;{{{ getting and speaking text ranges

(defsubst emacspeak-speak-get-text-range (property)
  "Return text range  around  at point and having the same value as  specified by argument PROPERTY."
  (buffer-substring
   (previous-single-property-change (point)
                                    property nil (point-min))
   (next-single-property-change
    (point) property nil (point-max))))

(defun emacspeak-speak-text-range (property)
  "Speak text range identified by this PROPERTY."
  (dtk-speak (emacspeak-speak-get-text-range property)))

;;}}}
;;{{{  Apply audio annotations

;;; prompt for auditory icon with completion

(defun emacspeak-audio-annotate-paragraphs ()
  "Set property auditory-icon at front of all paragraphs."
  (interactive )
  (save-excursion
    (goto-char (point-max))
    (ems-modify-buffer-safely
     (let ((sound-cue 'paragraph))
       (while (not (bobp))
         (backward-paragraph)
         (put-text-property  (1+ (point))
                             (+ 2    (point ))
                             'auditory-icon sound-cue ))))))

(defcustom  emacspeak-speak-paragraph-personality voice-animate
  "*Personality used to mark start of paragraph."
  :group 'emacspeak-speak
  :type 'symbol)

(defvar emacspeak-speak-voice-annotated-paragraphs nil
  "Records if paragraphs in this buffer have been voice
  annotated.")

(make-variable-buffer-local
 'emacspeak-speak-voice-annotated-paragraphs)

(defsubst emacspeak-speak-voice-annotate-paragraphs ()
  "Locate paragraphs and voice annotate the first word.
Here, paragraph is taken to mean a chunk of text preceded by a blank line.
Useful to do this before you listen to an entire buffer."
  (interactive)
  (declare (special emacspeak-speak-paragraph-personality
                    emacspeak-speak-voice-annotated-paragraphs))
  (when emacspeak-speak-paragraph-personality
    (save-excursion
      (goto-char (point-min))
      (condition-case nil
          (let ((start nil)
                (blank-line "\n[ \t\n\r]*\n")
                (inhibit-point-motion-hooks t))
            (ems-modify-buffer-safely
             (while (re-search-forward blank-line nil t)
               (skip-syntax-forward " ")
               (setq start (point))
               (unless (get-text-property start 'personality)
                 (skip-syntax-forward "^ ")
                 (put-text-property start (point)
                                    'personality
                                    emacspeak-speak-paragraph-personality)))))
        (error nil))
      (setq emacspeak-speak-voice-annotated-paragraphs t))))

;;}}}
;;{{{  sync emacspeak and TTS:

(defsubst   emacspeak-dtk-sync ()
  "Bring emacspeak and dtk in sync."
  (dtk-interp-sync))

;;}}}
;;{{{ helper function --prepare completions buffer

(defsubst emacspeak-prepare-completions-buffer()
  (ems-modify-buffer-safely
   (goto-char (point-min))
   (forward-line 3)
   (delete-region (point-min) (point))
   (dtk-set-punctuations 'all)
   (emacspeak-dtk-sync)
   (emacspeak-auditory-icon 'help)))

;;}}}
;;{{{ helper function --decode ISO date-time

(defvar emacspeak-speak-iso-datetime-pattern
  "[0-9]\\{8\\}\\(T[0-9]\\{6\\}\\)Z?"
  "Regexp pattern that matches ISO date-time.")

(defsubst emacspeak-speak-decode-iso-datetime (iso)
  "Return a speakable string description."
  (declare (special emacspeak-speak-time-format-string))
  (let ((year  (read (substring iso 0 4)))
        (month (read (substring iso 4 6)))
        (day   (read (substring iso 6 8)))
        (hour 0)
        (minute 0)
        (second 0))
    (when (> (length iso) 12) ;; hour/minute
      (setq hour (read (substring iso 9 11)))
      (setq minute (read (substring iso 11 13))))
    (when (> (length iso) 14) ;; seconds
      (setq second (read (substring iso 13 15))))
    (when (and (> (length iso) 15) ;; utc specifier
               (char-equal ?Z (aref iso 15)))
      (setq second (+ (car (current-time-zone
                            (encode-time second minute hour day month
                                         year))) second)))
    ;; create the decoded date-time
    (condition-case nil
        (format-time-string emacspeak-speak-time-format-string
                            (encode-time second minute hour day month
                                         year))
      (error iso))))

;;}}}
;;{{{  url link pattern:
(defcustom emacspeak-speak-embedded-url-pattern
  "<http:.*>"
  "Pattern to recognize embedded URLs."
  :type 'string
  :group 'emacspeak-speak)

;;}}}
;;{{{  Actions

;;; Setting value of property 'emacspeak-action to a list
;;; of the form (before | after function)
;;; function to be executed before or after the unit of text at that
;;; point is spoken.

(defvar emacspeak-action-mode nil
  "Determines if action mode is active.
Non-nil value means that any function that is set as the
value of property action is executed when the text at that
point is spoken."

  )

(make-variable-buffer-local 'emacspeak-action-mode)

;;; Record in the mode line
(or (assq 'emacspeak-action-mode minor-mode-alist)
    (setq minor-mode-alist
          (append minor-mode-alist
                  '((emacspeak-action-mode " Action")))))

;;; Return the appropriate action hook variable that defines actions
;;; for this mode.

(defsubst  emacspeak-action-get-action-hook (mode)
  "Retrieve action hook.
Argument MODE defines action mode."
  (intern (format "emacspeak-%s-actions-hook" mode )))

;;; Execute action at point
(defsubst emacspeak-handle-action-at-point ()
  "Execute action specified at point."
  (declare (special emacspeak-action-mode ))
  (let ((action-spec (get-text-property (point) 'emacspeak-action )))
    (when (and emacspeak-action-mode action-spec )
      (condition-case nil
          (funcall  action-spec )
        (error (message "Invalid actionat %s" (point )))))))

(ems-generate-switcher 'emacspeak-toggle-action-mode
                       'emacspeak-action-mode
                       "Toggle state of  Emacspeak  action mode.
Interactive PREFIX arg means toggle  the global default value, and then set the
current local  value to the result.")
;;}}}
;;{{{  line, Word and Character echo

(defcustom emacspeak-line-echo nil
  "If t, then emacspeak echoes lines as you type.
You can use \\[emacspeak-toggle-line-echo] to set this
option."
  :group 'emacspeak-speak
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
  :group 'emacspeak-speak
  :type 'boolean)

(ems-generate-switcher ' emacspeak-toggle-word-echo
                         'emacspeak-word-echo
                         "Toggle state of  Emacspeak  word echo.
Interactive PREFIX arg means toggle  the global default value, and then set the
current local  value to the result.")

(defcustom emacspeak-character-echo t
  "If t, then emacspeak echoes characters  as you type.
You can
use \\[emacspeak-toggle-character-echo] to toggle this
setting."
  :group 'emacspeak-speak
  :type 'boolean)

(ems-generate-switcher ' emacspeak-toggle-character-echo
                         'emacspeak-character-echo
                         "Toggle state of  Emacspeak  character echo.
Interactive PREFIX arg means toggle  the global default value, and then set the
current local  value to the result.")

;;}}}
;;{{{ Showing the point:

(defcustom emacspeak-show-point nil
  " If T, then command  `emacspeak-speak-line' indicates position of point by an
aural highlight.  You can use
command `emacspeak-toggle-show-point' bound to
\\[emacspeak-toggle-show-point] to toggle this setting."
  :group 'emacspeak-speak
  :type 'boolean)

(ems-generate-switcher ' emacspeak-toggle-show-point
                         'emacspeak-show-point
                         "Toggle state of  Emacspeak-show-point.
Interactive PREFIX arg means toggle  the global default value, and then set the
current local  value to the result.")

;;}}}
;;{{{ compute percentage into the buffer:
;;{{{ simple percentage getter

(defsubst emacspeak-get-current-percentage-into-buffer ()
  "Return percentage of position into current buffer."
  (let* ((pos (point))
         (total (buffer-size))
         (percent (if (> total 50000)
                      ;; Avoid overflow from multiplying by 100!
                      (/ (+ (/ total 200) (1- pos)) (max (/ total 100) 1))
                    (/ (+ (/ total 2) (* 100 (1- pos))) (max total 1)))))
    percent))

(defsubst emacspeak-get-current-percentage-verbously ()
  "Return percentage of position into current buffer as a string."
  (let ((percent (emacspeak-get-current-percentage-into-buffer)))
    (cond
     ((= 0 percent) " top ")
     ((= 100 percent) " bottom ")
     (t (format " %d%% " percent)))))

;;}}}
;;;percentage getter with personality
                                        ; (defsubst emacspeak-get-current-percentage-verbously ()
                                        ;   "Return percentage of position into current buffer as a string."
                                        ;   (let ((percent (emacspeak-get-current-percentage-into-buffer))
                                        ;       (message nil))
                                        ;     (setq message
                                        ;         (cond
                                        ;          ((= 0 percent) " top")
                                        ;          ((= 100 percent) " bottom ")
                                        ;          (t (format " %d percent " percent))))
                                        ;     (put-text-property 0 (length message)
                                        ;                      'property voice-bolden message)
                                        ;     message))

;;}}}
;;{{{  indentation:

(defcustom emacspeak-audio-indentation nil
  "Option indicating if line indentation is cued.
If non-nil , then speaking a line indicates its indentation.
You can use  command `emacspeak-toggle-audio-indentation' bound
to \\[emacspeak-toggle-audio-indentation] to toggle this
setting.."
  :group 'emacspeak-speak
  :type 'boolean)

(make-variable-buffer-local 'emacspeak-audio-indentation)

;;; Indicate indentation.
;;; Argument indent   indicates number of columns to indent.

(defsubst emacspeak-indent (indent)
  "Produce tone indent."
  (when (> indent 1 )
    (let ((duration (+ 50 (* 20  indent )))
          (dtk-stop-immediately nil))
      (dtk-tone  250 duration))))

(defvar emacspeak-audio-indentation-methods
  '(("speak" . "speak")
    ("tone" . "tone"))
  "Possible methods of indicating indentation.")

(defcustom emacspeak-audio-indentation-method   'speak
  "*Current technique used to cue indentation.  Default is
`speak'.  You can specify `tone' for producing a beep
indicating the indentation.  Automatically becomes local in
any buffer where it is set."
  :group 'emacspeak-speak
  :type '(choice
          (const :tag "Ignore" nil)
          (const :tag "speak" speak)
          (const :tag "tone" tone)))

(make-variable-buffer-local
 'emacspeak-audio-indentation-method)

(ems-generate-switcher ' emacspeak-toggle-audio-indentation
                         'emacspeak-audio-indentation
                         "Toggle state of  Emacspeak  audio indentation.
Interactive PREFIX arg means toggle  the global default value, and then set the
current local  value to the result.
Specifying the method of indentation as `tones'
results in the Dectalk producing a tone whose length is a function of the
line's indentation.  Specifying `speak'
results in the number of initial spaces being spoken.")

;;}}}
;;{{{ Core speech functions:

;;{{{  Speak units of text

(defsubst emacspeak-speak-region (start end )
  "Speak region.
Argument START  and END specify region to speak."
  (interactive "r" )
  (declare (special emacspeak-speak-voice-annotated-paragraphs
                    inhibit-point-motion-hooks
                    voice-lock-mode))
  (let ((inhibit-point-motion-hooks t))
    (when (and voice-lock-mode
               (not emacspeak-speak-voice-annotated-paragraphs))
      (save-restriction
        (narrow-to-region start end )
        (emacspeak-speak-voice-annotate-paragraphs)))
    (emacspeak-handle-action-at-point)
    (dtk-speak (buffer-substring start end ))))

(defsubst emacspeak-speak-string (string personality)
  "Apply personality to string and speak it."
  (put-text-property 0 (length string)
                     'personality personality string)
  (dtk-speak string))

(defcustom emacspeak-horizontal-rule "^\\([=_-]\\)\\1+$"
  "*Regular expression to match horizontal rules in ascii
text."
  :group 'emacspeak-speak
  :type 'string)

(put 'emacspeak-horizontal-rule 'variable-interactive
     "sEnterregular expression to match horizontal rule: ")

(defcustom emacspeak-decoration-rule
  "^[ \t!@#$%^&*()<>|_=+/\\,.;:-]+$"
  "*Regular expressions to match lines that are purely
decorative ascii."
  :group 'emacspeak-speak
  :type 'string)

(put 'emacspeak-decoration-rule 'variable-interactive
     "sEnterregular expression to match lines that are decorative ASCII: ")

(defcustom emacspeak-unspeakable-rule
  "^[^0-9a-zA-Z]+$"
  "*Pattern to match lines of special chars.
This is a regular expression that matches lines containing only
non-alphanumeric characters.  emacspeak will generate a tone
instead of speaking such lines when punctuation mode is set
to some."
  :group 'emacspeak-speak
  :type 'string)

(put 'emacspeak-unspeakable-rule 'variable-interactive
     "sEnterregular expression to match unspeakable lines: ")
(defcustom emacspeak-speak-maximum-line-length  512
  "*Threshold for determining `long' lines.
Emacspeak will ask for confirmation before speaking lines
that are longer than this length.  This is to avoid accidentally
opening a binary file and torturing the speech synthesizer
with a long string of gibberish."
  :group 'emacspeak-speak
  :type 'number)
(make-variable-buffer-local 'emacspeak-speak-maximum-line-length)
;;{{{ filtering columns

(defcustom emacspeak-speak-line-column-filter nil
  "*List that specifies columns to be filtered.
The list when set holds pairs of start-col.end-col pairs
that specifies the columns that should not be spoken.
Each column contains a single character --this is inspired
by cut -c on UNIX."
  :group 'emacspeak-speak
  :type '(choice
          (const :tag "None" nil)
          (repeat :tag "Filter Specification"
                  (list
                   (integer :tag "Start Column")
                   (integer :tag "End Column")))))

(defvar emacspeak-speak-filter-table (make-hash-table)
  "Hash table holding persistent filters.")

(make-variable-buffer-local 'emacspeak-speak-line-column-filter)

(defcustom emacspeak-speak-line-invert-filter nil
  "Non-nil means the sense of `filter' is inverted when filtering
columns in a line --see
command emacspeak-speak-line-set-column-filter."
  :type 'boolean
  :group 'emacspeak-speak)

(make-variable-buffer-local 'emacspeak-speak-line-invert-filter)

(ems-generate-switcher '
 emacspeak-toggle-speak-line-invert-filter
 'emacspeak-speak-line-invert-filter
 "Toggle state of   how column filter is interpreted.
Interactive PREFIX arg means toggle  the global default value, and then set the
current local  value to the result.")

(defsubst emacspeak-speak-line-apply-column-filter (line &optional invert-filter)
  (declare (special emacspeak-speak-line-column-filter))
  (let ((filter emacspeak-speak-line-column-filter)
        (l  (length line))
        (pair nil)
        (personality (if invert-filter nil
                       'inaudible)))
    (when invert-filter
      (put-text-property  0   l
                          'personality 'inaudible line))
    (while filter
      (setq pair (pop filter))
      (when (and (<= (first pair) l)
                 (<= (second pair) l))
        (put-text-property (first pair)
                           (second pair)
                           'personality personality
                           line)))
    line))

(defsubst emacspeak-speak-persist-filter-entry (k v)
  (insert
   (format
    "(cl-puthash
(intern \"%s\")
 '%s
 emacspeak-speak-filter-table)\n" k v )))

(defcustom emacspeak-speak-filter-persistent-store
  (expand-file-name ".filters"
                    emacspeak-resource-directory)
  "File where emacspeak filters are persisted."
  :type 'file
  :group 'emacspeak-speak)

(defvar emacspeak-speak-filters-loaded-p nil
  "Records if we    have loaded filters in this session.")

(defun emacspeak-speak-lookup-persistent-filter (key)
  "Lookup a filter setting we may have persisted."
  (declare (special emacspeak-speak-filter-table))
  (gethash  (intern key) emacspeak-speak-filter-table))

(defun emacspeak-speak-set-persistent-filter (key value)
  "Persist filter setting for future use."
  (declare (special emacspeak-speak-filter-table))
  (setf (gethash  (intern key) emacspeak-speak-filter-table)
        value))

(defun emacspeak-speak-persist-filter-settings ()
  "Persist emacspeak filter settings for future sessions."
  (declare (special emacspeak-speak-filter-persistent-store
                    emacspeak-speak-filter-table))
  (let ((buffer (find-file-noselect
                 emacspeak-speak-filter-persistent-store)))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (maphash 'emacspeak-speak-persist-filter-entry
               emacspeak-speak-filter-table)
      (save-buffer)
      (kill-buffer buffer))))

(defsubst emacspeak-speak-load-filter-settings ()
  "Load emacspeak filter settings.."
  (declare (special emacspeak-speak-filter-persistent-store
                    emacspeak-speak-filter-table
                    emacspeak-speak-filters-loaded-p))
  (unless emacspeak-speak-filters-loaded-p
    (load-file emacspeak-speak-filter-persistent-store)
    (setq emacspeak-speak-filters-loaded-p t)
    (add-hook 'kill-emacs-hook
              'emacspeak-speak-persist-filter-settings)))

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
               (if  (buffer-file-name )
                   (emacspeak-speak-lookup-persistent-filter (buffer-file-name))
                 ""))))))
  (cond
   ((and (listp filter)
         (every
          #'(lambda (l)
              (and (listp l)
                   (= 2 (length l))))
          filter))
    (setq emacspeak-speak-line-column-filter filter)
    (when (buffer-file-name)
      (emacspeak-speak-set-persistent-filter (buffer-file-name) filter)))
   (t
    (message "Unset column filter")
    (setq emacspeak-speak-line-column-filter nil))))

;;}}}                                   ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;

(defcustom emacspeak-speak-space-regexp
  "^[ \t\r]+$"
  "Pattern that matches white space."
  :type 'string
  :group 'emacspeak)

(defun emacspeak-speak-line (&optional arg)
  "Speaks current line.  With prefix ARG, speaks the rest of the line
from point.  Negative prefix optional arg speaks from start of line to
point.  Voicifies if option `voice-lock-mode' is on.  Indicates
indentation with a tone if audio indentation is in use.  Indicates
position of point with an aural highlight if option
`emacspeak-show-point' is turned on --see command
`emacspeak-show-point' bound to \\[emacspeak-show-point].  Lines that
start hidden blocks of text, e.g.  outline header lines, or header
lines of blocks created by command `emacspeak-hide-or-expose-block'
are indicated with auditory icon ellipses."
  (interactive "P")
  (declare (special voice-lock-mode voice-animate
                    voice-indent dtk-stop-immediately
                    inhibit-field-text-motion
                    emacspeak-speak-line-invert-filter
                    dtk-punctuation-mode
                    emacspeak-speak-space-regexp
                    emacspeak-speak-maximum-line-length
                    emacspeak-show-point
                    emacspeak-decoration-rule emacspeak-horizontal-rule
                    emacspeak-unspeakable-rule emacspeak-audio-indentation))
  (when (listp arg) (setq arg (car arg )))
  (save-excursion
    (let ((inhibit-field-text-motion t)
          (start  nil)
          (end nil )
          (inhibit-point-motion-hooks t)
          (line nil)
          (orig (point))
          (indent nil))
      (beginning-of-line)
      (emacspeak-handle-action-at-point)
      (setq start (point))
      (end-of-line)
      (setq end (point))
                                        ;determine what to speak based on prefix arg
      (cond
       ((null arg))
       ((> arg 0) (setq start orig))
       (t (setq end orig)))
      (setq line
            (if emacspeak-show-point
                (ems-set-personality-temporarily
                 orig (1+ orig)
                 voice-animate
                 (buffer-substring  start end ))
              (buffer-substring start end )))
      (when (and emacspeak-audio-indentation
                 (null arg ))
        (let ((limit (line-end-position)))
          (beginning-of-line 1)
          (skip-syntax-forward " " limit)
          (setq indent  (current-column )))
        (when (eq emacspeak-audio-indentation-method 'tone)
          (emacspeak-indent indent )))
      (when
          (or (text-invisible-p end)
              (get-text-property  start 'emacspeak-hidden-block))
        (emacspeak-auditory-icon 'ellipses))
      (cond
       ((string-equal ""  line)         ;blank line
        (when dtk-stop-immediately (dtk-stop))
        (dtk-tone 250   75 'force)
        (when (emacspeak-using-midi-p)
          (emacspeak-play-midi-icon 'empty-line)))
       ((string-match  emacspeak-speak-space-regexp  line) ;only white space
        (when dtk-stop-immediately (dtk-stop))
        (dtk-tone 300   120 'force)
        (when (emacspeak-using-midi-p)
          (emacspeak-play-midi-icon 'blank-line)))
       ((and (not (eq 'all dtk-punctuation-mode))
             (string-match  emacspeak-horizontal-rule line)) ;horizontal rule
        (when dtk-stop-immediately (dtk-stop))
        (dtk-tone 350   100 'force)
        (when (emacspeak-using-midi-p)
          (emacspeak-play-midi-icon 'horizontal-rule)))
       ((and (not (eq 'all dtk-punctuation-mode))
             (string-match  emacspeak-decoration-rule line) ) ;decorative rule
        (when dtk-stop-immediately (dtk-stop))
        (dtk-tone 450   100 'force)
        (when (emacspeak-using-midi-p)
          (emacspeak-play-midi-icon 'decorative-rule)))
       ((and (not (eq 'all  dtk-punctuation-mode))
             (string-match  emacspeak-unspeakable-rule line) ) ;unspeakable rule
        (when dtk-stop-immediately (dtk-stop))
        (dtk-tone 550   100 'force)
        (when (emacspeak-using-midi-p)
          (emacspeak-play-midi-icon 'unspeakable-rule)))
       (t (let ((l (length line))
                (confirm nil))
            (when (or selective-display
                      (< l emacspeak-speak-maximum-line-length)
                      (get-text-property start 'emacspeak-speak-this-long-line)
                      (setq confirm
                            (y-or-n-p
                             (format "Speak  this  %s long line? " l))))
              (when confirm             ;update threshold
                (setq emacspeak-speak-maximum-line-length (1+ l))
                ;; record the y answer
                (ems-modify-buffer-safely
                 (put-text-property start end
                                    'emacspeak-speak-this-long-line t)))
              (when (and (null arg)
                         emacspeak-speak-line-column-filter)
                (setq line
                      (emacspeak-speak-line-apply-column-filter line
                                                                emacspeak-speak-line-invert-filter)))
              (if (and (eq 'speak emacspeak-audio-indentation-method )
                       (null arg )
                       indent
                       (> indent 0))
                  (progn
                    (setq indent (format "indent %d" indent))
                    (put-text-property   0 (length indent)
                                         'personality voice-indent   indent )
                    (dtk-speak (concat indent line)))
                (dtk-speak line)))))))))

(defvar emacspeak-speak-last-spoken-word-position nil
  "Records position of the last word that was spoken.
Local to each buffer.  Used to decide if we should spell the word
rather than speak it.")

(make-variable-buffer-local 'emacspeak-speak-last-spoken-word-position)
(defsubst emacspeak-speak-spell-word (word)
  "Spell WORD."
  (declare (special voice-lock-mode
                    voice-animate))
  (let ((result "")
        (char-string ""))
    (loop for char across word
          do
          (setq char-string (format "%c " char))
          (when (and (<= ?A char)
                     (<= char ?Z))
            (if voice-lock-mode
                (put-text-property 0 1
                                   'personality voice-animate
                                   char-string)
              (setq char-string (format "cap %s " char-string))))
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
spelt instead of being spoken."
  (interactive "P")
  (declare (special voice-lock-mode
                    emacspeak-speak-last-spoken-word-position))
  (when (listp arg) (setq arg (car arg )))
  (emacspeak-handle-action-at-point)
  (save-excursion
    (let ((orig (point))
          (inhibit-point-motion-hooks t)
          (start nil)
          (end nil)
          (speaker 'dtk-speak))
      (forward-word 1)
      (setq end (point))
      (backward-word 1)
      (setq start (min orig  (point)))
      (cond
       ((null arg ))
       ((> arg 0) (setq start orig))
       ((< arg 0) (setq end orig )))
      ;; select speak or spell
      (cond
       ((and (interactive-p)
             (eq emacspeak-speak-last-spoken-word-position orig))
        (setq speaker 'emacspeak-speak-spell-word)
        (setq emacspeak-speak-last-spoken-word-position nil))
       (t (setq  emacspeak-speak-last-spoken-word-position orig)))
      (funcall speaker  (buffer-substring  start end )))))

(defsubst emacspeak-is-alpha-p (c)
  "Check if argument C is an alphabetic character."
  (= 119 (char-syntax c)))

;;{{{  phonemic table

(defvar emacspeak-char-to-phonetic-table
  '(
    ("1"  . "one")
    ("2" .  "two")
    ("3" .  "three")
    ("4" .  "four")
    ("5" .  "five")
    ("6" .  "six")
    ("7" .  "seven")
    ("8" .  "eight")
    ("9" .  "nine")
    ("0".  "zero")
    ("a" . "alpha" )
    ("b" . "bravo")
    ("c" .  "charlie")
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
    ("A" . "cap alpha" )
    ("B" . "cap bravo")
    ("C" .  "cap charlie")
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
  (declare (special emacspeak-char-to-phonetic-table))
  (let ((char-string   (char-to-string char )))
    (or   (cdr
           (assoc char-string emacspeak-char-to-phonetic-table ))
          " ")))

;;}}}
(defun emacspeak-speak-char (&optional prefix)
  "Speak character under point.
Pronounces character phonetically unless  called with a PREFIX arg."
  (interactive "P")
  (let ((char  (following-char )))
    (when char
      (cond
       ((and (not prefix)
             (emacspeak-is-alpha-p char))
        (dtk-speak (emacspeak-get-phonetic-string char )))
       ((emacspeak-is-alpha-p char) (dtk-letter (char-to-string char )))
       (t (dtk-dispatch
           (dtk-char-to-speech char )))))))

(defun emacspeak-speak-this-char (char)
  "Speak this CHAR."
  (when char
    (cond
     ((emacspeak-is-alpha-p char) (dtk-letter (char-to-string char )))
     (t (dtk-dispatch
         (dtk-char-to-speech char ))))))

;;{{{ emacspeak-speak-display-char

(defun emacspeak-speak-display-char  (&optional prefix)
  "Display char under point using current speech display table.
Behavior is the same as command `emacspeak-speak-char'
bound to \\[emacspeak-speak-char]
for characters in the range 0--127.
Optional argument PREFIX  specifies that the character should be spoken phonetically."
  (interactive "P")
  (declare (special dtk-display-table ))
  (let ((char (following-char )))
    (cond
     ((and dtk-display-table
           (> char 127))
      (dtk-dispatch (aref dtk-display-table char)))
     (t (emacspeak-speak-char prefix)))))

;;}}}
;;{{{ emacspeak-speak-set-display-table

(defvar emacspeak-speak-display-table-list
  '(("iso ascii" . "iso ascii")
    ("default" . "default"))
  "Available speech display tables.")

(defun emacspeak-speak-set-display-table(&optional prefix)
  "Sets up buffer specific speech display table that controls how
special characters are spoken. Interactive prefix argument causes
setting to be global."
  (interactive "P")
  (declare (special dtk-display-table
                    dtk-iso-ascii-character-to-speech-table
                    emacspeak-speak-display-table-list))
  (let ((type (completing-read
               "Select speech display table: "
               emacspeak-speak-display-table-list
               nil t ))
        (table nil))
    (cond
     ((string-equal "iso ascii" type)
      (setq table dtk-iso-ascii-character-to-speech-table))
     (t (setq table nil)))
    (cond
     (prefix
      (setq-default dtk-display-table table )
      (setq dtk-display-table table))
     (t (setq dtk-display-table table)))))

;;}}}
(defun emacspeak-speak-sentence (&optional arg)
  "Speak current sentence.
With prefix ARG, speaks the rest of the sentence  from point.
Negative prefix arg speaks from start of sentence to point."
  (interactive "P" )
  (when (listp arg) (setq arg (car arg )))
  (save-excursion
    (let ((orig (point))
          (inhibit-point-motion-hooks t)
          (start nil)
          (end nil))
      (forward-sentence 1)
      (setq end (point))
      (backward-sentence 1)
      (setq start (point))
      (emacspeak-handle-action-at-point)
      (cond
       ((null arg ))
       ((> arg 0) (setq start orig))
       ((< arg 0) (setq end orig )))
      (dtk-speak (buffer-substring start end )))))

(defun emacspeak-speak-sexp (&optional arg)
  "Speak current sexp.
With prefix ARG, speaks the rest of the sexp  from point.
Negative prefix arg speaks from start of sexp to point.
If option  `voice-lock-mode' is on, then uses the personality."
  (interactive "P" )
  (when (listp arg) (setq arg (car arg )))
  (save-excursion
    (let ((orig (point))
          (inhibit-point-motion-hooks t)
          (start nil)
          (end nil))
      (condition-case nil
          (forward-sexp 1)
        (error nil ))
      (setq end (point))
      (condition-case nil
          (backward-sexp 1)
        (error nil ))
      (setq start (point))
      (emacspeak-handle-action-at-point)
      (cond
       ((null arg ))
       ((> arg 0) (setq start orig))
       ((< arg 0) (setq end orig )))
      (dtk-speak (buffer-substring  start end )))))

(defun emacspeak-speak-page (&optional arg)
  "Speak a page.
With prefix ARG, speaks rest of current page.
Negative prefix arg will read from start of current page to point.
If option  `voice-lock-mode' is on, then it will use any defined personality."
  (interactive "P")
  (when (listp arg) (setq arg (car arg )))
  (save-excursion
    (let ((orig (point))
          (inhibit-point-motion-hooks t)
          (start nil)
          (end nil))
      (mark-page)
      (setq start  (point))
      (emacspeak-handle-action-at-point)
      (setq end  (mark))
      (cond
       ((null arg ))
       ((> arg 0) (setq start orig))
       ((< arg 0) (setq end orig )))
      (dtk-speak (buffer-substring start end )))))

(defun emacspeak-speak-paragraph(&optional arg)
  "Speak paragraph.
With prefix arg, speaks rest of current paragraph.
Negative prefix arg will read from start of current paragraph to point.
If voice-lock-mode is on, then it will use any defined personality. "
  (interactive "P")
  (when (listp arg) (setq arg (car arg )))
  (save-excursion
    (let ((orig (point))
          (inhibit-point-motion-hooks t)
          (start nil)
          (end nil))
      (forward-paragraph 1)
      (setq end (point))
      (backward-paragraph 1)
      (setq start (point))
      (emacspeak-handle-action-at-point)
      (cond
       ((null arg ))
       ((> arg 0) (setq start orig))
       ((< arg 0) (setq end orig )))
      (dtk-speak (buffer-substring  start end )))))

;;}}}
;;{{{  Speak buffer objects such as help, completions minibuffer etc

(defun emacspeak-speak-buffer (&optional arg)
  "Speak current buffer  contents.
With prefix ARG, speaks the rest of the buffer from point.
Negative prefix arg speaks from start of buffer to point.
 If voice lock mode is on, the paragraphs in the buffer are
voice annotated first,  see command `emacspeak-speak-voice-annotate-paragraphs'."
  (interactive "P" )
  (declare (special emacspeak-speak-voice-annotated-paragraphs
                    inhibit-point-motion-hooks
                    voice-lock-mode))
  (let ((inhibit-point-motion-hooks t))
    (when (and voice-lock-mode
               (not emacspeak-speak-voice-annotated-paragraphs))
      (emacspeak-speak-voice-annotate-paragraphs))
    (when (listp arg) (setq arg (car arg )))
    (let ((start nil )
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
      (dtk-speak (buffer-substring start end )))))

(defun emacspeak-speak-other-buffer (buffer)
  "Speak specified buffer.
Useful to listen to a buffer without switching  contexts."
  (interactive
   (list
    (read-buffer "Speak buffer: "
                 nil t)))
  (save-excursion
    (set-buffer buffer)
    (emacspeak-speak-buffer)))

(defun emacspeak-speak-front-of-buffer()
  "Speak   the buffer from start to   point"
  (interactive)
  (emacspeak-speak-buffer -1))

(defun emacspeak-speak-rest-of-buffer()
  "Speak remainder of the buffer starting at point"
  (interactive)
  (emacspeak-auditory-icon 'select-object)
  (emacspeak-speak-buffer 1))

(defun emacspeak-speak-help(&optional arg)
  "Speak help buffer if one present.
With prefix arg, speaks the rest of the buffer from point.
Negative prefix arg speaks from start of buffer to point."
  (interactive "P")
  (declare (special voice-lock-mode
                    help-buffer-list))
  (let ((help-buffer
         (if (boundp 'help-buffer-list)
             (car help-buffer-list)
           (get-buffer "*Help*"))))
    (cond
     (help-buffer
      (save-excursion
        (set-buffer help-buffer)
        (voice-lock-mode 1)
        (emacspeak-speak-buffer arg )))
     (t (dtk-speak "First ask for help" )))))

(defun emacspeak-speak-completions()
  "Speak completions  buffer if one present."
  (interactive )
  (let ((completions-buffer (get-buffer "*Completions*"))
        (start nil)
        (end nil )
        (continue t))
    (cond
     ((and completions-buffer
           (window-live-p (get-buffer-window
                           completions-buffer )))
      (save-excursion
        (save-window-excursion
          (save-match-data
            (select-window  (get-buffer-window completions-buffer ))
            (goto-char (point-min))
            (forward-line 3)
            (while continue
              (setq start (point)
                    end (or  (re-search-forward "\\( +\\)\\|\n"  (point-max) t)
                             (point-max )))
              (dtk-speak (buffer-substring start end ) t) ;wait
              (setq continue  (sit-for 1))
              (if (eobp) (setq continue nil )))) ;end while
          (discard-input)
          (goto-char start )
          (choose-completion ))))
     (t (dtk-speak "No completions" )))))

(defun emacspeak-speak-minibuffer(&optional arg)
  "Speak the minibuffer contents
 With prefix arg, speaks the rest of the buffer from point.
Negative prefix arg speaks from start of buffer to point."
  (interactive "P" )
  (let ((minibuff (window-buffer (minibuffer-window ))))
    (save-excursion
      (set-buffer minibuff)
      (emacspeak-speak-buffer arg))))
(unless (fboundp 'next-completion)
  (progn
    (defun next-completion (n)
      "Move to the next item in the completion list.
WIth prefix argument N, move N items (negative N means move backward)."
      (interactive "p")
      (while (and (> n 0) (not (eobp)))
        (let ((prop (get-text-property (point) 'mouse-face))
              (end (point-max)))
          ;; If in a completion, move to the end of it.
          (if prop
              (goto-char (next-single-property-change (point) 'mouse-face nil end)))
          ;; Move to start of next one.
          (goto-char (next-single-property-change (point) 'mouse-face nil end)))
        (setq n (1- n)))
      )

    (defun previous-completion (n)
      "Move to the previous item in the completion list."
      (interactive "p")
      (setq n (- n ))
      (while (and (< n 0) (not (bobp)))
        (let ((prop (get-text-property (1- (point)) 'mouse-face))
              (end (point-min)))
          ;; If in a completion, move to the start of it.
          (if prop
              (goto-char (previous-single-property-change
                          (point) 'mouse-face nil end)))
          ;; Move to end of the previous completion.
          (goto-char (previous-single-property-change (point) 'mouse-face nil end))
          ;; Move to the start of that one.
          (goto-char (previous-single-property-change (point) 'mouse-face nil end)))
        (setq n (1+ n))))

    (declaim (special completion-list-mode-map))
    (or completion-list-mode-map
        (make-sparse-keymap ))
    (define-key completion-list-mode-map '[right] 'next-completion)
    (define-key completion-list-mode-map '[left] 'previous-completion)
    )) ;; end emacs pre-19.30 specials

(defun emacspeak-get-current-completion-from-completions  ()
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
  :group 'emacspeak-speak)

(defcustom emacspeak-voicemail-spool-file
  nil
  "Mail spool file examined  to alert you about newly
arrived voicemail."
  :type '(choice
          (const :tag "None" nil)
          (file :tag "VoiceMail drop location"))
  :group 'emacspeak-speak)

(defsubst emacspeak-get-file-size (filename)
  "Return file size for file FILENAME."
  (or (nth 7 (file-attributes filename))
      0))

(defvar emacspeak-mail-last-alerted-time (list 0 0)
  "Least  significant 16 digits of the time when mail alert was last issued.
Alert the user only if mail has arrived since this time in the
  future.")

(defsubst emacspeak-mail-get-last-mail-arrival-time (f)
  "Return time when mail  last arrived."
  (if (file-exists-p f)
      (nth 5 (file-attributes f ))
    0))

(defcustom emacspeak-mail-alert-interval  300
  "Interval in seconds between mail alerts for the same pending
  message."
  :type 'integer
  :group 'emacspeak-speak)
(unless (fboundp 'time-add )
  (defun time-add (t1 t2) ;;; for pre emacs 21.4
    "Add two time values.  One should represent a time difference."
    (let ((high (car t1))
          (low (if (consp (cdr t1)) (nth 1 t1) (cdr t1)))
          (micro (if (numberp (car-safe (cdr-safe (cdr t1))))
                     (nth 2 t1)
                   0))
          (high2 (car t2))
          (low2 (if (consp (cdr t2)) (nth 1 t2) (cdr t2)))
          (micro2 (if (numberp (car-safe (cdr-safe (cdr t2))))
                      (nth 2 t2)
                    0)))
      ;; Add
      (setq micro (+ micro micro2))
      (setq low (+ low low2))
      (setq high (+ high high2))

      ;; Normalize
      ;; `/' rounds towards zero while `mod' returns a positive number,
      ;; so we can't rely on (= a (+ (* 100 (/ a 100)) (mod a 100))).
      (setq low (+ low (/ micro 1000000) (if (< micro 0) -1 0)))
      (setq micro (mod micro 1000000))
      (setq high (+ high (/ low 65536) (if (< low 0) -1 0)))
      (setq low (logand low 65535))

      (list high low micro))))
(defsubst  emacspeak-mail-alert-user-p (f)
  "Predicate to check if we need to play an alert for the specified spool."
  (declare (special emacspeak-mail-last-alerted-time
                    emacspeak-mail-alert-interval))
  (let* ((mod-time (emacspeak-mail-get-last-mail-arrival-time f))
         (size (emacspeak-get-file-size f))
         (result (and (> size 0)
                      (or
                       (time-less-p emacspeak-mail-last-alerted-time mod-time) ; new mail
                       (time-less-p     ;unattended mail
                        (time-add emacspeak-mail-last-alerted-time
                                  (list 0 emacspeak-mail-alert-interval))
                        (current-time))))))
    (when result
      (setq emacspeak-mail-last-alerted-time  (current-time)))
    result))

(defun emacspeak-mail-alert-user ()
  "Alerts user about the arrival of new mail."
  (declare (special emacspeak-mail-spool-file emacspeak-voicemail-spool-file))
  (when (and emacspeak-mail-spool-file
             (emacspeak-mail-alert-user-p emacspeak-mail-spool-file))
    (emacspeak-auditory-icon 'new-mail))
  (when (and emacspeak-voicemail-spool-file
             (emacspeak-mail-alert-user-p emacspeak-voicemail-spool-file))
    (emacspeak-auditory-icon 'voice-mail)))

(defcustom emacspeak-mail-alert t
  "*Option to indicate cueing of new mail.
If t, emacspeak will alert you about newly arrived mail
with an auditory icon when
displaying the mode line.
You can use command
`emacspeak-toggle-mail-alert' bound to
\\[emacspeak-toggle-mail-alert] to set this option.
If you have online access to a voicemail drop, you can have a
  voice-mail alert set up by specifying the location of the
  voice-mail drop via custom option
emacspeak-voicemail-spool-file."
  :group 'emacspeak-speak
  :type 'boolean)

(ems-generate-switcher ' emacspeak-toggle-mail-alert
                         'emacspeak-mail-alert
                         "Toggle state of  Emacspeak  mail alert.
Interactive PREFIX arg means toggle  the global default value, and then set the
current local  value to the result.
Turning on this option results in Emacspeak producing an auditory icon
indicating the arrival  of new mail when displaying the mode line.")

;;}}}
;;{{{  Speak mode line information

;;;compute current line number
(defsubst emacspeak-get-current-line-number()
  (let ((start (point)))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (+ 1 (count-lines start (point)))))))

;;; make line-number-mode buffer local
(declaim (special line-number-mode))
(make-variable-buffer-local 'line-number-mode)
(setq-default line-number-mode nil)

;;; make column-number-mode buffer local
(declaim (special column-number-mode))
(make-variable-buffer-local 'column-number-mode)
(setq-default column-number-mode nil)
;;{{{ tone based  mode line speaker
(defvar emacspeak-which-function-mode  nil
  "*If T, speaking mode line speaks the name of function containing point.")

(make-variable-buffer-local 'emacspeak-which-function-mode)

(ems-generate-switcher ' emacspeak-toggle-which-function
                         'emacspeak-which-function-mode
                         "Toggle state of  Emacspeak  which function mode.
Interactive PREFIX arg means toggle  the global default value, and then set the
current local  value to the result.")

(defsubst emacspeak-speak-which-function ()
  "Speak which function we are on.  Uses which-function from
which-func without turning that mode on.  We actually use
semantic to do the work."
  (declare (special semantic--buffer-cache))
  (require 'which-func)
  (when  (and (featurep 'semantic)
              semantic--buffer-cache)
    (message  (or
               (which-function)
               "Not inside a function."))))

(defsubst ems-process-mode-line-format (spec)
  "Process mode line format spec."
  (cond
   ((symbolp spec) (symbol-value  spec))
   ((stringp spec) spec)
   ((and (listp spec)
         (stringp (car spec)))
    (concat
     (car spec)
     (ems-process-mode-line-format (cdr spec))))
   ((and (listp spec)
         (symbolp (car spec))
         (null (car spec)))
    (ems-process-mode-line-format (cdr spec)))
   ((and (listp spec)
         (eq :eval  (car spec)))
    (eval (cadr spec)))
   ((and (listp spec)
         (symbolp (car spec)))
    (concat
     (ems-process-mode-line-format (symbol-value (car spec)))
     (ems-process-mode-line-format (cdr spec))))))

(defun emacspeak-speak-buffer-info ()
  "Speak buffer information."
  (message "Buffer has %s lines and %s characters "
           (count-lines (point-min) (point-max))
           (- (point-max) (point-min))))

(defun emacspeak-speak-mode-line (&optional buffer-info)
  "Speak the mode-line.
Interactive prefix arg speaks buffer info."
  (interactive "P")
  (declare (special  mode-name  major-mode voice-annotate
                     emacspeak-which-function-mode global-mode-string
                     column-number-mode line-number-mode
                     emacspeak-mail-alert mode-line-format ))
  (cond
   (buffer-info (emacspeak-speak-buffer-info))
   (t
    (dtk-stop)
    (force-mode-line-update)
    (emacspeak-dtk-sync)
    (let ((dtk-stop-immediately nil )
          (global-info (ems-process-mode-line-format global-mode-string))
          (frame-info nil)
          (recursion-depth (recursion-depth))
          (recursion-info nil)
          (dir-info (when (or
                           (eq major-mode 'shell-mode)
                           (eq major-mode 'comint-mode))
                      default-directory)))
      (when (and  emacspeak-which-function-mode
                  (fboundp 'which-function)
                  (which-function))
        (emacspeak-speak-which-function))
      (when   emacspeak-mail-alert (emacspeak-mail-alert-user))
      (cond
       ((stringp mode-line-format) (dtk-speak mode-line-format ))
       (t                               ;process modeline
        (when dir-info
          (put-text-property 0 (length dir-info)
                             'personality voice-annotate dir-info))
        (cond
         ((> (length (frame-list)) 1)
          (setq frame-info
                (format " %s " (frame-parameter (selected-frame) 'name)))
          (put-text-property 0 (length frame-info)
                             'personality voice-smoothen frame-info))
         (t (setq frame-info "")))
        (when (> recursion-depth 0)
          (setq  recursion-info
                 (format " Recursive Edit %d "
                         recursion-depth))
          (put-text-property 0 (length recursion-info)
                             'personality voice-smoothen
                             recursion-info))
        (unless (and buffer-read-only
                     (buffer-modified-p)) ;avoid pathological case
          (when(and (not (eq major-mode 'shell-mode))
                    (not (eq major-mode 'comint-mode))
                    (buffer-modified-p))
            (dtk-tone 950 100))
          (when buffer-read-only (dtk-tone 250 100)))
        (put-text-property 0 (length global-info)
                           'personality voice-smoothen global-info)
        (tts-with-punctuations 'all
                               (dtk-speak
                                (concat dir-info
                                        (buffer-name)
                                        (when line-number-mode
                                          (format " line %d"
                                                  (emacspeak-get-current-line-number)))
                                        (when column-number-mode
                                          (format " Column %d"
                                                  (current-column)))
                                        mode-name
                                        (emacspeak-get-current-percentage-verbously)
                                        frame-info
                                        recursion-info
                                        global-info)))))))))

(defun emacspeak-speak-current-buffer-name ()
  "Speak name of current buffer."
  (tts-with-punctuations 'all
                         (dtk-speak
                          (buffer-name))))

;;}}}
;;;Helper --return string describing coding system info if
;;;relevant

(defvar emacspeak-speak-default-os-coding-system
  'raw-text-unix
  "Default coding system used for text files.
This should eventually be initialized based on the OS we are
running under.")

(defsubst ems-get-buffer-coding-system ()
  "Return buffer coding system info if releant.
If emacspeak-speak-default-os-coding-system is set and matches the
current coding system, then we return an empty string."
  (declare (special buffer-file-coding-system voice-annotate
                    emacspeak-speak-default-os-coding-system))
  (cond
   ((and (boundp 'buffer-file-coding-system)
         buffer-file-coding-system
         emacspeak-speak-default-os-coding-system
         (not (eq buffer-file-coding-system emacspeak-speak-default-os-coding-system)))
    (let ((value (format "%s" buffer-file-coding-system)))
      (put-text-property 0  (length value)
                         'personality
                         voice-annotate
                         value)
      value))
   (t "")))

(defvar emacspeak-minor-mode-prefix
  "Active minor modes "
  "Prefix used in composing utterance produced by emacspeak-speak-minor-mode-line.")

(put-text-property 0 (length emacspeak-minor-mode-prefix)
                   'personality voice-annotate emacspeak-minor-mode-prefix)

(defun emacspeak-speak-minor-mode-line ()
  "Speak the minor mode-information."
  (interactive)
  (declare (special minor-mode-alist
                    emacspeak-minor-mode-prefix
                    voice-lock-mode vc-mode))
  (force-mode-line-update)
  (let ((voice-lock-mode t)
        (info
         (mapcar
          #'(lambda(item)
              (let ((var (car item))
                    (value (cadr item )))
                (cond
                 ((and (boundp var) (eval var ))
                  (if (symbolp value) (eval value) value))
                 (t nil))))
          minor-mode-alist)))
    (setq info
          (mapcar
           #'(lambda (form)
               (cond
                ((and form
                      (listp form)
                      (eq :eval (car form)))
                 (eval (cadr form)))
                (t form)))
           info))
    (setq info (delq nil info))
    (setq info (delete "" info))
    (tts-with-punctuations "some"
                           (dtk-speak
                            (concat
                             emacspeak-minor-mode-prefix
                             vc-mode
                             (mapconcat #'identity info " ")
                             (ems-get-buffer-coding-system))))))

;;; obseleted by what-line in simple.el

(defun emacspeak-speak-line-number-obsolete ()
  "Speak the line number of the current line."
  (interactive)
  (message "line %d"
           (emacspeak-get-current-line-number)))

(defalias 'emacspeak-speak-line-number 'what-line)

(defun emacspeak-speak-buffer-filename (&optional filename)
  "Speak name of file being visited in current buffer.
Speak default directory if invoked in a dired buffer,
or when the buffer is not visiting any file.
Interactive prefix arg `filename' speaks only the final path
component.
The result is put in the kill ring for convenience."
  (interactive "P")
  (let ((location (or (buffer-file-name)
                      default-directory)))
    (when filename
      (setq location
            (file-name-nondirectory location)))
    (kill-new location)
    (dtk-speak
     location)))
;;}}}
;;{{{  Speak text without moving point

;;; Functions to browse without moving:
(defun emacspeak-read-line-internal(arg)
  "Read a line without moving.
Line to read is specified relative to the current line, prefix args gives the
offset. Default  is to speak the previous line. "
  (save-excursion
    (cond
     ((zerop arg) (emacspeak-speak-line ))
     ((zerop (forward-line arg))
      (emacspeak-speak-line ))
     (t (dtk-speak "Not that many lines in buffer ")))))

(defun emacspeak-read-previous-line(&optional arg)
  "Read previous line, specified by an offset, without moving.
Default is to read the previous line. "
  (interactive "p")
  (emacspeak-read-line-internal (- (or arg 1 ))))

(defun emacspeak-read-next-line(&optional arg)
  "Read next line, specified by an offset, without moving.
Default is to read the next line. "
  (interactive "p")
  (emacspeak-read-line-internal (or arg 1 )))

(defun emacspeak-read-word-internal(arg)
  "Read a word without moving.
word  to read is specified relative to the current word, prefix args gives the
offset. Default  is to speak the previous word. "
  (save-excursion
    (cond
     ((= arg 0) (emacspeak-speak-word ))
     ((forward-word arg)
      (skip-syntax-forward " ")
      (emacspeak-speak-word 1 ))
     (t (dtk-speak "Not that many words ")))))

(defun emacspeak-read-previous-word(&optional arg)
  "Read previous word, specified as a prefix arg, without moving.
Default is to read the previous word. "
  (interactive "p")
  (emacspeak-read-word-internal (- (or arg 1 ))))

(defun emacspeak-read-next-word(&optional arg)
  "Read next word, specified as a numeric  arg, without moving.
Default is to read the next word. "
  (interactive "p")
  (emacspeak-read-word-internal  (or arg 1 )))

;;}}}
;;{{{  Speak misc information e.g. time, version, current-kill  etc

(defcustom emacspeak-speak-time-format-string
  "%_I %M %p on %A, %B %_e, %Y "
  "*Format string that specifies how the time should be spoken.
See the documentation for function
`format-time-string'"
  :group 'emacspeak-speak
  :type 'string)
;;{{{ world clock

(defcustom emacspeak-speak-zoneinfo-directory
  "/usr/share/zoneinfo/"
  "Directory containing timezone data."
  :type 'directory
  :group 'emacspeak-speak)
;;;###autoload
(defun emacspeak-speak-world-clock (zone &optional set)
  "Display current date and time  for specified zone.
Optional second arg `set' sets the TZ environment variable as well."
  (interactive
   (list
    (let ((completion-ignore-case t)
          (ido-case-fold t))
      (substring
       (read-file-name
        "Timezone: "
        emacspeak-speak-zoneinfo-directory)
       (length emacspeak-speak-zoneinfo-directory)))
    current-prefix-arg))
  (declare (special emacspeak-speak-time-format-string
                    emacspeak-speak-zoneinfo-directory))
  (when (and set
             (= 16 (car set)))
    ;; two interactive prefixes from caller
    (setenv "TZ" zone))
  (shell-command
   (format "export TZ=%s; date +\"%s\""
           zone
           (concat emacspeak-speak-time-format-string
                   (format
                    " in %s, %%Z, %%z "
                    zone)))))

;;}}}
(defun emacspeak-speak-time (&optional world)
  "Speak the time.
Optional interactive prefix arg `C-u'invokes world clock.
Timezone is specified using minibuffer completion.
Second interactive prefix sets clock to new timezone."
  (interactive "P")
  (declare (special emacspeak-speak-time-format-string))
  (cond
   (world
    (call-interactively 'emacspeak-speak-world-clock))
   (t
    (tts-with-punctuations 'some
                           (dtk-speak
                            (propertize
                             (format-time-string
                              emacspeak-speak-time-format-string)
                             'personality voice-punctuations-some))))))

(defconst emacspeak-codename
  "LiveDog"
  "Code name of present release.")

(defun emacspeak-speak-version ()
  "Announce version information for running emacspeak."
  (interactive)
  (declare (special emacspeak-version
                    voice-animate voice-bold
                    emacspeak-sounds-directory
                    emacspeak-use-auditory-icons
                    emacspeak-codename))
  (let ((signature "You are using  ")
        (version (format "Emacspeak %s" emacspeak-version)))
    (put-text-property 0 (length version)
                       'personality voice-animate version)
    (put-text-property 0 (length emacspeak-codename)
                       'personality voice-bolden
                       emacspeak-codename)
    (when (and  emacspeak-use-auditory-icons
                (file-exists-p "/usr/bin/mpg123"))
      (start-process "mp3" nil "mpg123"
                     "-q"
                     (expand-file-name "emacspeak.mp3" emacspeak-sounds-directory)))
    (tts-with-punctuations 'some
                           (dtk-speak
                            (concat signature
                                    version
                                    emacspeak-codename)))))

(defun emacspeak-speak-current-kill (count)
  "Speak the current kill entry.
This is the text that will be yanked in by the next \\[yank].
Prefix numeric arg, COUNT, specifies that the text that will be yanked as a
result of a
\\[yank]  followed by count-1 \\[yank-pop]
be spoken.
 The kill number that is spoken says what numeric prefix arg to give
to command yank."
  (interactive "p")
  (let ((voice-lock-mode t)
        (context
         (format "kill %s "
                 (if current-prefix-arg (+ 1 count)  1 ))))
    (put-text-property 0 (length context)
                       'personality voice-annotate context )
    (dtk-speak
     (concat
      context
      (current-kill (if current-prefix-arg count 0)t)))))

(defun emacspeak-zap-tts ()
  "Send this command to the TTS directly."
  (interactive)
  (dtk-dispatch
   (read-from-minibuffer"Enter TTS command string: ")))

(defun emacspeak-speak-string-to-phone-number (string)
  "Convert alphanumeric phone number to true phone number.
Argument STRING specifies the alphanumeric phone number."
  (setq string (downcase string ))
  (let ((i 0))
    (loop for character across string
          do
          (aset string i
                (case character
                  (?a  ?2)
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
          (incf i))
    string))

(defun emacspeak-dial-dtk (number)
  "Prompt for and dial a phone NUMBER with the Dectalk."
  (interactive "sEnter phone number to dial:")
  (let ((dtk-stop-immediately nil))
    (dtk-dispatch (format "[:dial %s]"
                          (emacspeak-speak-string-to-phone-number number)))
    (sit-for 4)))

;;}}}
;;{{{ speaking marks

;;; Intelligent mark feedback for emacspeak:
;;;

(defun emacspeak-speak-current-mark (count)
  "Speak the line containing the mark.
With no argument, speaks the
line containing the mark--this is where `exchange-point-and-mark'
\\[exchange-point-and-mark] would jump.  Numeric prefix arg 'COUNT' speaks
line containing mark 'n' where 'n' is one less than the number of
times one has to jump using `set-mark-command' to get to this marked
position.  The location of the mark is indicated by an aural highlight
achieved by a change in voice personality."
  (interactive "p")
  (unless (mark)
    (error "No marks set in this buffer"))
  (when (and current-prefix-arg
             (> count (length mark-ring)))
    (error "Not that many marks in this buffer"))
  (let ((voice-lock-mode t)
        (line nil)
        (position nil)
        (context
         (format "mark %s "
                 (if current-prefix-arg count   0 ))))
    (put-text-property 0 (length context)
                       'personality voice-annotate context )
    (setq position
          (if current-prefix-arg
              (elt mark-ring(1-  count))
            (mark)))
    (save-excursion
      (goto-char position)
      (ems-set-personality-temporarily
       position (1+ position) voice-animate
       (setq line
             (thing-at-point  'line ))))
    (dtk-speak
     (concat context line))))

;;}}}
;;{{{ speaking personality chunks

(defun emacspeak-speak-this-personality-chunk ()
  "Speak chunk of text around point that has current
personality."
  (interactive)
  (let ((personality (get-text-property (point) 'personality))
        (start (previous-single-property-change (point) 'personality))
        (end (next-single-property-change  (point) 'personality)))
    (emacspeak-speak-region
     (or start (point-min))
     (or end (point-max)))))

(defun emacspeak-speak-next-personality-chunk ()
  "Moves to the front of next chunk having current personality.
Speak that chunk after moving."
  (interactive)
  (let ((personality (get-text-property (point) 'personality))
        (this-end (next-single-property-change (point)
                                               'personality))
        (next-start nil))
    (cond
     ((and (< this-end (point-max))
           (setq next-start
                 (text-property-any  this-end (point-max)
                                     'personality personality)))
      (goto-char next-start)
      (forward-char 1)
      (emacspeak-speak-this-personality-chunk))
     (t (error "No more chunks with current personality.")))))

;;; this helper is here since text-property-any doesn't work
;;; backwards

(defsubst ems-backwards-text-property-any (max min property
                                               value)
  "Scan backwards from max till we find specified property
                                               setting.
Return buffer position or nil on failure."
  (let ((result nil)
        (start nil)
        (continue t))
    (save-excursion
      (while (and continue
                  (not (bobp)))
        (backward-char 1)
        (setq start (previous-single-property-change  (point) property))
        (if (null start)
            (setq continue nil)
          (setq continue
                (not (eq  value
                          (get-text-property start property)))))
        (or continue
            (setq result start)))
      result)))

(defun emacspeak-speak-previous-personality-chunk ()
  "Moves to the front of previous chunk having current personality.
Speak that chunk after moving."
  (interactive)
  (let ((personality (get-text-property (point) 'personality))
        (this-start (previous-single-property-change (point) 'personality))
        (next-end nil))
    (cond
     ((and (> this-start (point-min))
           (setq next-end
                 (ems-backwards-text-property-any  (1- this-start) (point-min)
                                                   'personality personality)))
      (goto-char next-end)
      (backward-char 1)
      (emacspeak-speak-this-personality-chunk))
     (t (error "No previous  chunks with current personality.")))))

;;}}}
;;{{{  Execute command repeatedly, browse

(defun emacspeak-execute-repeatedly (command)
  "Execute COMMAND repeatedly."
  (interactive "CCommand to execute repeatedly:")
  (let ((key "")
        (position (point ))
        (continue t )
        (message (format "Press space to execute %s again" command)))
    (while continue
      (call-interactively command )
      (cond
       ((= (point) position ) (setq continue nil))
       (t (setq position (point))
          (setq key
                (let ((dtk-stop-immediately nil ))
                                        ;(sit-for 2)
                  (read-key-sequence message )))
          (when(and (stringp key)
                    (not (=  32  (string-to-char key ))))
            (dtk-stop)
            (setq continue nil )))))
    (dtk-speak "Exited continuous mode ")))

(defun emacspeak-speak-continuously ()
  "Speak a buffer continuously.
First prompts using the minibuffer for the kind of action to
perform after speaking each chunk.  E.G.  speak a line at a time
etc.  Speaking commences at current buffer position.  Pressing
\\[keyboard-quit] breaks out, leaving point on last chunk that
was spoken.  Any other key continues to speak the buffer."
  (interactive)
  (let ((command (key-binding
                  (read-key-sequence "Press key sequence to repeat: "))))
    (unless command
      (error "You specified an invalid key sequence.  " ))
    (emacspeak-execute-repeatedly command)))

(defun emacspeak-speak-browse-buffer (&optional browse)
  "Browse current buffer.
Default is to speak chunk having current personality.
Interactive prefix arg `browse'  repeatedly browses  through
  chunks having same personality as the current text chunk."
  (interactive "P")
  (cond
   (browse
    (emacspeak-execute-repeatedly
     'emacspeak-speak-next-personality-chunk))
   (t (emacspeak-speak-this-personality-chunk))))

(defvar emacspeak-read-line-by-line-quotient 10
  "Determines behavior of emacspeak-read-line-by-line.")

(defvar emacspeak-read-by-line-by-line-tick 1.0
  "Granularity of time for reading line-by-line.")

                                        ;(defun emacspeak-read-line-by-line ()
                                        ;  "Read line by line until interrupted"
                                        ;  (interactive)
                                        ;  (let ((count 0)
                                        ;        (line-length 0)
                                        ;        (continue t))
                                        ;    (while
                                        ;        (and continue
                                        ;             (not (eobp)))
                                        ;      (setq dtk-last-output "")
                                        ;      (call-interactively 'next-line)
                                        ;      (setq line-length (length  (thing-at-point 'line)))
                                        ;      (setq count 0)
                                        ;      (when (> line-length 0)
                                        ;        (while(and (< count
                                        ;                      (1+ (/ line-length emacspeak-read-line-by-line-quotient)))
                                        ;                   (setq continue
                                        ;                         (sit-for
                                        ;                          emacspeak-read-by-line-by-line-tick 0 nil ))
                                        ;                   (not (string-match  "done" dtk-last-output))
                                        ;                   (incf count))))))
                                        ;  (emacspeak-auditory-icon 'task-done)
                                        ;  (message "done moving "))

;;}}}
;;{{{  skimming

(defun emacspeak-speak-skim-paragraph()
  "Skim paragraph.
Skimming a paragraph results in the speech speeding up after
the first clause.
Speech is scaled by the value of dtk-speak-skim-scale"
  (interactive)
  (save-excursion
    (let ((inhibit-point-motion-hooks t)
          (start nil)
          (end nil))
      (forward-paragraph 1)
      (setq end (point))
      (backward-paragraph 1)
      (setq start (point))
      (dtk-speak (buffer-substring  start end )
                 'skim))))

(defun emacspeak-speak-skim-next-paragraph()
  "Skim next paragraph."
  (interactive)
  (forward-paragraph 1)
  (emacspeak-speak-skim-paragraph))

(defun emacspeak-speak-skim-buffer ()
  "Skim the current buffer  a paragraph at a time."
  (interactive)
  (emacspeak-execute-repeatedly 'emacspeak-speak-skim-next-paragraph))

;;}}}
;;{{{ comint

(defcustom emacspeak-comint-autospeak t
  "Says if comint output is automatically spoken.
You can use
  `emacspeak-toggle-comint-autospeak` bound to
  \\[emacspeak-toggle-comint-autospeak] to toggle this
setting."
  :group 'emacspeak-speak
  :type 'boolean)

(ems-generate-switcher ' emacspeak-toggle-comint-autospeak
                         'emacspeak-comint-autospeak
                         "Toggle state of Emacspeak comint autospeak.
When turned on, comint output is automatically spoken.  Turn this on if
you want your shell to speak its results.  Interactive
PREFIX arg means toggle the global default value, and then
set the current local value to the result.")

(defvar emacspeak-comint-output-monitor nil
  "Switch to monitor comint output.
When turned on,  comint output will be spoken even when the
buffer is not current or its window live.")

(make-variable-buffer-local
 'emacspeak-comint-output-monitor)
;;;###autoload
(ems-generate-switcher ' emacspeak-toggle-comint-output-monitor
                         'emacspeak-comint-output-monitor
                         "Toggle state of Emacspeak comint monitor.
When turned on, comint output is automatically spoken.  Turn this on if
you want your shell to speak its results.  Interactive
PREFIX arg means toggle the global default value, and then
set the current local value to the result.")

(defcustom emacspeak-comint-split-speech-on-newline  nil
  "*Option to have comint split speech on newlines.
Non-nil means we split speech on newlines in comint buffer."
  :group 'emacspeak-speak
  :type 'boolean)

(defun emacspeak-comint-speech-setup ()
  "Set up splitting of speech into chunks in comint modes."
  (declare (special
            emacspeak-comint-split-speech-on-newline ))
  (dtk-set-punctuations 'all)
  (when emacspeak-comint-split-speech-on-newline
    (modify-syntax-entry 10 ">"))
  (emacspeak-pronounce-refresh-pronunciations))

(add-hook 'comint-mode-hook 'emacspeak-comint-speech-setup)
(defvar emacspeak-speak-comint-output nil
  "Temporarily set to T by command
emacspeak-speak-comint-send-input.")

(defun emacspeak-speak-comint-send-input ()
  "Causes output to be spoken i.e., as if comint autospeak were turned
on."
  (interactive)
  (declare (special emacspeak-speak-comint-output))
  (setq emacspeak-speak-comint-output t)
  (call-interactively 'comint-send-input)
  (emacspeak-auditory-icon 'select-object))

;;}}}
;;{{{   quiten messages

(defcustom emacspeak-speak-messages t
  "*Option indicating if messages are spoken.  If nil,
emacspeak will not speak messages as they are echoed to the
message area.  You can use command
`emacspeak-toggle-speak-messages' bound to
\\[emacspeak-toggle-speak-messages]."

  :group 'emacspeak-speak
  :type 'boolean)

(ems-generate-switcher 'emacspeak-toggle-speak-messages
                       'emacspeak-speak-messages
                       "Toggle the state of whether emacspeak echoes messages.")

;;}}}
;;{{{  Moving across fields:

;;; For the present, we define a field
;;; as a contiguous series of non-blank characters
;;; helper function: speak a field
(defsubst  emacspeak-speak-field (start end )
  "Speaks field delimited by arguments START and END."
  (declare (special voice-annotate))
  (let ((header (or (get-text-property start  'field-name) "")))
    (dtk-speak
     (concat
      (progn (put-text-property 0 (length header )
                                'personality voice-annotate
                                header )
             header )
      " "
      (buffer-substring  start end)))))

(cond
 ;; emacs 21 defines fields
 ((fboundp 'field-beginning)
  (defun emacspeak-speak-current-field ()
    "Speak current field.
A field is
defined  by Emacs 21."
    (interactive)
    (emacspeak-speak-region (field-beginning)
                            (field-end))))
 (t
  (defun emacspeak-speak-current-field ()
    "Speak current field.
A field is defined currently as a sequence of non-white space characters.  may be made
  mode specific later."
    (interactive)
    (cond
     ((window-minibuffer-p (selected-window))
      (emacspeak-speak-line))
     (t (let ((start nil ))
          (save-excursion
            (skip-syntax-backward "^ ")
            (setq start (point ))
            (skip-syntax-forward "^ ")
            (emacspeak-speak-field start (point )))))))))

(defun emacspeak-speak-next-field ()
  "Skip across and speak the next contiguous sequence of non-blank characters.
Useful in moving across fields.
Will be improved if it proves useful."
  (interactive)
  (declare (special inhibit-field-text-motion))
  (let((inhibit-field-text-motiont)
       (start nil ))
    (skip-syntax-forward "^ ")
    (skip-syntax-forward " ")
    (setq start (point ))
    (save-excursion
      (skip-syntax-forward "^ ")
      (emacspeak-speak-field start (point)))))

(defun emacspeak-speak-previous-field ()
  "Skip backwards across and speak  contiguous sequence of non-blank characters.
Useful in moving across fields.
Will be improved if it proves useful."
  (interactive)
  (declare (special inhibit-field-text-motion))
  (let ((inhibit-field-text-motion t)
        (start nil ))
    (skip-syntax-backward " ")
    (setq start (point ))
    (skip-syntax-backward "^ ")
    (emacspeak-speak-field (point ) start)))

(defun emacspeak-speak-current-column ()
  "Speak the current column."
  (interactive)
  (message "Point at column %d" (current-column )))

(defun emacspeak-speak-current-percentage ()
  "Announce the percentage into the current buffer."
  (interactive)
  (message "Point is  %d%% into  the current buffer"
           (emacspeak-get-current-percentage-into-buffer )))

;;}}}
;;{{{  Speak the last message again:

(defcustom emacspeak-speak-message-again-should-copy-to-kill-ring t
  "If set, asking for last message will copy it to the kill ring."
  :type 'boolean
  :group 'emacspeak-speak)

(defun emacspeak-speak-message-again (&optional from-message-cache)
  "Speak the last message from Emacs once again.
Optional interactive prefix arg
`from-message-cache' speaks message cached from the most
recent call to function `message'.
The message is also placed in the kill ring for convenient yanking
if `emacspeak-speak-message-again-should-copy-to-kill-ring' is set."
  (interactive "P")
  (declare (special emacspeak-last-message
                    emacspeak-speak-message-again-should-copy-to-kill-ring))
  (cond
   (from-message-cache
    (dtk-speak   emacspeak-last-message )
    (when (and (interactive-p)
               emacspeak-speak-message-again-should-copy-to-kill-ring)
      (kill-new emacspeak-last-message)))
   (t (save-excursion
        (set-buffer "*Messages*")
        (goto-char (point-max))
        (skip-syntax-backward " ")
        (emacspeak-speak-line)
        (when (and (interactive-p)
                   emacspeak-speak-message-again-should-copy-to-kill-ring)
          (kill-new
           (buffer-substring (line-beginning-position)
                             (line-end-position))))))))

(defun emacspeak-announce (announcement)
  "Speak the ANNOUNCEMENT, if possible.
Otherwise just display a message."
  (message announcement))

;;}}}
;;{{{  Using emacs's windows usefully:

;;Return current window contents
(defsubst emacspeak-get-window-contents ()
  "Return window contents."
  (let ((start nil))
    (save-excursion
      (move-to-window-line 0)
      (setq start (point))
      (move-to-window-line -1)
      (end-of-line)
      (buffer-substring start (point)))))

(defun emacspeak-speak-window-information ()
  "Speaks information about current window."
  (interactive)
  (message "Current window has %s lines and %s columns with
top left %s %s "
           (window-height)
           (window-width)
           (first (window-edges))
           (second (window-edges))))

(defun emacspeak-speak-current-window ()
  "Speak contents of current window.
Speaks entire window irrespective of point."
  (interactive)
  (emacspeak-speak-region (window-start) (window-end )))

(defun emacspeak-speak-other-window (&optional arg)
  "Speak contents of `other' window.
Speaks entire window irrespective of point.
Semantics  of `other' is the same as for the builtin Emacs command
`other-window'.
Optional argument ARG  specifies `other' window to speak."
  (interactive "nSpeak window")
  (save-excursion
    (save-window-excursion
      (other-window arg )
      (save-excursion
        (set-buffer (window-buffer))
        (emacspeak-speak-region
         (max (point-min) (window-start) )
         (min (point-max)(window-end )))))))

(defun emacspeak-speak-next-window ()
  "Speak the next window."
  (interactive)
  (emacspeak-speak-other-window 1 ))

(defun emacspeak-speak-previous-window ()
  "Speak the previous window."
  (interactive)
  (emacspeak-speak-other-window -1 ))

(defun  emacspeak-owindow-scroll-up ()
  "Scroll up the window that command `other-window' would move to.
Speak the window contents after scrolling."
  (interactive)
  (let ((window (selected-window)))
    (other-window 1)
    (call-interactively 'scroll-up)
    (select-window window)))

(defun  emacspeak-owindow-scroll-down ()
  "Scroll down  the window that command `other-window' would move to.
Speak the window contents after scrolling."
  (interactive)
  (let ((window (selected-window)))
    (other-window 1)
    (call-interactively 'scroll-down)
    (select-window window)))

(defun emacspeak-owindow-next-line (count)
  "Move to the next line in the other window and speak it.
Numeric prefix arg COUNT can specify number of lines to move."
  (interactive "p")
  (setq count (or count 1 ))
  (let  ((residue nil )
         (old-buffer (current-buffer )))
    (unwind-protect
        (progn
          (set-buffer (window-buffer (next-window )))
          (end-of-line)
          (setq residue (forward-line count))
          (cond
           ((> residue 0) (message "At bottom of other window "))
           (t (set-window-point (get-buffer-window (current-buffer ))
                                (point))
              (emacspeak-speak-line ))))
      (set-buffer old-buffer ))))

(defun emacspeak-owindow-previous-line (count)
  "Move to the next line in the other window and speak it.
Numeric prefix arg COUNT specifies number of lines to move."
  (interactive "p")
  (setq count (or count 1 ))
  (let  ((residue nil )
         (old-buffer (current-buffer )))
    (unwind-protect
        (progn
          (set-buffer (window-buffer (next-window )))
          (end-of-line)
          (setq residue (forward-line (- count)))
          (cond
           ((> 0 residue) (message "At top of other window "))
           (t (set-window-point (get-buffer-window (current-buffer ))
                                (point))
              (emacspeak-speak-line ))))
      (set-buffer old-buffer ))))

(defun emacspeak-owindow-speak-line ()
  "Speak the current line in the other window."
  (interactive)
  (let  ((old-buffer (current-buffer )))
    (unwind-protect
        (progn
          (set-buffer (window-buffer (next-window )))
          (goto-char (window-point ))
          (emacspeak-speak-line))
      (set-buffer old-buffer ))))
(defun emacspeak-speak-predefined-window (&optional arg)
  "Speak one of the first 10 windows on the screen.
Speaks entire window irrespective of point.
In general, you'll never have Emacs split the screen into more than
two or three.
Argument ARG determines the 'other' window to speak.
Semantics  of `other' is the same as for the builtin Emacs command
`other-window'."
  (interactive "P")
  (let* ((window-size-change-functions nil)
         (window
          (cond
           ((not (interactive-p)) arg)
           (t
            (condition-case nil
                (read (format "%c" last-input-event ))
              (error nil ))))))
    (or (numberp window)
        (setq window
              (read-minibuffer "Window   between 1 and 9 to
speak")))
    (setq window (1- window))
    (save-excursion
      (save-window-excursion
        (other-window window )
        (emacspeak-speak-region (window-start) (window-end ))))))

;;}}}
;;{{{  Intelligent interactive commands for reading:

;;; Prompt the user if asked to prompt.
;;; Prompt is:
;;; press 'b' for beginning of unit,
;;; 'r' for rest of unit,
;;; any other key for entire unit
;;; returns 1, -1, or nil accordingly.
;;; If prompt is nil, does not prompt: just gets the input

(defun emacspeak-ask-how-to-speak (unit-name prompt)
  "Argument UNIT-NAME specifies kind of unit that is being spoken.
Argument PROMPT specifies the prompt to display."

  (if prompt
      (message
       (format "Press s to speak start of %s, r for rest of  %s. \
 Any  key for entire %s "
               unit-name unit-name unit-name )))
  (let ((char (read-char )))
    (cond
     ((= char ?s) -1)
     ((= char ?r) 1)
     (t nil )))
  )

(defun emacspeak-speak-buffer-interactively ()
  "Speak the start of, rest of, or the entire buffer.
's' to speak the start.
'r' to speak the rest.
any other key to speak entire buffer."
  (interactive)
  (emacspeak-speak-buffer
   (emacspeak-ask-how-to-speak "buffer" (sit-for 1 0 nil ))))

(defun emacspeak-speak-help-interactively ()
  "Speak the start of, rest of, or the entire help.
's' to speak the start.
'r' to speak the rest.
any other key to speak entire help."
  (interactive)
  (emacspeak-speak-help
   (emacspeak-ask-how-to-speak "help" (sit-for 1 0 nil ))))

(defun emacspeak-speak-line-interactively ()
  "Speak the start of, rest of, or the entire line.
's' to speak the start.
'r' to speak the rest.
any other key to speak entire line."
  (interactive)
  (emacspeak-speak-line
   (emacspeak-ask-how-to-speak "line" (sit-for 1 0 nil ))))

(defun emacspeak-speak-paragraph-interactively ()
  "Speak the start of, rest of, or the entire paragraph.
's' to speak the start.
'r' to speak the rest.
any other key to speak entire paragraph."
  (interactive)
  (emacspeak-speak-paragraph
   (emacspeak-ask-how-to-speak "paragraph" (sit-for 1 0 nil ))))

(defun emacspeak-speak-page-interactively ()
  "Speak the start of, rest of, or the entire page.
's' to speak the start.
'r' to speak the rest.
any other key to speak entire page."
  (interactive)
  (emacspeak-speak-page
   (emacspeak-ask-how-to-speak "page" (sit-for 1 0 nil ))))

(defun emacspeak-speak-word-interactively ()
  "Speak the start of, rest of, or the entire word.
's' to speak the start.
'r' to speak the rest.
any other key to speak entire word."
  (interactive)
  (emacspeak-speak-word
   (emacspeak-ask-how-to-speak "word" (sit-for 1 0 nil ))))

(defun emacspeak-speak-sexp-interactively ()
  "Speak the start of, rest of, or the entire sexp.
's' to speak the start.
'r' to speak the rest.
any other key to speak entire sexp."
  (interactive)
  (emacspeak-speak-sexp
   (emacspeak-ask-how-to-speak "sexp" (sit-for 1 0 nil ))))

;;}}}
;;{{{  emacs' register related commands

;;; Things like view-register are useful.

(defun emacspeak-view-register ()
  "Display the contents of a register, and then speak it."
  (interactive)
  (call-interactively 'view-register)
  (save-excursion (set-buffer "*Output*")
                  (dtk-speak (buffer-string ))))

;;}}}
;;{{{  emacs rectangles and regions:

(eval-when (compile) (require 'rect))
;;; These help you listen to columns of text. Useful for tabulated data
(defun emacspeak-speak-rectangle ( start end )
  "Speak a rectangle of text.
Rectangle is delimited by point and mark.
When call from a program,
arguments specify the START and END of the rectangle."
  (interactive  "r")
  (require 'rect)
  (dtk-speak-list (extract-rectangle start end )))

;;; helper function: emacspeak-put-personality
;;; sets property 'personality to personality
(defsubst emacspeak-put-personality (start end personality )
  "Apply specified personality to region delimited by START and END.
Argument PERSONALITY gives the value for property personality."
  (put-text-property start end 'personality personality ))

;;; Compute table of possible voices to use in completing-read
;;; We rely on dectalk-voice-table as our default voice table.
;;; Names defined in this --- and other voice tables --- are
;;; generic --and  not device specific.
;;;

(defsubst  emacspeak-possible-voices ()
  "Return possible voices."
  (declare (special dectalk-voice-table ))
  (loop for key being the hash-keys of dectalk-voice-table
        collect  (cons
                  (symbol-name key)
                  (symbol-name key))))

(defun emacspeak-voicify-rectangle (start end &optional personality )
  "Voicify the current rectangle.
When calling from a program,arguments are
START END personality
Prompts for PERSONALITY  with completion when called interactively."
  (interactive "r")
  (declare (special voice-lock-mode))
  (require 'rect)
  (require 'emacspeak-personality )
  (or voice-lock-mode (setq voice-lock-mode t ))
  (let ((personality-table (emacspeak-possible-voices )))
    (when (interactive-p)
      (setq personality
            (read
             (completing-read "Use personality: "
                              personality-table nil t ))))
    (ems-modify-buffer-safely
     (operate-on-rectangle
      (function (lambda ( start-seg begextra endextra )
                  (emacspeak-put-personality start-seg  (point) personality )))
      start end  nil))))

(defun emacspeak-voicify-region (start end &optional personality )
  "Voicify the current region.
When calling from a program,arguments are
START END personality.
Prompts for PERSONALITY  with completion when called interactively."
  (interactive "r")
  (declare (special voice-lock-mode))
  (require 'emacspeak-personality )
  (or voice-lock-mode (setq voice-lock-mode t ))
  (let ((personality-table (emacspeak-possible-voices )))
    (when (interactive-p)
      (setq personality
            (read
             (completing-read "Use personality: "
                              personality-table nil t ))))
    (put-text-property start end 'personality personality )))

(defun emacspeak-put-text-property-on-rectangle   (start end prop value )
  "Set property to specified value for each line in the rectangle.
Argument START and END specify the rectangle.
Argument PROP specifies the property and VALUE gives the
value to apply."
  (require 'rect)
  (operate-on-rectangle
   (function (lambda ( start-seg begextra endextra )
               (put-text-property  start-seg (point)    prop value  )))
   start end  nil ))

;;}}}
;;{{{  Matching delimiters:

;;; A modified blink-matching-open that always displays the matching line
;;; in the minibuffer so emacspeak can speak it.

(defun emacspeak-blink-matching-open ()
  "Display matching delimiter in the minibuffer."
  (interactive)
  (declare (special blink-matching-paren-distance))
  (and (> (point) (1+ (point-min)))
       (not (memq (char-syntax (char-after (- (point) 2))) '(?/ ?\\ )))
       blink-matching-paren
       (let* ((oldpos (point))
              (emacspeak-blink-delay 5)
              (blinkpos)
              (mismatch))
         (save-excursion
           (save-restriction
             (if blink-matching-paren-distance
                 (narrow-to-region (max (point-min)
                                        (- (point) blink-matching-paren-distance))
                                   oldpos))
             (condition-case ()
                 (setq blinkpos (scan-sexps oldpos -1))
               (error nil)))
           (and blinkpos (/= (char-syntax (char-after blinkpos))
                             ?\$)
                (setq mismatch
                      (/= (char-after (1- oldpos))
                          (matching-paren (char-after blinkpos)))))
           (if mismatch (setq blinkpos nil))
           (if blinkpos
               (progn
                 (goto-char blinkpos)
                 (message
                  "Matches %s"
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
                                          (progn (end-of-line) (point)))
                      ;; Otherwise show the previous nonblank line.
                      (concat
                       (buffer-substring (progn
                                           (backward-char 1)
                                           (skip-chars-backward "\n \t")
                                           (line-beginning-position))
                                         (progn (end-of-line)
                                                (skip-chars-backward " \t")
                                                (point)))
                       ;; Replace the newline and other whitespace with `...'.
                       "..."
                       (buffer-substring blinkpos (1+
                                                   blinkpos)))))))
             (cond (mismatch
                    (message "Mismatched parentheses"))
                   ((not blink-matching-paren-distance)
                    (message "Unmatched parenthesis")))))
         (sit-for emacspeak-blink-delay))))

(defun  emacspeak-use-customized-blink-paren ()
  "A customized blink-paren to speak  matching opening paren.
We need to call this in case Emacs
is anal and loads its own builtin blink-paren function
which does not talk."
  (interactive)
  (fset 'blink-matching-open (symbol-function 'emacspeak-blink-matching-open))
  (and (interactive-p)
       (message "Using customized blink-paren function provided by Emacspeak.")))

(emacspeak-use-customized-blink-paren)

;;}}}
;;{{{  Auxillary functions:

(defsubst emacspeak-kill-buffer-carefully (buffer)
  "Kill BUFFER BUF if it exists."
  (and buffer
       (get-buffer buffer)
       (buffer-name (get-buffer buffer ))
       (kill-buffer buffer)))

(defsubst emacspeak-overlay-get-text (o)
  "Return text under overlay OVERLAY.
Argument O specifies overlay."
  (save-excursion
    (set-buffer (overlay-buffer o ))
    (buffer-substring
     (overlay-start o)
     (overlay-end o ))))

;;}}}
;;{{{ Speaking spaces

(defun emacspeak-speak-spaces-at-point ()
  "Speak the white space at point."
  (interactive)
  (cond
   ((not (= 32 (char-syntax (following-char ))))
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
               (1+ (- orig start)) (- end start ))))))

;;}}}
;;{{{  translate faces to voices

(defun voice-lock-voiceify-faces ()
  "Map faces to personalities."
  (declare (special voice-lock-mode))
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t )
          (face nil )
          (start (point)))
      (setq voice-lock-mode t)
      (unwind-protect
          (while (not (eobp))
            (setq face (get-text-property (point) 'face ))
            (goto-char
             (or (next-single-property-change (point) 'face )
                 (point-max)))
            (put-text-property start  (point)
                               'personality
                               (if (listp face)
                                   (car face)
                                 face ))
            (setq start (point)))
        (setq inhibit-read-only nil)))))

;;}}}
;;{{{  completion helpers

;;{{{ switching to completions window from minibuffer:

(defsubst emacspeak-get-minibuffer-contents ()
  "Return contents of the minibuffer."
  (let ((inhibit-field-text-motion t))
    (save-excursion
      (set-buffer (window-buffer (minibuffer-window)))
      (buffer-substring (field-beginning)
                        (field-end)))))

;;; Make all occurrences of string inaudible
(defsubst emacspeak-make-string-inaudible(string)
  (unless (string-match "^ *$" string)
    (save-excursion
      (goto-char (point-min))
      (save-match-data
        (ems-modify-buffer-safely
         (while (search-forward string nil t)
           (put-text-property (match-beginning 0)
                              (match-end 0)
                              'personality 'inaudible)))))))

(defvar emacspeak-completions-current-prefix nil
  "Prefix typed in the minibuffer before completions was invoked.")

(make-variable-buffer-local 'emacspeak-completions-current-prefix)

(defun emacspeak-switch-to-completions-window ()
  "Jump to the *Completions* buffer if it is active.
We make the current minibuffer contents (which is obviously the
prefix for each entry in the completions buffer) inaudible
to reduce chatter."
  (interactive)
  (declare (special voice-lock-mode
                    emacspeak-completions-current-prefix))
  (let ((completions-buffer (get-buffer "*Completions*"))
        (current-entry (emacspeak-get-minibuffer-contents)))
    (cond
     ((and completions-buffer
           (window-live-p (get-buffer-window completions-buffer )))
      (select-window  (get-buffer-window completions-buffer ))
      (when (interactive-p)
        (setq voice-lock-mode t)
        (message current-entry)
        (when (and  current-entry
                    (> (length current-entry) 0))
          (setq emacspeak-completions-current-prefix current-entry)
          (emacspeak-make-string-inaudible current-entry))
        (dtk-toggle-splitting-on-white-space)
        (dtk-speak
         (emacspeak-get-current-completion-from-completions)))
      (emacspeak-auditory-icon 'select-object))
     (t (message "No completions")))))

(defun emacspeak-completions-move-to-completion-group()
  "Move to group of choices beginning with character last
typed. If no such group exists, then we dont move. "
  (interactive)
  (declare (special last-input-char
                    emacspeak-completions-current-prefix))
  (let ((pattern (format "[ \t\n]%s%c"
                         (or
                          emacspeak-completions-current-prefix "")
                         last-input-char))
        (case-fold-search t))
    (when (re-search-forward pattern nil t)
      (emacspeak-auditory-icon 'search-hit))
    (dtk-speak
     (emacspeak-get-current-completion-from-completions ))))
(declaim (special completion-list-mode-map))
(let ((chars
       "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
  (loop for char across chars
        do
        (define-key completion-list-mode-map
          (format "%c" char)
          'emacspeak-completions-move-to-completion-group)))

;;}}}

;;}}}
;;{{{ mark convenience commands

(defsubst emacspeak-mark-speak-mark-line()
  (declare (special voice-animate))
  (emacspeak-auditory-icon 'mark-object )
  (ems-set-personality-temporarily (point) (1+ (point))
                                   voice-animate
                                   (emacspeak-speak-line)))

(defun emacspeak-mark-forward-mark ()
  "Cycle forward through the mark ring."
  (interactive)
  (set-mark-command t)
  (when (interactive-p )
    (emacspeak-mark-speak-mark-line)))

(defun emacspeak-mark-backward-mark ()
  "Cycle backward through the mark ring."
  (interactive)
  (declare (special mark-ring))
  (let ((target  (car (last mark-ring ))))
    (cond
     (target
      (setq mark-ring
            (cons (copy-marker (mark-marker))
                  (butlast mark-ring 1)))
      (set-marker (mark-marker) (+ 0 target)
                  (current-buffer))
      (move-marker target nil)
      (goto-char (mark t))
      (when (interactive-p)
        (emacspeak-mark-speak-mark-line)))
     (t (message "No previous mark to move to")))))

;;}}}
;;{{{ customize emacspeak

;;}}}
;;{{{ speaking an extent of text delimited by specified char

(defun emacspeak-speak-and-skip-extent-upto-char (char)
  "Search forward from point until we hit char.
Speak text between point and the char we hit."
  (interactive "c")
  (let ((start (point))
        (goal nil))
    (save-excursion
      (cond
       ((search-forward (format "%c" char)
                        (point-max)
                        'no-error)
        (setq goal (point))
        (emacspeak-speak-region start goal)
        (emacspeak-auditory-icon 'select-object))
       (t (error "Could not find %c" char))))
    (when goal (goto-char goal))))

(defun emacspeak-speak-and-skip-extent-upto-this-char ()
  "Speak extent delimited by point and last character typed."
  (interactive)
  (declare (special last-input-char))
  (emacspeak-speak-and-skip-extent-upto-char last-input-char))

;;}}}
;;{{{  speak message at time
(defun emacspeak-speak-message-at-time (time message)
  "Set up ring-at-time to speak message at specified time.
Provides simple stop watch functionality in addition to other things.
See documentation for command run-at-time for details on time-spec."
  (interactive
   (list
    (read-from-minibuffer "Time specification:  ")
    (read-from-minibuffer "Message: ")))
  (run-at-time time nil
               #'(lambda (m)
                   (message m)
                   (emacspeak-auditory-icon 'alarm))
               message))

;;}}}
;;{{{ Directory specific settings
(defcustom  emacspeak-speak-load-directory-settings-quietly t
  "*User option that affects loading of directory specific settings.
If set to T,Emacspeak will not prompt before loading
directory specific settings."
  :group 'emacspeak-speak
  :type 'boolean)

(defcustom emacspeak-speak-directory-settings
  ".espeak.el"
  "*Name of file that holds directory specific settings."
  :group 'emacspeak-speak
  :type 'string)

(defsubst emacspeak-speak-root-dir-p (dir)
  "Check if we are at the root of the filesystem."
  (let ((parent (expand-file-name  "../" dir)))
    (or (or (not (file-readable-p dir))
            (not (file-readable-p parent)))
        (and
         (string-equal (file-truename dir) "/")
         (string-equal (file-truename parent) "/")))))

(defun emacspeak-speak-get-directory-settings (dir)
  "Finds the next directory settings  file upwards in the directory tree
from DIR. Returns nil if it cannot find a settings file in DIR
or an ascendant directory."
  (declare (special emacspeak-speak-directory-settings
                    default-directory))
  (let ((file (find emacspeak-speak-directory-settings
                    (directory-files dir)
                    :test 'string-equal)))
    (cond
     (file (expand-file-name file dir))
     ((not (emacspeak-speak-root-dir-p dir))
      (emacspeak-speak-get-directory-settings (expand-file-name ".." dir)))
     (t nil))))

;;;###autoload
(defun emacspeak-speak-load-directory-settings (&optional directory)
  "Load a directory specific Emacspeak settings file.
This is typically used to load up settings that are specific to
an electronic book consisting of many files in the same
directory."
  (interactive "%DDirectory:")
  (or directory
      (setq directory default-directory))
  (let ((settings (emacspeak-speak-get-directory-settings directory)))
    (when (and settings
               (file-exists-p  settings)
               (or emacspeak-speak-load-directory-settings-quietly
                   (y-or-n-p "Load directory settings? ")
                   "Load  directory specific Emacspeak
settings? "))
      (condition-case nil
          (load-file settings)
        (error (message "Error loading settings %s" settings))))))

;;}}}
(provide 'emacspeak-speak )
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
