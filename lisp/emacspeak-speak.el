;;; emacspeak-speak.el --- Implements Emacspeak's core speech services  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Contains the functions for speaking various chunks of text
;;; Keywords: Emacspeak,  Spoken Output
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2008-08-18 16:25:05 -0700 (Mon, 18 Aug 2008) $ |
;;;  $Revision: 4552 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;;Copyright (C) 1995 -- 2018, T. V. Raman
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

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))

(require 'dtk-speak)
(require 'emacspeak-pronounce)
(require 'sox-gen)
(declare-function emacspeak-play-auditory-icon "emacspeak-sounds" (sound-name))
(declare-function operate-on-rectangle "rect" (function start end coerce-tabs))
(declare-function which-function "which-func" nil)
(declare-function calendar-cursor-to-nearest-date "cal-move" [Arg list not available until function definition is loaded.])
(declare-function word-at-point "thingatpt" (&optional no-properties))

;;}}}
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


;}}}
;;{{{  Speak mode line information

;;;compute current line number
(defun emacspeak-get-current-line-number ()
  (let ((start (point)))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (+ 1 (count-lines start (point)))))))

;;; make line-number-mode buffer local
(cl-declaim (special line-number-mode))
(make-variable-buffer-local 'line-number-mode)
(setq-default line-number-mode nil)

;;; make column-number-mode buffer local
(cl-declaim (special column-number-mode))
(make-variable-buffer-local 'column-number-mode)
(setq-default column-number-mode nil)
;;{{{   mode line speaker

(defun emacspeak-speak-which-function ()
  "Speak which function we are on.  Uses which-function from
which-func without turning that mode on.  We actually use
semantic to do the work."
  (interactive)
  (require 'semantic "semantic")
    (require 'which-func)
    (message (or
              (which-function)
              "Not inside a function.")))

(defun emacspeak-speak-buffer-info ()
  "Speak buffer information."
  (message "Buffer has %s lines and %s characters %s "
           (count-lines (point-min) (point-max))
           (- (point-max) (point-min))
           (if (= 1 (point-min))
               ""
             "with narrowing in effect. ")))
(voice-setup-map-face 'header-line 'voice-bolden)

(defun emacspeak--sox-multiwindow (corners)
  "Takes `window-edges' and plays a sound cue based on position of current window with respect to
the overall window layout."
  (let
      ((tr 0)
       (mr (/ (frame-height) 2))
       (br (1- (frame-height)))
       (lc 0)
       (mc (/ (frame-width) 2))
       (rc (frame-width)))
    (cond
     ((equal corners `(,lc ,tr ,mc ,br))
      (sox-multiwindow)
      'left-half)
     ((equal corners `(,mc ,tr ,rc ,br))
      (sox-multiwindow 'swap)
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
            (vc-state (when (and vc-mode (buffer-file-name)) (vc-state (buffer-file-name))))
            (frame-info (emacspeak-get-voicefied-frame-info (selected-frame)))
            (recursion-info (emacspeak-get-voicefied-recursion-info (recursion-depth)))
            (dir-info
             (when (or (eq major-mode 'shell-mode)
                       (eq major-mode 'comint-mode))
               (abbreviate-file-name default-directory))))
        (when (> window-count 1) (emacspeak--sox-multiwindow (window-edges)))
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
            (when buffer-read-only (emacspeak-auditory-icon 'unmodified-object)))
          (tts-with-punctuations 'all
            (dtk-speak
             (concat
              dir-info
              (propertize (buffer-name) 'personality voice-lighten-medium)
              (when window-count (propertize window-count 'personality voice-smoothen))
              (when vc-mode (propertize (downcase vc-mode) 'personality voice-smoothen))
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

;;}}}

;;;###autoload
(defun emacspeak-speak-minor-mode-line (&optional log-msg)
  "Speak the minor mode-information.
Optional interactive prefix arg `log-msg' logs spoken info to
*Messages*."
  (interactive "P")
  (cl-declare (special minor-mode-alist))
  (let ((info  (format-mode-line minor-mode-alist)))
    (when log-msg (ems--log-message info))
    (dtk-speak  info)))

(cl--defalias 'emacspeak-speak-line-number 'what-line)

;;;###autoload
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
      (when (> window-count 1) (sox-multiwindow))
      (dtk-speak (format-mode-line header-line-format))))
   (t (dtk-speak "No header line."))))

;;;###autoload
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

;;; Functions to browse without moving:
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

;;;###autoload
(defun emacspeak-read-previous-line (&optional arg)
  "Read previous line, specified by an offset, without moving.
Default is to read the previous line. "
  (interactive "p")
  (emacspeak-read-line-internal (- (or arg 1))))

;;;###autoload
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

;;;###autoload
(defun emacspeak-read-previous-word (&optional arg)
  "Read previous word, specified as a prefix arg, without moving.
Default is to read the previous word. "
  (interactive "p")
  (emacspeak-read-word-internal (- (or arg 1))))

;;;###autoload
(defun emacspeak-read-next-word (&optional arg)
  "Read next word, specified as a numeric  arg, without moving.
Default is to read the next word. "
  (interactive "p")
  (emacspeak-read-word-internal (or arg 1)))

;;}}}
;;{{{  Speak misc information e.g. time, version, current-kill  etc

(defcustom emacspeak-speak-time-format-string
  "%H:%M   on %A, %B %_e, %Y "
  "Format string that specifies how the time should be spoken.
See the documentation for function
`format-time-string'"
  :group 'emacspeak
  :type 'string)

;;{{{ world clock

(defcustom emacspeak-speak-zoneinfo-directory
  "/usr/share/zoneinfo/"
  "Directory containing timezone data."
  :type 'directory
  :group 'emacspeak)
;;;###autoload
(defun emacspeak-speak-world-clock (zone &optional set)
  "Display current date and time  for specified zone.
Optional second arg `set' sets the TZ environment variable as well."
  (interactive
   (list
    (let ((completion-ignore-case t)
          (ido-case-fold t)
          (read-file-name-completion-ignore-case t))
      (read-file-name
       "Timezone: "
       emacspeak-speak-zoneinfo-directory))
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
                    (substring zone (length emacspeak-speak-zoneinfo-directory)))))))

;;}}}
;;;###autoload
(defun emacspeak-speak-time (&optional world)
  "Speak the time.
Spoken time  is available in the *notifications* buffer via \\[emacspeak-view-notifications].
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
      (tts-with-punctuations
          'some
        (dtk-notify-speak time-string))))))


(defun emacspeak-speak-seconds-since-epoch (seconds)
  "Speaks time value specified as seconds  since epoch, e.g. as from float-time."
  (interactive
   (list
    (read-minibuffer "Seconds: "
                     (word-at-point))))
  (cl-declare (special emacspeak-speak-time-format-string))
  (message
   (format-time-string
    emacspeak-speak-time-format-string (seconds-to-time seconds))))


(defun emacspeak-speak-microseconds-since-epoch (ms)
  "Speaks time value specified as microseconds  since epoch, e.g. as from float-time."
  (interactive
   (list
    (read-minibuffer "MicroSeconds: " (word-at-point))))
  (let ((seconds (/ ms 1000000)))
    (emacspeak-speak-seconds-since-epoch seconds)))


(defun emacspeak-speak-milliseconds-since-epoch (ms)
  "Speaks time value specified as milliseconds  since epoch, e.g. as from float-time."
  (interactive
   (list
    (read-minibuffer "MilliSeconds: " (word-at-point))))
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

(defvar emacspeak-codename
  (propertize "EfficientDog" 'face 'bold)
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
  (concat "53.0,   " emacspeak-codename)
  "Version number for Emacspeak.")

;;;###autoload
(defun emacspeak-speak-version (&optional speak-rev)
  "Announce version information for running emacspeak.
Optional interactive prefix arg `speak-rev' speaks only the Git revision number."
  (interactive "P")
  (cl-declare (special emacspeak-version emacspeak-sounds-directory
                       emacspeak-m-player-program emacspeak-use-auditory-icons))
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

;;;###autoload
(defun emacspeak-speak-current-kill (&optional count)
  "Speak the current kill entry.
This is the text that will be yanked in
by the next \\[yank]. Prefix numeric arg, COUNT, specifies that the
text that will be yanked as a result of a \\[yank] followed by count-1
\\[yank-pop] be spoken. The kill number that is spoken says what
numeric prefix arg to give to command yank."
  (interactive "p")
  (let ((context
         (format "kill %s "
                 (if current-prefix-arg (+ 1 count) 1))))
    (put-text-property 0 (length context)
                       'personality voice-annotate context)
    (dtk-speak
     (concat
      context
      (current-kill (if current-prefix-arg count 0) t)))))

;;;###autoload
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


(defun emacspeak-dial-dtk (number)
  "Prompt for and dial a phone NUMBER with the Dectalk."
  (interactive "sEnter phone number to dial:")
  (let ((dtk-stop-immediately nil))
    (dtk-dispatch (format "[:dial %s]"
                          (emacspeak-speak-string-to-phone-number number)))
    (sit-for 4)))

;;}}}
;;{{{ Ordinal Numbers:

(defun emacspeak-speak-ordinal (n)
  "Return ordinal number for n"
  (format
   (concat
    "%d"
    (if (memq n '(11 12 13)) "th"
      (let ((last-digit (% n 10)))
        (cl-case last-digit
          (1 "st")
          (2 "nd")
          (3 "rd")
          (otherwise "th")))))
   n))

;;}}}
;;{{{ speaking marks

;;; Intelligent mark feedback for emacspeak:
;;;

;;;###autoload
(defun emacspeak-speak-current-mark (count)
  "Speak the line containing the mark.
With no argument, speaks the line containing the mark--this is
where `exchange-point-and-mark' \\[exchange-point-and-mark] would
jump.  Numeric prefix arg 'COUNT' speaks line containing mark 'n'
where 'n' is one less than the number of times one has to jump
using `set-mark-command' to get to this marked position.  The
location of the mark is indicated by an aural highlight achieved
by a change in voice personality."
  (interactive "p")
  (unless (mark) (error "No marks set in this buffer"))
  (when (and current-prefix-arg (> count (length mark-ring)))
    (error "Not that many marks in this buffer"))
  (let ((line nil)
        (pos nil)
        (context
         (format "mark %s "
                 (if current-prefix-arg count 0))))
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
       (setq line (ems--this-line))))
    (dtk-speak
     (concat context line))))

;;}}}
;;{{{ speaking personality chunks


(defun emacspeak-speak-this-personality-chunk ()
  "Speak chunk of text around point that has current
personality."
  (interactive)
  (let ((start (dtk-previous-style-change (point)))
        (end (dtk-next-style-change (point))))
    (emacspeak-speak-region
     (if  start (1+ start) (point-min))
     (or  end  (point-max)))))


(defun emacspeak-speak-next-personality-chunk ()
  "Moves to the front of next chunk having current personality.
Speak that chunk after moving."
  (interactive)
  (let ((this-end (dtk-next-style-change (point) (point-max)))
        (next-start nil))
    (cond
     ((and (< this-end (point-max))
           (setq next-start
                 (dtk-next-style-change this-end (point-max))))
      (goto-char next-start)
      (forward-char 1)
      (emacspeak-speak-this-personality-chunk))
     (t (error "No more chunks with current personality.")))))

;;; this helper is here since text-property-any doesn't work
;;; backwards

(defun ems-backwards-text-property-any (max min property
                                            value)
  "Scan backwards from max till we find specified property
                                               setting.
Return buffer position or nil on failure."
  (let ((result nil)
        (start max)
        (continue t))
    (save-excursion
      (while (and continue
                  (not
                   (or (< (point) min)
                       (bobp))))
        (backward-char 1)
        (setq start (previous-single-property-change (point) property))
        (if (null start)
            (setq continue nil)
          (setq continue
                (not (eq value
                         (get-text-property start property)))))
        (or continue
            (setq result start)))
      result)))


(defun emacspeak-speak-previous-personality-chunk ()
  "Moves to the front of previous chunk having current personality.
Speak that chunk after moving."
  (interactive)
  (let ((this-start (dtk-previous-style-change (point))))
    (cond
     ((and (> this-start (point-min))
           (goto-char (dtk-previous-style-change (point)))
           (backward-char 1)
           (emacspeak-speak-this-personality-chunk)))
     (t (error "No previous  chunks with current personality.")))))

(defun emacspeak-speak-face-interval-and-move ()
  "Speaks region delimited by text in current face, and moves past the chunk."
  (interactive)
  (let ((face (get-char-property (point) 'face))
        (start (point))
        (end nil))
;;; skip over opening delimiter
    (goto-char (next-single-char-property-change start 'face))
    (when (eobp) (error "End of buffer"))
    (setq end
          (or
           (text-property-any (point) (point-max)
                              'face face)
           (point-max)))
    (dtk-speak
     (buffer-substring start end))
    (goto-char end)
    (emacspeak-auditory-icon 'large-movement)))

;;}}}
;;{{{ speaking face   chunks


(defun emacspeak-speak-this-face-chunk ()
  "Speak chunk of text around point that has current face."
  (interactive)
  (let ((start (previous-single-property-change (point) 'face))
        (end (next-single-property-change (point) 'face )))
    (emacspeak-speak-region
     (if  start (1+ start) (point-min))
     (or end (point-max)))))


(defun emacspeak-speak-next-face-chunk ()
  "Moves to the front of next chunk having current style.
Speak that chunk after moving."
  (interactive)
  (let ((face (get-text-property (point) 'face))
        (this-end (next-single-property-change (point) 'face))
        (next-start nil))
    (cond
     ((and (< this-end (point-max))
           (setq next-start (next-single-property-change this-end 'face)))
      (goto-char next-start)
      (when (eq face  (get-text-property (point) 'face))
        (emacspeak-speak-this-face-chunk)))
     (t (message "No more chunks with current face.")))))


(defun emacspeak-speak-previous-face-chunk ()
  "Moves to the front of previous chunk having current face.
Speak that chunk after moving."
  (interactive)
  (let ((face (get-text-property (point) 'face))
        (this-start (previous-single-property-change (point)  'face))
        (prev-end nil))
    (cond
     ((and (> this-start (point-min))
           (setq prev-end
                 (previous-single-property-change (1- this-start) 'face)))
      (goto-char prev-end)
      (when (eq face (get-text-property (point) 'face))
        (emacspeak-speak-this-face-chunk)))
     (t (error "No previous  chunks with current face.")))))

;;}}}
;;{{{  Execute command repeatedly,

;;;###autoload
(defun emacspeak-execute-repeatedly (command)
  "Execute COMMAND repeatedly."
  (interactive
   (list (read-command "Command to execute repeatedly:")))
  (let ((key "")
        (pos (point))
        (continue t)
        (message (format "Press space to execute %s again" command)))
    (while continue
      (call-interactively command)
      (cond
       ((= (point) pos) (setq continue nil))
       (t (setq pos (point))
          (setq key
                (let ((dtk-stop-immediately nil))
                  (read-key-sequence message)))
          (when (and (stringp key)
                     (not (= 32 (string-to-char key))))
            (dtk-stop)
            (setq continue nil)))))
    (dtk-speak "Exited continuous mode ")))

;;;###autoload
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

;;;###autoload
(defun      emacspeak-speak-browse-buffer-by-style (&optional browse)
  "Browse current buffer by style.
Default is to speak chunk having current style.
Interactive prefix arg `browse'  repeatedly browses  through
  chunks having same style as the current text chunk."
  (interactive "P")
  (cond
   (browse
    (emacspeak-execute-repeatedly
     'emacspeak-speak-next-personality-chunk))
   (t (emacspeak-speak-this-personality-chunk))))

;;}}}
;;{{{  skimming

;;;###autoload
(defun emacspeak-speak-skim-buffer ()
  "Skim the current buffer  a paragraph at a time."
  (interactive)
  (emacspeak-execute-repeatedly 'forward-paragraph))

;;}}}
;;{{{   quieten messages

(ems-generate-switcher 'emacspeak-toggle-speak-messages
                       'emacspeak-speak-messages
                       "Toggle the state of whether emacspeak echoes messages.")

;;}}}
;;{{{  Moving across fields:

;;; Fields are defined by property 'field

;;; helper function: speak a field
(defun emacspeak-speak-field (start end)
  "Speaks field delimited by arguments START and END."
  (cl-declare (special voice-annotate))
  (let ((header (or (get-text-property start 'field-name) "")))
    (dtk-speak
     (concat
      (progn (put-text-property 0 (length header)
                                'personality voice-annotate
                                header)
             header)
      " "
      (buffer-substring start end)))))

(defun emacspeak-speak-current-field ()
  "Speak current field."
  (interactive)
  (emacspeak-speak-region (field-beginning)
                          (field-end)))

(defun emacspeak-speak-next-field ()
  "Move to and speak next field."
  (interactive)
  (cl-declare (special inhibit-field-text-motion))
  (let ((inhibit-field-text-motion t)
        (start nil))
    (skip-syntax-forward "^ ")
    (skip-syntax-forward " ")
    (setq start (point))
    (save-excursion
      (skip-syntax-forward "^ ")
      (emacspeak-speak-field start (point)))))

;;;###autoload
(defun emacspeak-speak-previous-field ()
  "Move to previous field and speak it."
  (interactive)
  (cl-declare (special inhibit-field-text-motion))
  (let ((inhibit-field-text-motion t)
        (start nil))
    (skip-syntax-backward " ")
    (setq start (point))
    (skip-syntax-backward "^ ")
    (emacspeak-speak-field (point) start)))

(defun emacspeak-speak-current-column ()
  "Speak the current column."
  (interactive)
  (message "Point at column %d" (current-column)))

(defun emacspeak-speak-current-percentage ()
  "Announce the percentage into the current buffer."
  (interactive)
  (message "Point is  %d%% into  the current buffer"
           (emacspeak-get-current-percentage-into-buffer)))

(defvar ems--message-filter-pattern nil
  "Internal variable that holds pattern used to filter spoken
  messages.")

;;}}}
;;{{{  Speak the last message again:

(defvar emacspeak-speak-message-again-copy-as-kill t
  "If set, asking for last message will copy it to the kill ring.")

;;;###autoload
(defun emacspeak-speak-message-again (&optional from-message-cache)
  "Speak the last message from Emacs once again.
The message is also placed in the kill ring for convenient yanking
if `emacspeak-speak-message-again-copy-as-kill' is set."
  (interactive "P")
  (cl-declare (special emacspeak-last-message
                       emacspeak-speak-message-again-copy-as-kill))
  (cond
   (from-message-cache
    (dtk-speak emacspeak-last-message)
    (when (and (called-interactively-p 'interactive)
               emacspeak-speak-message-again-copy-as-kill)
      (kill-new emacspeak-last-message)))
   (t
    (save-current-buffer
      (set-buffer "*Messages*")
      (goto-char (point-max))
      (skip-syntax-backward " >")
      (emacspeak-speak-line)
      (when (and (called-interactively-p 'interactive)
                 emacspeak-speak-message-again-copy-as-kill)
        (kill-new (ems--this-line)))))))

(defun emacspeak-announce (announcement)
  "Speak the ANNOUNCEMENT, if possible.
Otherwise just display a message."
  (message announcement))

;;}}}
;;{{{  Using emacs's windows usefully:

;;Return current window contents
(defun emacspeak-get-window-contents ()
  "Return window contents."
  (let ((start nil))
    (save-excursion
      (move-to-window-line 0)
      (setq start (point))
      (move-to-window-line -1)
      (end-of-line)
      (buffer-substring start (point)))))

;;;###autoload
(defun emacspeak-speak-window-information ()
  "Speaks information about current window."
  (interactive)
  (message "Current window has %s lines and %s columns with
top left %s %s "
           (window-height)
           (window-width)
           (cl-first (window-edges))
           (cl-second (window-edges))))

;;;###autoload
(defun emacspeak-speak-current-window ()
  "Speak contents of current window.
Speaks entire window irrespective of point."
  (interactive)
  (emacspeak-speak-region (window-start) (window-end)))

;;;###autoload
(defun emacspeak-speak-other-window (&optional arg)
  "Speak contents of `other' window.
Speaks entire window irrespective of point.
Semantics  of `other' is the same as for the builtin Emacs command
`other-window'.
Optional argument ARG  specifies `other' window to speak."
  (interactive "nSpeak window")
  (save-excursion
    (save-window-excursion
      (other-window arg)
      (save-current-buffer
        (set-buffer (window-buffer))
        (emacspeak-speak-region
         (max (point-min) (window-start))
         (min (point-max) (window-end)))))))

;;;###autoload
(defun emacspeak-speak-next-window ()
  "Speak the next window."
  (interactive)
  (emacspeak-speak-other-window 1))

;;;###autoload
(defun emacspeak-speak-previous-window ()
  "Speak the previous window."
  (interactive)
  (emacspeak-speak-other-window -1))


(defun emacspeak-owindow-scroll-up ()
  "Scroll up the window that command `other-window' would move to.
Speak the window contents after scrolling."
  (interactive)
  (let ((window (selected-window)))
    (other-window 1)
    (call-interactively 'scroll-up)
    (select-window window)))


(defun emacspeak-owindow-scroll-down ()
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
  "Speak one of the first 10 windows on the screen.
Speaks entire window irrespective of point.
In general, you'll never have Emacs split the screen into more than
two or three.
Argument ARG determines the 'other' window to speak.
Semantics  of `other' is the same as for the builtin Emacs command
`other-window'."
  (interactive "P")
  (cl-declare (special last-input-event))
  (let* ((window-size-change-functions nil)
         (window
          (cond
           ((not (called-interactively-p 'interactive)) arg)
           (t
            (condition-case nil
                (read (format "%c" last-input-event))
              (error nil))))))
    (or (numberp window)
        (setq window
              (read-number "Window   between 1 and 9 to speak" 1)))
    (setq window (1- window))
    (save-excursion
      (save-window-excursion
        (other-window window)
        (emacspeak-speak-region (window-start) (window-end))))))

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
               unit-name unit-name unit-name)))
  (let ((char (read-char)))
    (cond
     ((= char ?s) -1)
     ((= char ?r) 1)
     (t nil))))

;;;###autoload
(defun emacspeak-speak-buffer-interactively ()
  "Speak the start of, rest of, or the entire buffer.
's' to speak the start.
'r' to speak the rest.
any other key to speak entire buffer."
  (interactive)
  (emacspeak-speak-buffer
   (emacspeak-ask-how-to-speak "buffer" (sit-for 1))))

;;;###autoload
(defun emacspeak-speak-help-interactively ()
  "Speak the start of, rest of, or the entire help.
's' to speak the start.
'r' to speak the rest.
any other key to speak entire help."
  (interactive)
  (emacspeak-speak-help
   (emacspeak-ask-how-to-speak "help" (sit-for 1))))

;;;###autoload
(defun emacspeak-speak-line-interactively ()
  "Speak the start of, rest of, or the entire line.
's' to speak the start.
'r' to speak the rest.
any other key to speak entire line."
  (interactive)
  (emacspeak-speak-line
   (emacspeak-ask-how-to-speak "line" (sit-for 1))))

;;;###autoload
(defun emacspeak-speak-paragraph-interactively ()
  "Speak the start of, rest of, or the entire paragraph.
's' to speak the start.
'r' to speak the rest.
any other key to speak entire paragraph."
  (interactive)
  (emacspeak-speak-paragraph
   (emacspeak-ask-how-to-speak "paragraph" (sit-for 1))))

;;;###autoload
(defun emacspeak-speak-page-interactively ()
  "Speak the start of, rest of, or the entire page.
's' to speak the start.
'r' to speak the rest.
any other key to speak entire page."
  (interactive)
  (emacspeak-speak-page
   (emacspeak-ask-how-to-speak "page" (sit-for 1))))

;;;###autoload
(defun emacspeak-speak-word-interactively ()
  "Speak the start of, rest of, or the entire word.
's' to speak the start.
'r' to speak the rest.
any other key to speak entire word."
  (interactive)
  (emacspeak-speak-word
   (emacspeak-ask-how-to-speak "word" (sit-for 1))))

;;;###autoload
(defun emacspeak-speak-sexp-interactively ()
  "Speak the start of, rest of, or the entire sexp.
's' to speak the start.
'r' to speak the rest.
any other key to speak entire sexp."
  (interactive)
  (emacspeak-speak-sexp
   (emacspeak-ask-how-to-speak "sexp" (sit-for 1))))

;;}}}
;;{{{  emacs rectangles and regions:


;;; These help you listen to columns of text. Useful for tabulated data
;;;###autoload
(defun emacspeak-speak-rectangle (start end)
  "Speak a rectangle of text.
Rectangle is delimited by point and mark.  When call from a
program, arguments specify the START and END of the rectangle."
  (interactive "r")
  (require 'rect)
  (dtk-speak-list (extract-rectangle start end)))

;;; helper function: emacspeak-put-personality
;;; sets property 'personality to personality
(defun emacspeak-put-personality (start end personality)
  "Apply specified personality to region delimited by START and END.
Argument PERSONALITY gives the value for property personality."
  (put-text-property start end 'personality personality))

;;; Compute table of possible voices to use in completing-read
;;; We rely on dectalk-voice-table as our default voice table.
;;; Names defined in this --- and other voice tables --- are
;;; generic --and  not device specific.
;;;

(defun emacspeak-possible-voices ()
  "Return possible voices."
  (cl-declare (special dectalk-voice-table))
  (cl-loop for key being the hash-keys of dectalk-voice-table
           collect (cons
                    (symbol-name key)
                    (symbol-name key))))


(defun emacspeak-voiceify-rectangle (start end &optional personality)
  "Voicify the current rectangle.
When calling from a program,arguments are
START END personality
Prompts for PERSONALITY  with completion when called interactively."
  (interactive "r")
  (require 'rect)
  (let ((personality-table (emacspeak-possible-voices)))
    (when (called-interactively-p 'interactive)
      (setq personality
            (read
             (completing-read "Use personality: "
                              personality-table nil t))))
    (with-silent-modifications
      (operate-on-rectangle
       #'(lambda (start-seg _begextra _endextra)
           (emacspeak-put-personality start-seg (point) personality))
       start end nil))))


(defun emacspeak-voiceify-region (start end &optional personality)
  "Voicify the current region.
When calling from a program,arguments are
START END personality.
Prompts for PERSONALITY  with completion when called interactively."
  (interactive "r")
  (let ((personality-table (emacspeak-possible-voices)))
    (when (called-interactively-p 'interactive)
      (setq personality
            (read
             (completing-read "Use personality: "
                              personality-table nil t))))
    (put-text-property start end 'personality personality)))

(defun emacspeak-put-text-property-on-rectangle (start end prop value)
  "Set property to specified value for each line in the rectangle.
Argument START and END specify the rectangle.
Argument PROP specifies the property and VALUE gives the
value to apply."
  (require 'rect)
  (operate-on-rectangle
   #'(lambda (start-seg _begextra _endextra)
       (put-text-property start-seg (point) prop value))
   start end nil))

;;}}}
;;{{{  Matching delimiters:

;;; A modified blink-matching-open that always displays the matching line
;;; in the minibuffer so emacspeak can speak it.
;;;Helper: emacspeak-speak-blinkpos-message

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
;;; Show what precedes the open in its line, if anything.
        ((save-excursion
           (skip-chars-backward " \t")
           (not (bolp)))
         (buffer-substring (line-beginning-position) (1+ blinkpos)))
;;; Show what follows the open in its line, if anything.
        ((save-excursion
           (forward-char 1)
           (skip-chars-forward " \t")
           (not (eolp)))
         (buffer-substring blinkpos (line-end-position)))
;;; Otherwise show the previous nonblank line.
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
;;; Replace the newline and other whitespace with `...'.
          "..."
          (buffer-substring blinkpos (1+ blinkpos)))))))))))

;;; The only change to emacs' default blink-matching-paren is the
;;; addition of the call to helper emacspeak-speak-blinkpos-message
;;; This matcher if from emacs 19 from memory.

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
                           ;; Replace the newline and other whitespace with `...'.
                           "..."
                           (buffer-substring blinkpos (1+ blinkpos)))
                        ;; There is nothing to show except the char itself.
                        (buffer-substring blinkpos (1+ blinkpos)))))))
          (message "Matches %s"
                   (substring-no-properties
                    open-paren-line-string))
          (sit-for blink-matching-delay)))))))

(defun emacspeak-use-customized-blink-paren ()
  "A customized blink-paren to speak  matching opening paren.
We need to call this in case Emacs is anal and loads its own
builtin blink-paren function which does not talk."
  (fset 'blink-matching-open (symbol-function 'emacspeak-blink-matching-open))
  (and (called-interactively-p 'interactive)
       (message "Using customized blink-paren function provided by Emacspeak.")))

;;}}}
;;{{{  Auxillary functions:

(defun emacspeak-kill-buffer-carefully (buffer)
  "Kill BUFFER BUF if it exists."
  (and buffer
       (get-buffer buffer)
       (buffer-name (get-buffer buffer))
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

;;{{{ switching to completions window from minibuffer:

(defun emacspeak-get-minibuffer-contents ()
  "Return contents of the minibuffer."
  (save-current-buffer
    (set-buffer (window-buffer (minibuffer-window)))
    (minibuffer-contents-no-properties)))

;;; Make all occurrences of string inaudible
(defun emacspeak-make-string-inaudible (string)
  (unless (string-match "^ *$" string)
    (with-silent-modifications
      (save-excursion
        (goto-char (point-min))
        (save-match-data
          (with-silent-modifications
            (while (search-forward string nil t)
              (put-text-property (match-beginning 0)
                                 (match-end 0)
                                 'personality 'inaudible))))))))


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

;;;###autoload
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
(define-key completion-list-mode-map "\C-o" 'emacspeak-switch-to-reference-buffer)
(define-key completion-list-mode-map " " 'next-completion)
(define-key completion-list-mode-map "\C-m" 'choose-completion)
(define-key completion-list-mode-map "\M-\C-m" 'emacspeak-completion-pick-completion)
(let ((chars
       "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
  (cl-loop for char across chars
           do
           (define-key completion-list-mode-map
             (format "%c" char)
             'emacspeak-completions-move-to-completion-group)))

;;}}}

;;}}}
;;{{{ mark convenience commands

(defun emacspeak-mark-speak-mark-line ()
  (cl-declare (special voice-animate))
  (emacspeak-auditory-icon 'mark-object)
  (ems-set-personality-temporarily (point) (1+ (point))
                                   voice-animate
                                   (emacspeak-speak-line)))

;;;###autoload
(defun emacspeak-mark-backward-mark ()
  "Cycle backward through the mark ring."
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
;;{{{ speaking an extent of text delimited by specified char


(defun emacspeak-speak-and-skip-extent-upto-char (char)
  "Search forward from point until we hit char.
Speak text between point and the char we hit."
  (interactive (list (read-char "Char: ")))
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
  (cl-declare (special last-input-event))
  (emacspeak-speak-and-skip-extent-upto-char last-input-event))

;;}}}
;;{{{  speak message at time
;;;###autoload
(defun emacspeak-speak-message-at-time (time message)
  "Set up ring-at-time to speak message at specified time.
Provides simple stop watch functionality in addition to other things.
See documentation for command run-at-time for details on time-spec."
  (interactive
   (list
    (read-from-minibuffer "Time specification:  ")
    (read-from-minibuffer "Message: ")))
  (run-at-time
   time nil
   #'(lambda (m)
       (message m)
       (dtk-notify-speak m)
       (when emacspeak-use-auditory-icons (emacspeak-play-auditory-icon 'alarm))
       (sox-tones))
   message)
  (message "Set alarm for %s" time)
  (emacspeak-auditory-icon 'button))

;;}}}
;;{{{ Directory specific settings

(defvar emacspeak-speak-directory-settings
  ".espeak.el"
  "Name of file that holds directory specific settings.")

;;;###autoload
(defun emacspeak-speak-load-directory-settings (&optional dir)
  "Load a directory specific Emacspeak settings file.
This is typically used to load up settings that are specific to
an electronic book consisting of many files in the same
directory."
  (cl-declare (special emacspeak-speak-directory-settings default-directory))
  (unless dir (setq dir default-directory))
  (let ((res (locate-dominating-file dir emacspeak-speak-directory-settings)))
    (when
        (and res
             (file-exists-p (expand-file-name emacspeak-speak-directory-settings res)))
      (ems--fastload (expand-file-name emacspeak-speak-directory-settings res))
      (emacspeak-auditory-icon 'task-done))))

;;}}}
;;{{{ silence:

(defcustom emacspeak-silence-hook nil
  "Functions run after emacspeak-silence is called."
  :type '(repeat function)
  :group 'emacspeak)

;;;###autoload
(defun emacspeak-silence ()
  "Silence is golden. Stop speech, and pause/resume any media
streams. Runs `emacspeak-silence-hook' which can be used to
configure which media players get silenced or paused/resumed."
  (interactive)
  (cl-declare (special emacspeak-silence-hook))
  (dtk-stop)
  (run-hooks 'emacspeak-silence-hook))

;;}}}
;;{{{ Search

;;}}}
;;{{{ Smart date prompers:

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

;;; Hacked out of choose-completion
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

(define-key minibuffer-local-completion-map "\C-n" 'emacspeak-minibuffer-next-completion)
(define-key minibuffer-local-completion-map "\C-p" 'emacspeak-minibuffer-previous-completion)
(define-key minibuffer-local-completion-map (ems-kbd "C-@") 'emacspeak-minibuffer-choose-completion)
(define-key minibuffer-local-completion-map
  (ems-kbd "C-SPC") 'emacspeak-minibuffer-choose-completion)

;;}}}
;;{{{ Open Emacspeak Info Pages:

(defun emacspeak-open-info ()
  "Open Emacspeak Info Manual."
  (interactive)
  (cl-declare (special emacspeak-info-directory))
  (funcall-interactively #'info (expand-file-name "emacspeak.info" emacspeak-info-directory) "*Emacspeak Info*"))

;;}}}
;;{{{ Describe help map:
;;;###autoload
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
(provide 'emacspeak-speak)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
