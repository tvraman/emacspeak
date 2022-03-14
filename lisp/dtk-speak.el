;;; dtk-speak.el --- Interface to speech server -*- lexical-binding: t; -*-
;;;$Id$
;;; $Author: tv.raman.tv $
;;; Description:  Emacs interface to TTS
;;; Keywords: TTS  Emacs Elisp
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2008-07-06 10:18:30 -0700 (Sun, 06 Jul 2008) $ |
;;;  $Revision: 4670 $ |
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ introduction:

;;; Commentary:
;;;Defines the TTS interface.
;;; Here, prefix dtk is synonymous with tts.
;;; Code:
;;

;;}}}
;;{{{ required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(eval-when-compile (require 'subr-x))
(declare-function ems--fastload "emacspeak-preamble" (file))

;;}}}
;;{{{ Forward Declarations:

(declare-function voice-setup-get-voice-for-face "voice-setup" (face))
(declare-function emacspeak-auditory-icon "emacspeak-sounds.el" (icon))
(declare-function emacspeak-queue-auditory-icon "emacspeak-sounds.el"
                  (icon))
;;;###autoload
(defvar dtk-program
  (or
   (getenv "DTK_PROGRAM")
   (cond
    ((eq system-type 'darwin) "mac")
    (t "espeak")))
  "Speech-server.
Choices:
dtk-exp     For the Dectalk Express.
outloud     For IBM ViaVoice Outloud
espeak      For eSpeak (default on Linux)
mac for MAC TTS (default on Mac)")
;;}}}
;;{{{  user customizations:

(defgroup tts nil
  "TTS."
  :group 'emacspeak
  :prefix "dtk-")

(defvar-local tts-strip-octals nil
  "Strip all octal chars. ")

(defcustom dtk-speech-rate-base
  (if (string-match "dtk" dtk-program) 180 50)
  "Value of lowest speech rate."
  :type 'integer
  :group 'tts)

(defcustom dtk-speech-rate-step
  (if (string-match "dtk" dtk-program) 50 8)
  "Speech rate step used by `dtk-set-predefined-speech-rate'."
  :type 'integer
  :group 'tts)

(defvar-local dtk-quiet nil
  "Silence speech ")

(defvar-local  dtk-split-caps t
  "Flag indicating whether to use split caps.
 Use   `dtk-toggle-split-caps'bound to \\[dtk-toggle-split-caps].")

(defcustom dtk-cleanup-repeats
  (list
   ". " "." "_" "-" "=" "/" "+" "*" ":" ";" "%"
   "\\/" "/\\" "{" "}" "~" "$" ")" "#" "<>" "^" "<" ">")
  "List of repeating patterns to clean up.
Use `dtk-add-cleanup-pattern'
 bound to \\[dtk-add-cleanup-pattern]  to add  patterns.

More than 3 consecutive occurrences
of a  pattern   is
replaced with a repeat count. "
  :type '(repeat (string :tag "pattern"))
  :group 'tts)

;;}}}
;;{{{  internal variables

(defvar dtk-character-scale 1.1
  "Factor used to  scale speech rate  when speaking letters.
  Use `dtk-set-character-scale' bound to
\\[dtk-set-character-scale].")

(defvar-local dtk-caps nil
  "Non-nil means  indicate  capitalization.
Capitalized words are preceded by `cap', and upper-case words are
  preceded by `ac' spoken in a lower voice.
Use dtk-toggle-caps
bound to \\[dtk-toggle-caps].")

(defconst dtk-punctuation-mode-alist
  '("some" "all" "none")
  "List of  punctuation modes.")

(defvar-local dtk-speech-rate
  (if (string-match "dtk" dtk-program)
      225 100)
  "Speech rate.
 Use `dtk-set-rate'
 bound to \\[dtk-set-rate].")

;;}}}
;;{{{Style Helper:

;;; helper: Identify (a . b).
(defsubst dtk-plain-cons-p (value)
  (and (consp value) (not (proper-list-p value))))

;;; Helper: Get face->voice mapping

(defun dtk-get-voice-for-face  (value)
    "Face->voice map"
    (when value
      (cond
       ((symbolp value) (voice-setup-get-voice-for-face value))
       ((dtk-plain-cons-p value)) ;;pass on plain cons
       ((listp value)
        (delq nil (mapcar   #'voice-setup-get-voice-for-face value))))))

(defsubst dtk-get-style (&optional pos)
  " Return  style based on personality or face at `POS'.   "
  (or pos (setq pos (point)))
  (or
   (get-text-property pos 'personality)
   (dtk-get-voice-for-face (get-text-property pos 'face))))

;;}}}
;;{{{ Tone Helpers:

;;; Deletion Tone.
(defsubst dtk-tone-deletion ()
  (dtk-tone 500 75 'force))
;;; Upcase Tone.
(defsubst dtk-tone-upcase ()
  (dtk-tone 800 100 'force))
;;;Downcase tone.
(defsubst dtk-tone-downcase ()
  (dtk-tone 600 100 'force))

;;}}}
;;{{{ helper: apply pronunciations

;;; moved here from the emacspeak-pronounce module for efficient
;;compilation

;;; Helper: like replace-match but preserves existing face or apply
;;; 'match for pronunciation

(defsubst tts-replace-match (replace)
  (cl-declare (special emacspeak-pronounce-personality))
  (let* ((start (match-beginning 0))
         (face
          (or (get-text-property start 'face) emacspeak-pronounce-personality)))
    (replace-match replace t t)
    (when face (put-text-property start (point) 'face face))))

(defun tts-apply-pronunciations (pronunciation-table)
  "Applies pronunciations per pronunciation table to current buffer. "
  (cl-loop
   for w being the hash-keys of pronunciation-table do
   (let ((pronunciation (gethash w pronunciation-table)))
     (goto-char (point-min))
     (cond
      ((stringp pronunciation)
       (while (search-forward w nil t)
         (tts-replace-match pronunciation)))
      ((consp pronunciation)
       (let ((matcher (car pronunciation))
             (pronouncer (cdr pronunciation))
             (pronunciation ""))
         (while (funcall matcher w nil t)
           (setq
            pronunciation
            (save-match-data
              (funcall pronouncer
                       (buffer-substring (match-beginning 0) (match-end 0)))))
           (tts-replace-match pronunciation))))))))

;;}}}
;;{{{  Helpers to handle invisible text:

(defun dtk--skip-invisible-forward ()
  "Move across invisible text."
  (while (and (not (eobp))
              (invisible-p (point)))
    (goto-char
     (next-single-property-change (point) 'invisible
                                  (current-buffer) (point-max)))))

(defun dtk--skip-invisible-backward ()
  "Move backwards over invisible text."
  (while (and (not (bobp))
              (invisible-p (point)))
    (goto-char
     (previous-single-property-change (point) 'invisible
                                      (current-buffer) (point-min)))))

(defun dtk--delete-invisible-text ()
  "Delete invisible text."
  (goto-char (point-min))
  (let ((start (point)))
    (while (not (eobp))
      (cond
       ((invisible-p (point))
        (dtk--skip-invisible-forward)
        (delete-region start (point))
        (setq start (point)))
       (t (goto-char
           (or (next-single-property-change (point) 'invisible)
               (point-max)))
          (setq start (point)))))))

;;}}}
;;{{{  Tones, Language, formatting speech etc.

(defun dtk-silence (duration &optional force)
  "Produce `duration' ms of silence. "
  (cl-declare (special dtk-quiet dtk-speaker-process
                       dtk-speak-server-initialized))
  (unless dtk-quiet
    (when dtk-speak-server-initialized
      (dtk-interp-silence duration
                          (if force "\nd" "")))))

(defcustom dtk-use-tones t
  "Toggle tones. "
  :type 'boolean
  :group 'tts)

(defun dtk-tone (pitch duration &optional force)
  "Produce a tone.
 Pitch   is  in hertz.
 Duration  is  in milliseconds.
Uses a 5ms fade-in and fade-out. "
  (cl-declare (special dtk-quiet dtk-speaker-process
                       dtk-use-tones dtk-speak-server-initialized))
  (unless
      (or dtk-quiet
          (not dtk-use-tones)
          (not dtk-speak-server-initialized))
    (dtk-interp-tone pitch duration force)))

(defun dtk-set-language (lang)
  "Set language."
  (interactive "sEnter language: \n")
  (cl-declare (special dtk-speak-server-initialized))
  (when dtk-speak-server-initialized
    (dtk-interp-language lang (called-interactively-p 'interactive))))

(defun dtk-set-next-language ()
  "Switch to  next  language"
  (interactive)
  (cl-declare (special dtk-speak-server-initialized))
  (when dtk-speak-server-initialized
    (dtk-interp-next-language (called-interactively-p 'interactive))))

(defun dtk-set-previous-language ()
  "Switch to  previous  language"
  (interactive)
  (cl-declare (special dtk-speak-server-initialized))
  (when dtk-speak-server-initialized
    (dtk-interp-previous-language (called-interactively-p 'interactive))))

(defun dtk-set-preferred-language (alias lang)
  "Set language by alias."
  (interactive "s")
  (cl-declare (special dtk-speak-server-initialized))
  (when dtk-speak-server-initialized
    (dtk-interp-preferred-language alias lang)))

(defun dtk-list-languages ()
  "List  available languages."
  (interactive)
  (cl-declare (special dtk-speak-server-initialized))
  (when dtk-speak-server-initialized
    (dtk-interp-list-language)))

;;; helper function:
;;; Quote the string in current buffer so tcl does not barf.
;;; Fix brackets by changing to text.
;;; This is necessary because
;;;  [] marks dtk commands; {} is special to tcl

(defconst dtk-bracket-regexp
  "[][{}<>\\|`#\n]"
  "Brackets and other chars  that are special to dtk and tcl.
Newlines  become spaces so each server request is a single line.
{} is special to tcl.
[] is special to both dtk and tcl.
<> and | are fixed to improve pronunciation.
\\ is fixed because it tends to be a metacharacter")

(defun dtk-strip-octals ()
  "Remove all octal chars."
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (while (re-search-forward "[\177-\377]+" nil t)
      (replace-match " "))))

(defun dtk-fix-brackets (mode)
  "Quote  delimiters that need special treatment. Argument MODE
specifies the current pronunciation mode --- See
\\[dtk-bracket-regexp]"
  (cl-declare (special dtk-bracket-regexp))
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (cond
     ((eq 'all mode)
      (let ((start nil)
            (personality nil))
        (while (re-search-forward dtk-bracket-regexp nil t)
          (setq start (match-beginning 0))
          (setq personality (dtk-get-style (match-beginning 0)))
          (cond
           ((= 10 (char-after (match-beginning 0))) ; newline
            (replace-match " "))
           ((= ?| (char-after (match-beginning 0)))
            (replace-match " pipe " nil t))
           ((= ?< (char-after (match-beginning 0)))
            (replace-match " less than " nil t))
           ((= ?> (char-after (match-beginning 0)))
            (replace-match " greater than " nil t))
           ((= ?{ (char-after (match-beginning 0)))
            (replace-match " left brace " nil t))
           ((= ?} (char-after (match-beginning 0)))
            (replace-match " right brace " nil t))
           ((= ?\] (char-after (match-beginning 0)))
            (replace-match " right bracket " nil t))
           ((= ?\[ (char-after (match-beginning 0)))
            (replace-match " left bracket " nil t))
           ((= ?\\ (char-after (match-beginning 0)))
            (replace-match " backslash " nil t))
           ((= ?# (char-after (match-beginning 0)))
            (replace-match " pound " nil t))
           ((= ?` (char-after (match-beginning 0)))
            (replace-match " backquote " nil t)))
          (when personality
            (put-text-property start (point) 'personality personality)))))
     (t
      (while (re-search-forward dtk-bracket-regexp nil t)
        (replace-match " " nil t))))))

(defvar-local dtk-speak-nonprinting-chars nil
  "Speak non-printing chars.")

(defvar dtk-octal-chars
  "[\000-\010\013\014\016-\037\177-\377]"
  "Regular expression matching control chars. ")

(defun dtk-fix-control-chars ()
  "Handle control characters in speech stream."
  (cl-declare (special dtk-character-to-speech-table
                       dtk-octal-chars
                       dtk-speak-nonprinting-chars))
  (let ((char nil))
    (goto-char (point-min))
    (cond
     (tts-strip-octals ;;;Strip octals if asked to
      (dtk-strip-octals))
     (dtk-speak-nonprinting-chars
      (while (re-search-forward dtk-octal-chars nil t)
        (setq char (char-after (match-beginning 0)))
        (replace-match
         (format " %s " (aref dtk-character-to-speech-table char))
         nil t))))))
(defconst dtk-caps-regexp
  (concat
   "\\(\\b[A-Z][A-Z0-9_-]+\\b\\)"
   "\\|"
   "\\(\\b[A-Z]\\)")
  "Match capitalized or upper-case words.")

(defcustom dtk-caps-prefix
  
  (propertize  "cap" 'personality 'acss-s4-r6)
  "Prefix used to indicate capitalization":type 'string
  :group 'tts
  :set #'(lambda (sym val)
           (set-default sym
                        (propertize  val 'personality 'acss-p3-s1-r3))))

(defcustom dtk-allcaps-prefix
  
  (propertize  " acc " 'personality 'acss-s4-r6)
  "Prefix used to indicate AllCaps"
  :type 'string
  :group 'tts
  :set #'(lambda (sym val)
           (set-default sym
                        (propertize  val 'personality 'acss-p3-s1-r3))))

(defun dtk-handle-caps ()
  "Handle capitalization"
  (cl-declare (special dtk-caps dtk-caps-regexp
                       dtk-caps-prefix dtk-allcaps-prefix))
  (when dtk-caps
    (let ((inhibit-read-only t)
          (case-fold-search nil))
      (goto-char (point-min))
      (while
          (re-search-forward dtk-caps-regexp nil t)
        (save-excursion
          (goto-char (match-beginning 0))
          (cond
           ((= 1  (- (match-end 0) (match-beginning 0)))
            (insert dtk-caps-prefix))
           (t (insert dtk-allcaps-prefix))))))))

;;; Takes a string, and replaces occurrences  of this pattern
;;; that are longer than 3 by a string of the form \"count
;;; string\". Second argument, mode, is the pronunciation
;;; mode being used to speak.  Removing repeated chars, and
;;; replacing them by a count:

(defun dtk-replace-duplicates (string mode)
  "Replace repeating patterns.
 `STRING' is  the repeating string to replace.
` MODE' is  the current pronunciation mode."
  (let* ((inhibit-read-only t)
         (len (length string))
         (pattern (regexp-quote string))
         (reg (concat
               pattern pattern
               "\\(" pattern "\\)+"))
         (start nil)
         (personality nil)
         (replacement nil))
    (while (re-search-forward reg nil t)
      (setq personality (dtk-get-style (match-beginning 0)))
      (setq replacement
            (if (eq 'all mode)
                (format
                 " aw %s %s"
                 (/ (- (match-end 0) (match-beginning 0)) len)
                 (if (string-equal " " string) " space " string))
              ""))
      (replace-match replacement nil t)
      (setq start (- (point) (length replacement)))
      (when personality
        (put-text-property start (point) 'personality personality)))
    (goto-char (point-min))))

(defun dtk-handle-repeating-patterns (mode)
  "Handle repeating patterns by replacing them with 'aw <length> char-names'"
  (cl-declare (special dtk-cleanup-repeats))
  (goto-char (point-min))
  (mapc
   #'(lambda (str)
       (dtk-replace-duplicates str mode))
   dtk-cleanup-repeats))

(defun dtk-quote (mode)
  "Clean-up text."
  (let ((inhibit-read-only t))
;;; dtk will think it's processing a command otherwise:
    (dtk-fix-brackets mode)
    (dtk-handle-caps)
;;; fix control chars
    (dtk-fix-control-chars)))

(defun dtk-fix-backslash ()
  "Quote backslash characters."
  (goto-char (point-min))
  (while (search-forward "\\" nil t)
    (replace-match " backslash " nil t)))

;;; Moving  across a chunk of text.
;;; A chunk  is specified by a punctuation (todo? followed by whitespace)
;;; or  multiple blank lines
;;; or a comment start or end
;;; or a parenthesis grouping start or end
;;; leaves point at the end of the chunk.
;;; returns  distance moved; nil if stationery
(defvar-local dtk-chunk-separator-syntax ".>)$\""
  "Syntax classes  used when   splitting text.")

(defsubst dtk-complement-chunk-separator-syntax ()
  "Return complement of syntactic class that splits clauses."
  (cl-declare (special dtk-chunk-separator-syntax))
  (concat "^" dtk-chunk-separator-syntax))

;;; set chunk separator to match both whitespace and punctuations:
(defun dtk-chunk-on-white-space-and-punctuations ()
  (cl-declare (special dtk-chunk-separator-syntax))
  (setq dtk-chunk-separator-syntax
        (concat dtk-chunk-separator-syntax "-")))

(defun dtk-chunk-only-on-punctuations ()
  (cl-declare (special dtk-chunk-separator-syntax))
  (setq dtk-chunk-separator-syntax
        (cl-delete-if
         #'(lambda (x) (= x ?-))
         dtk-chunk-separator-syntax)))

;;; invarianc: looking at complement
;;; move across the complement and the following separator
;;; return value is a boolean indicating if we moved.
;;; side-effect is to move across a chunk
(defun dtk-move-across-a-chunk (separator complement)
  "Move over a chunk of text.
Chunks are defined  based on major modes.
Argument SEPARATOR  is the syntax class of chunk separators.
Argument COMPLEMENT  is the complement of separator."
  (> (+ (skip-syntax-forward complement)
        (skip-syntax-forward separator))
     0))

(defun dtk-speak-using-voice (voice text)
  "Use voice VOICE to speak text TEXT."
  (cl-declare (special  tts-default-voice))
  (unless (or (eq 'inaudible voice)
              (null text) (string-equal text "")
              (and (listp voice) (memq 'inaudible voice)))
;;; ensure text is a  string
    (unless (stringp text) (setq text (format "%s" text)))
    (dtk-interp-queue-code
     (cond
      ((symbolp voice)
       (tts-get-voice-command
        (if (boundp voice)
            (symbol-value voice)
          voice)))
      ((listp voice)
       (mapconcat
        #'(lambda (v)
            (tts-get-voice-command
             (if (boundp v)
                 (symbol-value v)
               v)))
        voice
        " "))
      (t "")))
    (dtk-interp-queue text)
    (dtk-interp-queue-code (tts-voice-reset-code))))

(defun dtk-letter-using-voice (voice letter)
  "Use voice VOICE to speak letter."
  (cl-declare (special dtk-quiet tts-default-voice))
  (unless (or (eq 'inaudible voice) dtk-quiet
              (null letter) (string-equal letter ""))
    (dtk-interp-queue-code (tts-voice-reset-code))
    (dtk-interp-queue-code
     (cond
      ((symbolp voice)
       (tts-get-voice-command
        (if (boundp voice) (symbol-value voice) voice)))
      ((listp voice)
       (mapconcat
        #'(lambda (v)
            (tts-get-voice-command
             (if (boundp v) (symbol-value v) v)))
        voice " "))
      (t "")))
    (dtk-interp-letter letter)
    (dtk-interp-queue-code (tts-voice-reset-code))))

;;;Internal function used by dtk-speak to send text out.
;;;Handles voice locking etc.
;;; assumes in dtk-scratch-buffer
;;;start and end give the extent of the
;;;text to be spoken.
;;; note that property auditory-icon at the start  of a clause
;;; causes the sound
;;; to be queued.
;;;
;;; Similarly, property pause at the start of a clause specifies
;;; amount of pause to insert.

(defsubst dtk-next-single-property-change (start prop object limit)
  (let ((initial-value (get-text-property start prop object)))
    (cond
     ((atom initial-value)
      (next-single-property-change start prop object limit))
     (t
      (let ((pos start))
        (while (and (< pos limit)
                    (equal initial-value (get-text-property pos prop object)))
          (setq pos (next-single-property-change pos prop object limit)))
        pos)))))

;;; Get position of previous style change from start to end. Here, style
;;; change is any change in property personality, face or font-lock-face.

(defsubst dtk-previous-style-change (start &optional end)
  (or end (setq end (point-min)))
  (max
   (previous-single-property-change start 'personality (current-buffer) end)
   (previous-single-property-change start 'face (current-buffer) end)
   (previous-single-property-change start 'font-lock-face (current-buffer) end)))

;;; Get position of next style change from start   to end.
;;; Here,  change is any change in property personality, face or font-lock-face.
(defsubst dtk-next-style-change (start &optional end)
  (or end (setq end (point-max)))
  (min
   (dtk-next-single-property-change start 'personality (current-buffer) end)
   (dtk-next-single-property-change start 'face (current-buffer) end)
   (dtk-next-single-property-change start 'font-lock-face (current-buffer) end)))

(defun dtk-audio-format (start end)
  "Format and speak text from `start' to `end'. "
  (cl-declare (special voice-lock-mode dtk-speaker-process
                       tts-default-voice emacspeak-use-auditory-icons))
  (when (and emacspeak-use-auditory-icons
             (get-text-property start 'auditory-icon))
    (emacspeak-queue-auditory-icon (get-text-property start 'auditory-icon)))
  (dtk-interp-queue-code (tts-voice-reset-code))
  (when-let ((pause  (get-text-property start 'pause))
             (dtk-interp-silence pause)))
  (cond
   ((not voice-lock-mode)
    (dtk-interp-queue (buffer-substring-no-properties start end)))
   (t                                   ; voiceify as we go
    (let ((last nil)
          (personality (dtk-get-style start)))
      (while
          (and
           (< start end)
           (setq last (dtk-next-style-change start end)))
        (if personality
            (dtk-speak-using-voice
             personality (buffer-substring-no-properties start last))
          (dtk-interp-queue (buffer-substring-no-properties start last)))
        (setq
         start last
         personality (dtk-get-style last))
        (when (get-text-property start 'pause)
          (dtk-interp-silence (get-text-property start 'pause) nil)))))))

;;;Force the speech.
(defalias 'dtk-force 'dtk-interp-speak)

;;;Write out the string to the tts via TCL.
;;; No quoting is done,
;;; ifyou want to quote the text, see dtk-speak

(defun dtk-dispatch (string)
  "Send request  to speech server."
  (cl-declare (special dtk-speaker-process
                       dtk-speak-server-initialized
                       dtk-quiet))
  (unless dtk-quiet
    (when dtk-speak-server-initialized
      (dtk-interp-say string))))

(defun dtk-stop (&optional all)
  "Stop speech.
Optional arg `all' or interactive call   silences notification stream as well."
  (interactive "P")
  (when (process-live-p dtk-speaker-process) (dtk-interp-stop))
  (when
      (and (dtk-notify-process) (or all (called-interactively-p 'interactive)))
    (dtk-notify-stop)))

(defun dtk-reset-default-voice ()
  (cl-declare (special tts-default-voice))
  (dtk-dispatch (tts-get-voice-command tts-default-voice)))

;;}}}
;;{{{  adding cleanup patterns:

(defun dtk-add-cleanup-pattern (&optional delete)
  "Add this pattern to the list of repeating patterns.
  Optional interactive prefix arg deletes
this pattern if previously added.    "
  (interactive "P")
  (cl-declare (special dtk-cleanup-repeats))
  (cond
   (delete
    (setq dtk-cleanup-repeats
          (delete
           (read-from-minibuffer "Specify repeating pattern to delete: ")
           dtk-cleanup-repeats)))
   (t
    (cl-pushnew (read-from-minibuffer "Specify repeating pattern: ")
                dtk-cleanup-repeats
                :test #'string-equal))))

;;}}}
;;{{{ helper --generate state switcher:

(defun ems-generate-switcher (command switch documentation)
  "Generate  command to switch  state."
  (eval
   `(defun ,command (&optional prefix)
      ,documentation
      (interactive "P")
      (cl-declare (special ,switch))
      (cond
       (prefix
        (setq-default ,switch
                      (not (default-value ',switch)))
        (setq ,switch (default-value ',switch)))
       (t (make-local-variable ',switch)
          (setq ,switch (not ,switch))))
      (dtk-interp-sync)
      (when (called-interactively-p 'interactive)
        (emacspeak-auditory-icon (if ,switch "on" "off"))
        (message
         (format "Turned %s %s  %s."
                 (if ,switch "on" "off")
                 ',switch
                 (if prefix "" " locally")))))))

;;}}}
;;{{{  sending commands

(defun dtk-set-rate (rate &optional prefix)
  "Set speaking RATE.
Interactive PREFIX arg means set   the global default value, and then set the
current local  value to the result."
  (interactive
   (list
    (read-from-minibuffer "Enter new rate: ")
    current-prefix-arg))
  (cl-declare (special dtk-speech-rate dtk-speaker-process
                       tts-default-speech-rate
                       dtk-program dtk-speak-server-initialized))
  (when dtk-speak-server-initialized
    (cond
     (prefix
      (unless (eq dtk-speaker-process (dtk-notify-process))
        (let ((dtk-speaker-process (dtk-notify-process)))
          (dtk-set-rate rate)))
      (setq tts-default-speech-rate rate)
      (setq-default dtk-speech-rate rate)
      (setq dtk-speech-rate rate))
     (t (setq dtk-speech-rate rate)))
    (dtk-interp-set-rate rate)
    (when (called-interactively-p 'interactive)
      (message "Set speech rate to %s %s"
               rate
               (if prefix "" "locally")))))

(defun dtk-set-predefined-speech-rate (&optional prefix)
  "Set speech rate to one of nine predefined levels.
Interactive PREFIX arg says to set the rate globally.
Formula used is:
rate = dtk-speech-rate-base + dtk-speech-rate-step * level."
  (interactive "P")
  (cl-declare (special dtk-speech-rate-step
                       dtk-speech-rate-base
                       last-input-event))
  (let ((level
         (condition-case nil
             (read (format "%c" last-input-event))
           (error nil))))
    (or (numberp level)
        (setq level
              (read-minibuffer "Enter level between 1 and 9 to set
speech rate:")))
    (cond
     ((or (not (numberp level))
          (< level 0)
          (> level 9))
      (error "Invalid level %s" level))
     (t (dtk-set-rate
         (+ dtk-speech-rate-base
            (* dtk-speech-rate-step level))
         prefix)
        (when (called-interactively-p 'interactive)
          (message "Set speech rate to level %s %s"
                   level
                   (if prefix "" "locally")))))))

(defun dtk-set-character-scale (factor &optional prefix)
  "Set character scale FACTOR for   speech rate.
Speech rate is scaled by this factor when speaking characters.
Interactive PREFIX arg means set the global default value, and
then set the current local value to the result."
  (interactive "nEnter new factor:\nP")
  (cl-declare (special dtk-character-scale dtk-speaker-process
                       dtk-speak-server-initialized))
  (when dtk-speak-server-initialized
    (cond
     (prefix
      (setq-default dtk-character-scale factor)
      (setq dtk-character-scale factor))
     (t (make-local-variable 'dtk-character-scale)
        (setq dtk-character-scale factor)))
    (dtk-interp-set-character-scale dtk-character-scale)
    (when (called-interactively-p 'interactive)
      (message "Set character scale factor to %s %s"
               dtk-character-scale
               (if prefix "" "locally")))))

(ems-generate-switcher
 'dtk-toggle-quiet
 'dtk-quiet
 "Toggles state of  dtk-quiet.
Turning on this switch silences speech.  Optional interactive
prefix arg causes this setting to become global.")

(ems-generate-switcher
 'dtk-toggle-split-caps
 'dtk-split-caps
 "Toggle split caps mode.
Split caps mode is useful when reading Hungarian notation in
program source code.  Interactive PREFIX arg means toggle the
global default value, and then set the current local value to the
result.")

(ems-generate-switcher
 'dtk-toggle-strip-octals
 'tts-strip-octals
 "Toggle stripping of octals.
Interactive prefix arg means
 toggle the global default value, and then set the current local
value to the result.")

(ems-generate-switcher
 'dtk-toggle-caps
 'dtk-caps
 "Toggle caps.
when set, capitalized words are preceded by a short `cap', and
upper-case words are preceded by a `ac' spoken in a lower voice.
Interactive PREFIX arg means toggle the global default value, and
then set the current local value to the result.")

(ems-generate-switcher
 'dtk-toggle-speak-nonprinting-chars
 'dtk-speak-nonprinting-chars
 "Toggle speak-nonprinting-chars.
Interactive PREFIX arg means toggle the global default
value, and then set the current local value to the result.")

(defun dtk-set-punctuations (mode &optional prefix)
  "Set punctuation mode to MODE.
Possible values are `some', `all', or `none'.
Interactive PREFIX arg means set   the global default value, and then set the
current local  value to the result."
  (interactive
   (list
    (intern
     (completing-read "Enter punctuation mode: "
                      dtk-punctuation-mode-alist
                      nil
                      t))
    current-prefix-arg))
  (cl-declare (special dtk-punctuation-mode dtk-speaker-process
                       dtk-speak-server-initialized
                       dtk-punctuation-mode-alist))
  (when dtk-speak-server-initialized
    (cond
     (prefix
      (setq dtk-punctuation-mode mode)
      (setq-default dtk-punctuation-mode mode))
     (t (make-local-variable 'dtk-punctuation-mode)
        (setq dtk-punctuation-mode mode)))
    (dtk-interp-set-punctuations mode)
    (when (called-interactively-p 'interactive)
      (message "set punctuation mode to %s %s"
               mode
               (if prefix "" "locally")))))

(defun dtk-set-punctuations-to-all (&optional prefix)
  "Set punctuation  mode to all.
Interactive PREFIX arg sets punctuation mode globally."
  (interactive "P")
  (dtk-set-punctuations 'all prefix))

(defun dtk-set-punctuations-to-some (&optional prefix)
  "Set punctuation  mode to some.
Interactive PREFIX arg sets punctuation mode globally."
  (interactive "P")
  (dtk-set-punctuations 'some prefix))

(defun dtk-toggle-punctuation-mode (&optional prefix)
  "Toggle punctuation mode between \"some\" and \"all\".
Interactive PREFIX arg makes the new setting global."
  (interactive "P")
  (cl-declare (special dtk-punctuation-mode))
  (cond
   ((eq 'all dtk-punctuation-mode)
    (dtk-set-punctuations-to-some prefix))
   ((eq 'some dtk-punctuation-mode)
    (dtk-set-punctuations-to-all prefix)))
  (when (called-interactively-p 'interactive)
    (message "set punctuation mode to %s %s"
             dtk-punctuation-mode
             (if prefix "" "locally"))))

(defun dtk-reset-state ()
  "Reset TTS engine."
  (interactive)
  (cl-declare (special dtk-speaker-process
                       dtk-speak-server-initialized))
  (when dtk-speak-server-initialized
    (dtk-interp-reset-state)))

(defun tts-speak-version ()
  "Speak version."
  (interactive)
  (dtk-interp-say-version))

;;}}}
;;{{{  Internal variables:

(defvar dtk-stop-immediately t
  "If t, speech stopped immediately when new speech received.
Emacspeak sets this to nil if the current message being spoken is too
important to be interrupted.")

(defvar dtk-speaker-process nil
  "Speaker process handle.")

(defvar dtk-notify-process nil
  "Notify speaker  process handle.")

(defvar-local dtk-punctuation-mode 'all
  "Punctuation state (some, all or none).
Set by \\[dtk-set-punctuations].")

(defvar dtk-servers-alist nil
  "Speech servers.")

(defun tts-setup-servers-alist ()
  "Read servers/.servers"
  (cl-declare (special emacspeak-servers-directory dtk-servers-alist))
  (let ((result nil)
        (servers
         (find-file-noselect
          (expand-file-name ".servers" emacspeak-servers-directory)))
        (this nil))
    (with-current-buffer servers
      (goto-char (point-min))
      (while (not (eobp))
        (unless
            (looking-at "^#")
          (setq this
                (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position)))
          (push this result))
        (forward-line 1)))
    (setq dtk-servers-alist result)))

;;}}}
;;{{{  Mapping characters to speech:

(defvar dtk-character-to-speech-table
  (make-vector 256 "")
  "Maps characters to pronunciation strings.")

;;;  Assign entries in the table:
(defun dtk-speak-setup-character-table ()
  "Setup pronunciations in the character table for theTTS engine."
  (let ((table dtk-character-to-speech-table))
    (aset table 0 "control at")
    (aset table 1 "control a")
    (aset table 2 "control b")
    (aset table 3 "control c")
    (aset table 4 "control d")
    (aset table 5 "control e")
    (aset table 6 "control f")
    (aset table 7 "control g")
    (aset table 8 "control h")
    (aset table 9 "tab")
    (aset table 10 "newline")
    (aset table 11 "control k")
    (aset table 12 "control l")
    (aset table 13 "control m")
    (aset table 14 "control n")
    (aset table 15 "control o")
    (aset table 16 "control p")
    (aset table 17 "control q")
    (aset table 18 "control r")
    (aset table 19 "control s")
    (aset table 20 "control t")
    (aset table 21 "control u")
    (aset table 22 "control v")
    (aset table 23 "control w")
    (aset table 24 "control x")
    (aset table 25 "control y")
    (aset table 26 "control z")
    (aset table 27 "escape")
    (aset table 28 "control[*]backslash")
    (aset table 29 "control[*]right bracket")
    (aset table 30 "control[*]caret")
    (aset table 31 "control[*]underscore")
    (aset table 32 "space")
    (aset table 33 "exclamation")
    (aset table 34 "quotes")
    (aset table 35 "pound")
    (aset table 36 "dollar")
    (aset table 37 "percent")
    (aset table 38 "ampersand")
    (aset table 39 "apostrophe")
    (aset table 40 "left[*]paren")
    (aset table 41 "right[*]paren")
    (aset table 42 "star")
    (aset table 43 "plus")
    (aset table 44 "comma")
    (aset table 45 "dash")
    (aset table 46 "dot")
    (aset table 47 "slash")
    (aset table 48 "zero")
    (aset table 49 "one")
    (aset table 50 "two")
    (aset table 51 "three")
    (aset table 52 "four")
    (aset table 53 "five")
    (aset table 54 "six")
    (aset table 55 "seven")
    (aset table 56 "eight")
    (aset table 57 "nine")
    (aset table 58 "colon")
    (aset table 59 "semi")
    (aset table 60 "less[*]than")
    (aset table 61 "equals")
    (aset table 62 "greater[*]than")
    (aset table 63 "question[*]mark")
    (aset table 64 "at")
    (aset table 65 " cap[*]a")
    (aset table 66 " cap[*]b")
    (aset table 67 "cap[*]c")
    (aset table 68 "cap[*]d")
    (aset table 69 "cap[*]e")
    (aset table 70 "cap[*]f")
    (aset table 71 "cap[*]g")
    (aset table 72 "cap[*]h")
    (aset table 73 "cap[*]i")
    (aset table 74 "cap[*]j")
    (aset table 75 "cap[*]k")
    (aset table 76 "cap[*]l")
    (aset table 77 "cap[*]m")
    (aset table 78 "cap[*]m")
    (aset table 79 "cap[*]o")
    (aset table 80 "cap[*]p")
    (aset table 81 "cap[*]q")
    (aset table 82 "cap[*]r")
    (aset table 83 "cap[*]s")
    (aset table 84 "cap[*]t")
    (aset table 85 "cap[*]u")
    (aset table 86 "cap[*]v")
    (aset table 87 "cap[*]w")
    (aset table 88 "cap[*]x")
    (aset table 89 "cap[*]y")
    (aset table 90 "cap[*]z")
    (aset table 91 "left[*]bracket")
    (aset table 92 "backslash")
    (aset table 93 "right[*]bracket")
    (aset table 94 "caret")
    (aset table 95 "underscore")
    (aset table 96 "backquote")
    (aset table 97 "a")
    (aset table 98 "b")
    (aset table 99 "c")
    (aset table 100 "d")
    (aset table 101 "e")
    (aset table 102 "f")
    (aset table 103 "g")
    (aset table 104 "h")
    (aset table 105 "i")
    (aset table 106 "j")
    (aset table 107 "k")
    (aset table 108 "l")
    (aset table 109 "m")
    (aset table 110 "n")
    (aset table 111 "o")
    (aset table 112 "p")
    (aset table 113 "q")
    (aset table 114 "r")
    (aset table 115 "s")
    (aset table 116 "t")
    (aset table 117 "u")
    (aset table 118 "v")
    (aset table 119 "w")
    (aset table 120 "x")
    (aset table 121 "y")
    (aset table 122 "z")
    (aset table 123 "left[*]brace")
    (aset table 124 "pipe")
    (aset table 125 "right[*]brace ")
    (aset table 126 "tilde")
    (aset table 127 "backspace")
;;; Characters with the 8th bit set:
    (aset table 128 " octal 200 ")
    (aset table 129 " ")                ;shows up on WWW pages
    (aset table 130 " octal 202 ")
    (aset table 131 " octal 203 ")
    (aset table 132 " octal 204 ")
    (aset table 133 " octal 205 ")
    (aset table 134 " octal 206 ")
    (aset table 135 " octal 207 ")
    (aset table 136 " octal 210 ")
    (aset table 137 " octal 211 ")
    (aset table 138 " octal 212 ")
    (aset table 139 " octal 213 ")
    (aset table 140 " octal 214 ")
    (aset table 141 " octal 215 ")
    (aset table 142 " octal 216 ")
    (aset table 143 " octal 217 ")
    (aset table 144 " octal 220 ")
    (aset table 145 " octal 221 ")
    (aset table 146 " '  ")
    (aset table 147 " quote  ")
    (aset table 148 " octal 224 ")
    (aset table 149 " octal 225 ")
    (aset table 150 " octal 226 ")
    (aset table 151 " octal 227 ")
    (aset table 152 " octal 230 ")
    (aset table 153 " octal 231 ")
    (aset table 154 " octal 232 ")
    (aset table 155 " octal 233 ")
    (aset table 156 " octal 234 ")
    (aset table 157 " octal 235 ")
    (aset table 158 " octal 236 ")
    (aset table 159 " octal 237 ")
    (aset table 160 "  ")               ;non breaking space
    (aset table 161 " octal 241 ")
    (aset table 162 " octal 242 ")
    (aset table 163 " octal 243 ")
    (aset table 164 " octal 244 ")
    (aset table 165 " octal 245 ")
    (aset table 166 " octal 246 ")
    (aset table 167 " octal 247 ")
    (aset table 168 " octal 250 ")
    (aset table 169 " copyright ")      ;copyright sign
    (aset table 170 " octal 252 ")
    (aset table 171 " octal 253 ")
    (aset table 172 " octal 254 ")
    (aset table 173 "-")                ;soft hyphen
    (aset table 174 " (R) ")            ;registered sign
    (aset table 175 " octal 257 ")
    (aset table 176 " octal 260 ")
    (aset table 177 " octal 261 ")
    (aset table 178 " octal 262 ")
    (aset table 179 " octal 263 ")
    (aset table 180 " octal 264 ")
    (aset table 181 " octal 265 ")
    (aset table 182 " octal 266 ")
    (aset table 183 " octal 267 ")
    (aset table 184 " octal 270 ")
    (aset table 185 " octal 271 ")
    (aset table 186 " octal 272 ")
    (aset table 187 " octal 273 ")
    (aset table 188 " octal 274 ")
    (aset table 189 " octal 275 ")
    (aset table 190 " octal 276 ")
    (aset table 191 " octal 277 ")
    (aset table 192 " octal 300 ")
    (aset table 193 " octal 301 ")
    (aset table 194 " octal 302 ")
    (aset table 195 " octal 303 ")
    (aset table 196 " octal 304 ")
    (aset table 197 " octal 305 ")
    (aset table 198 " octal 306 ")
    (aset table 199 " octal 307 ")
    (aset table 200 " octal 310 ")
    (aset table 201 " octal 311 ")
    (aset table 202 " octal 312 ")
    (aset table 203 " octal 313 ")
    (aset table 204 " octal 314 ")
    (aset table 205 " octal 315 ")
    (aset table 206 " octal 316 ")
    (aset table 207 " octal 317 ")
    (aset table 208 " octal 320 ")
    (aset table 209 " octal 321 ")
    (aset table 210 " octal 322 ")
    (aset table 211 " octal 323 ")
    (aset table 212 " octal 324 ")
    (aset table 213 " octal 325 ")
    (aset table 214 " octal 326 ")
    (aset table 215 " octal 327 ")
    (aset table 216 " octal 330 ")
    (aset table 217 " octal 331 ")
    (aset table 218 " octal 332 ")
    (aset table 219 " octal 333 ")
    (aset table 220 " octal 334 ")
    (aset table 221 " octal 335 ")
    (aset table 222 " octal 336 ")
    (aset table 223 " octal 337 ")
    (aset table 224 " octal 340 ")
    (aset table 225 " octal 341 ")
    (aset table 226 " octal 342 ")
    (aset table 227 " octal 343 ")
    (aset table 228 " octal 344 ")
    (aset table 229 " octal 345 ")
    (aset table 230 " octal 346 ")
    (aset table 231 " octal 347 ")
    (aset table 232 " octal 350 ")
    (aset table 233 " octal 351 ")
    (aset table 234 " octal 352 ")
    (aset table 235 " octal 353 ")
    (aset table 236 " octal 354 ")
    (aset table 237 " octal 355 ")
    (aset table 238 " octal 356 ")
    (aset table 239 " octal 357 ")
    (aset table 240 " octal 360 ")
    (aset table 241 " octal 361 ")
    (aset table 242 " octal 362 ")
    (aset table 243 " octal 363 ")
    (aset table 244 " octal 364 ")
    (aset table 245 " octal 365 ")
    (aset table 246 " octal 366 ")
    (aset table 247 " octal 367 ")
    (aset table 248 " octal 370 ")
    (aset table 249 " octal 371 ")
    (aset table 250 " octal 372 ")
    (aset table 251 " octal 373 ")
    (aset table 252 " octal 374 ")
    (aset table 253 " octal 375 ")
    (aset table 254 " octal 376 ")
    (aset table 255 " octal 377 ")))

(dtk-speak-setup-character-table)

(defun dtk-char-to-speech (char)
  "Translate CHAR to speech string."
  (cl-declare (special dtk-character-to-speech-table))
  (if (eq (char-charset char) 'ascii)
      (aref dtk-character-to-speech-table char)
    (or (dtk-unicode-short-name-for-char char)
        (format "octal %o" char))))

;;}}}
;;{{{  interactively selecting the server:

;;; will be reset on a per TTS engine basis.
(fset 'tts-get-voice-command 'dectalk-get-voice-command)

(defun tts-voice-reset-code ()
  "Return voice reset code."
  (tts-get-voice-command tts-default-voice))

(defvar tts-device "default"
  "Name of  sound device.")

(defcustom dtk-cloud-server "cloud-outloud"
  "Set this to your preferred cloud TTS server."
  :type '(string
          (choice
           (:const "cloud-outloud" :tag "Outloud Variants")
           (:const "cloud-dtk" :tag "DTK Variants")
           (:const "cloud-espeak" :tag "ESpeak Variants")
           (:const "cloud-mac" :tag "Mac Variants")))
  :group 'dtk)

(defun dtk-select-server (program &optional device)
  "Select  speech server `program'.
 Optional arg device sets  env-var ALSA_DEFAULT."
  (interactive
   (list
    (completing-read
     "Speech server:"
     (or dtk-servers-alist (tts-setup-servers-alist))
     nil t)
    current-prefix-arg))
  (cl-declare (special dtk-program dtk-servers-alist
                       tts-device emacspeak-servers-directory))
  (when  device
    (setq tts-device
          (completing-read
           "Device: "
           (split-string (shell-command-to-string "aplay -L | grep tts"))
           nil nil nil nil "default"))
    (setenv "ALSA_DEFAULT" tts-device))
  (setq dtk-program program)
  (when (called-interactively-p 'interactive)
    (ems--fastload "voice-setup")
    (dtk-initialize)))

(defsubst tts-multistream-p (tts-engine)
  "Checks if this tts-engine can support multiple streams."
  (cl-declare (special tts-notification-device))
  (and
   (member tts-engine '("outloud"  "cloud-outloud"))
   (not (string= tts-notification-device "default"))))

(defun dtk-cloud ()
  "Select  Cloud TTS server."
  (interactive)
  (cl-declare (special dtk-cloud-server))
  (dtk-select-server dtk-cloud-server)
  (dtk-initialize)
  (when (tts-multistream-p dtk-cloud-server)
    (dtk-notify-initialize)))

(defvar dtk-local-server-process nil
  "Local server process.")

(defvar dtk-speech-server-program "speech-server"
  "Local speech server script.")
(defvar dtk-local-server-port "2222"
  "Port where we run our local server.")

(defcustom dtk-local-engine "outloud"
  "Engine we use  for our local TTS  server."
  :type '(choice
          (const :tag "Dectalk Express" "dtk-exp")
          (const :tag "Viavoice Outloud" "outloud")
          (const :tag "32Bit ViaVoice on 64Bit Linux" "32-outloud"))
  :group 'dtk)

(defun dtk-local-server (program &optional prompt-port)
  "Select and start an local speech server interactively. Local server
lets Emacspeak on a remote host connect back via SSH port forwarding
for instance. Argument PROGRAM specifies the speech server
program. Port defaults to dtk-local-server-port"
  (interactive
   (list
    (completing-read
     "Select speech server:"
     (or dtk-servers-alist
         (tts-setup-servers-alist))
     nil
     t
     nil nil
     dtk-program)
    current-prefix-arg))
  (cl-declare (special dtk-servers-alist dtk-local-server-port
                       dtk-local-server-process emacspeak-servers-directory))
  (setq dtk-local-server-process
        (start-process
         "LocalTTS"
         "localTTS*"
         (expand-file-name dtk-speech-server-program emacspeak-servers-directory)
         (if prompt-port
             (read-from-minibuffer "Port:" "3333")
           dtk-local-server-port)
         (expand-file-name program emacspeak-servers-directory))))

;;}}}
;;{{{  initialize the speech process

(defcustom tts-notification-device
  (eval-when-compile
    (or (getenv "ALSA_NOTIFY")
        (cl-first
         (split-string
          (shell-command-to-string  "aplay -L 2>/dev/null | grep mono")))))
  "Virtual ALSA device to use for notifications stream.
Set to nil to disable a separate Notification stream."
  :type '(choice
          (const :tag "None" nil)
          (string :value ""))
  :group 'tts)

(defvar dtk-speak-server-initialized nil
  "Records if the server is initialized.")

;;; Helper: dtk-make-process:
(defun dtk-make-process (name)
  "Make a  TTS process called name."
  (cl-declare (special dtk-program  emacspeak-servers-directory))
  (let ((process-connection-type nil)
        (program (expand-file-name dtk-program emacspeak-servers-directory))
        (process nil))
    (setq process (start-process name nil program))
    (set-process-coding-system process 'utf-8 'utf-8)
    process))

(defun dtk-initialize ()
  "Initialize speech system."
  (cl-declare (special dtk-speaker-process
                       dtk-speak-server-initialized
                       dtk-program))
  (let* ((new-process (dtk-make-process "Speaker"))
         (state (process-status new-process)))
    (setq dtk-speak-server-initialized (memq state '(run open)))
    (cond
     (dtk-speak-server-initialized ;; nuke old server
      (when (and dtk-speaker-process (process-live-p dtk-speaker-process))
        (delete-process dtk-speaker-process))
      (setq dtk-speaker-process new-process)
      (when (process-live-p dtk-notify-process) (delete-process dtk-notify-process))
      (when (tts-multistream-p dtk-program) (dtk-notify-initialize))))))

(defun tts-restart ()
  "Restart TTS server."
  (interactive)
  (dtk-initialize)
  (dtk-interp-sync))

;;}}}
;;{{{  interactively select how text is split:

(defun dtk-toggle-splitting-on-white-space ()
  "Toggle splitting of speech on white space. "
  (interactive)
  (cl-declare (special dtk-chunk-separator-syntax))
  (cond
   ((not (string-match "-" dtk-chunk-separator-syntax))
    (dtk-chunk-on-white-space-and-punctuations)
    (when (called-interactively-p 'interactive)
      (message "Text will be split at punctuations and white space when speaking")))
   (t (dtk-chunk-only-on-punctuations)
      (when (called-interactively-p 'interactive)
        (message "Text split  at clause boundaries")))))

(defun dtk-set-chunk-separator-syntax (s)
  "Interactively set how text is split in chunks.
Argument S specifies the syntax class."
  (interactive
   (list
    (read-from-minibuffer "Specify separator syntax string: ")))
  (cl-declare (special dtk-chunk-separator-syntax))
  (setq dtk-chunk-separator-syntax s)
  (when (called-interactively-p 'interactive)
    (message "Set  separator to %s" s)))

;;}}}
;;{{{ speak text

(defvar-local dtk-yank-excluded-properties
  '(category field follow-link fontified font-lock-face help-echo
             keymap local-map mouse-face read-only intangible yank-handler)
  "Like yank-excluded-properties, but without  invisible
 in it.
This is so text marked invisible is silenced.")

(defun dtk-speak (text)
  "Speak the TEXT string
unless   `dtk-quiet' is set to t. "
  (cl-declare (special dtk-yank-excluded-properties
                       dtk-speaker-process dtk-stop-immediately
                       tts-strip-octals inhibit-point-motion-hooks
                       dtk-speak-server-initialized emacspeak-use-auditory-icons
                       dtk-speech-rate dtk-speak-nonprinting-chars
                       dtk-quiet dtk-chunk-separator-syntax inhibit-modification-hooks
                       voice-lock-mode dtk-punctuation-mode
                       dtk-split-caps
                       emacspeak-pronounce-pronunciation-table selective-display))
;;; ensure text is a  string
  (unless (stringp text) (setq text (format "%s" text)))
;;; ensure  the process  is live
  (unless (process-live-p dtk-speaker-process) (dtk-initialize))
;;; If you dont want me to talk,or my server is not running,
;;; I will remain silent.
;;; I also do nothing if text is nil or ""
  (unless
      (or dtk-quiet (not dtk-speak-server-initialized)
          (null text) (zerop (length text)))
;;; flush previous speech if asked to
    (when dtk-stop-immediately
      (when (process-live-p dtk-notify-process) (dtk-notify-stop))
      (dtk-stop))
    (when selective-display
      (let ((ctrl-m (string-match "\015" text)))
        (and ctrl-m (setq text (substring text 0 ctrl-m))
             (emacspeak-auditory-icon 'ellipses))))
    (let ((inhibit-point-motion-hooks t) ;snapshot relevant state
          (yank-excluded-properties  yank-excluded-properties)
          (inhibit-read-only t)
          (inhibit-modification-hooks t)
          (deactivate-mark nil)
          (invisibility-spec buffer-invisibility-spec)
          (syntax-table (syntax-table))
          (inherit-speaker-process dtk-speaker-process)
          (pron-table emacspeak-pronounce-pronunciation-table)
          (pron-personality emacspeak-pronounce-personality)
          (use-auditory-icons emacspeak-use-auditory-icons)
          (chunk-sep dtk-chunk-separator-syntax)
          (inherit-speak-nonprinting-chars dtk-speak-nonprinting-chars)
          (inherit-strip-octals tts-strip-octals)
          (complement-sep (dtk-complement-chunk-separator-syntax))
          (speech-rate dtk-speech-rate)
          (caps dtk-caps)
          (split-caps dtk-split-caps)
          (dtk-scratch-buffer (get-buffer-create " *dtk-scratch-buffer* "))
          (start 1)
          (end nil)
          (mode dtk-punctuation-mode)
          (voice-lock voice-lock-mode)) ; done snapshotting
      (with-current-buffer dtk-scratch-buffer
        (setq buffer-undo-list t)
        (erase-buffer)
;;; inherit environment
        (setq
         yank-excluded-properties dtk-yank-excluded-properties
         emacspeak-pronounce-personality pron-personality
         buffer-invisibility-spec invisibility-spec
         dtk-chunk-separator-syntax chunk-sep
         dtk-speaker-process inherit-speaker-process
         dtk-speech-rate speech-rate
         emacspeak-use-auditory-icons use-auditory-icons
         dtk-punctuation-mode mode
         dtk-split-caps split-caps
         dtk-caps caps
         dtk-speak-nonprinting-chars inherit-speak-nonprinting-chars
         tts-strip-octals inherit-strip-octals
         voice-lock-mode voice-lock)
        (set-syntax-table syntax-table)
        (dtk-interp-sync)
        (insert-for-yank text)          ; insert and pre-process text
        (dtk--delete-invisible-text)
        (dtk-handle-repeating-patterns mode)
        (when pron-table (tts-apply-pronunciations pron-table))
        (dtk-unicode-replace-chars mode)
        (dtk-quote mode)
        (goto-char (point-min))         ; text is ready to be spoken
        (skip-syntax-forward "-")      ;skip leading whitespace
        (setq start (point))
        (while (and (not (eobp))
                    (dtk-move-across-a-chunk chunk-sep complement-sep))
          (unless ;;;If  embedded punctuations, continue
              (and (char-after (point))
                   (= ?. (char-syntax (preceding-char)))
                   (not (= 32 (char-syntax (following-char)))))
            (skip-syntax-forward "-")   ;skip  trailing whitespace
            (setq end (point))
            (dtk-audio-format start end)
            (setq start end)))          ; end while
;;; process trailing text
        (unless (= start (point-max))
          (skip-syntax-forward " ")     ;skip leading whitespace
          (unless (eobp) (dtk-audio-format (point) (point-max))))))
    (dtk-force)))

(defmacro ems-with-messages-silenced (&rest body)
  "Evaluate body  after temporarily silencing messages."
  (declare (indent 1) (debug t))
  `(let ((emacspeak-speak-messages nil)
         (inhibit-message t))
     ,@body))

(defun dtk-speak-and-echo (message)
  "Speak message and echo it."
  (ems-with-messages-silenced
      (dtk-speak message)
    (message "%s" message)))

(defun dtk-speak-list (text &optional group)
  "Speak a  list of strings.
 Optional argument group specifies grouping for intonation.  If
`group' is a list, it should specify split points where clause
boundaries are inserted.  Otherwise it is a number that specifies
grouping"
  (cl-declare (special dtk-speaker-process))
  (unless group (setq group 3))
  (when (numberp group)
;;; Create split list
    (setq group
          (let ((q (/ (length text) group))
                (r (% (length text) group))
                (splits nil))
            (setq splits (cl-loop for i from 0 to (1- q) collect group))
            (if (zerop r)
                splits
              `(,@splits ,r)))))
  (cl-assert
   (= (length text) (apply #'+ group)) group "Argument mismatch:" text group)
  (let ((dtk-scratch-buffer (get-buffer-create " *dtk-scratch-buffer* "))
        (contents nil)
        (count 1)
        (inhibit-read-only t))
    (save-current-buffer
      (set-buffer dtk-scratch-buffer)
      (setq buffer-undo-list t)
      (erase-buffer)
      (cl-loop
       for element in text do
       (let
           ((p (and (stringp element)
                    (get-text-property 0 'personality element))))
         (if (stringp element)
             (insert element)
           (insert (format " %s" element)))
         (cond
          ((= count (car group))
           (setq count 1)
           (pop group)
           (if p
               (insert (propertize ", " 'personality p))
             (insert ", ")))
          (t (cl-incf count)
             (insert " ")))))
      (setq contents (buffer-string)))
    (tts-with-punctuations 'some (dtk-speak contents))
    t))

(defun dtk-letter (letter)
  "Speak a LETTER."
  (cl-declare (special dtk-speaker-process
                       dtk-speak-server-initialized
                       dtk-quiet))
  (unless dtk-quiet
    (when dtk-speak-server-initialized
      (dtk-interp-letter letter))))

(defun dtk-say (words)
  "Say these WORDS."
  (cl-declare (special dtk-speaker-process dtk-stop-immediately
                       dtk-speak-server-initialized dtk-quiet))
                                        ; ensure words is a  string
  (unless (stringp words) (setq words (format "%s" words)))
  ;; I won't talk if you dont want me to
  (unless
      (or dtk-quiet (string-equal words ""))
    (or (eq 'run (process-status dtk-speaker-process))
        (eq 'open (process-status dtk-speaker-process))
        (dtk-initialize))
    (when dtk-speak-server-initialized
      (when dtk-stop-immediately
        (when (process-live-p dtk-notify-process) (dtk-notify-stop))
        (dtk-stop))
      (dtk-interp-say words))))

;;}}}
;;{{{ Notify:

(defun dtk-notify-process ()
  "Return  Notification TTS handle or nil. "
  (cl-declare (special dtk-notify-process dtk-speaker-process
                       dtk-program))
  (let ((result
         (cond
          ((null dtk-notify-process) dtk-speaker-process)
          ((and (processp dtk-notify-process)
                (memq (process-status dtk-notify-process) '(open run)))
           dtk-notify-process)
          (t dtk-speaker-process))))
    (when (process-live-p result) result)))

(defun dtk-notify-stop ()
  "Stop  speech on notification stream."
  (interactive)
  (let ((dtk-speaker-process (dtk-notify-process)))
    (when dtk-speaker-process (dtk-stop))))

(defun dtk-notify-apply (func text)
  " Applies func to text with dtk-speaker-process bound to notification stream."
  (let ((dtk-speaker-process (dtk-notify-process)))
    (funcall func text)))
(declare-function emacspeak-log-notification "emacspeak-speak" (text))

(defun dtk-notify-speak (text &optional dont-log)
  "Speak text on notification stream.
Notification is logged in the notifications buffer unless `dont-log' is T. "
  (cl-declare (special dtk-speaker-process emacspeak-last-message))
  (unless dont-log (emacspeak-log-notification text))
  (setq emacspeak-last-message text)
  (cond
   ((dtk-notify-process)                ; we have a live notifier
    (dtk-notify-apply #'dtk-speak text))
   (t (dtk-speak text)))
  text)

(defun dtk-notify-say (text &optional dont-log)
  "Say text on notification stream. "
  (cl-declare (special dtk-speaker-process emacspeak-last-message))
  (unless dont-log (emacspeak-log-notification text))
  (setq emacspeak-last-message text)
  (cond
   ((dtk-notify-process)                ; we have a live notifier
    (dtk-notify-apply #'dtk-say text))
   (t (dtk-say text)))
  text)

(defun dtk-notify-letter (letter)
  "Speak letter on notification stream. "
  (cond
   ((dtk-notify-process)                ; we have a live notifier
    (dtk-notify-apply #'dtk-letter letter))
   (t (dtk-letter letter))))

(defun dtk-notify-icon (icon)
  "Play icon  on notification stream. "
  (cond
   ((dtk-notify-process)                ; we have a live notifier
    (dtk-notify-apply #'emacspeak-auditory-icon icon))))

(defsubst dtk-get-notify-alsa-device ()
  "Returns name of Alsa device for use as the notification stream."
  (cl-declare (special tts-notification-device))
  (or tts-notification-device (getenv "ALSA_DEFAULT")))

(defun dtk-notify-initialize ()
  "Initialize notification TTS stream."
  (interactive)
  (cl-declare (special dtk-notify-process))
  (let ((save-device (getenv "ALSA_DEFAULT"))
        (device (dtk-get-notify-alsa-device))
        (dtk-program
         (if
             (string-match "cloud" dtk-program)
             "cloud-notify"
           dtk-program))
        (new-process nil))
    (setenv "ALSA_DEFAULT" device)
    (setq new-process (dtk-make-process "Notify"))
    (when
        (memq (process-status new-process) '(run open))
      (when (and dtk-notify-process (process-live-p dtk-notify-process))
        (delete-process dtk-notify-process))
      (setenv "ALSA_DEFAULT" save-device)
      (setq dtk-notify-process new-process))))

(defun dtk-notify-using-voice (voice text &optional dont-log)
  "Use voice VOICE to speak text TEXT on notification stream."
  (unless dont-log (emacspeak-log-notification text))
  (setq emacspeak-last-message text)
  (let ((dtk-speaker-process (dtk-notify-process)))
    (when (process-live-p dtk-speaker-process)
      (dtk-speak-using-voice voice text)
      (dtk-force))))

;;}}}

;;; Importing dtk-interp by inclusion:

;;{{{ macros

(defmacro tts-with-punctuations (setting &rest body)
  "Set punctuation  and exec   body."
  (declare (indent 1) (debug t))
  `(let ((save-punctuation-mode dtk-punctuation-mode))
     (unless (eq ,setting save-punctuation-mode)
       (dtk-interp-set-punctuations ,setting)
       (setq dtk-punctuation-mode ,setting))
     ,@body
     (unless (eq ,setting save-punctuation-mode)
       (setq dtk-punctuation-mode save-punctuation-mode)
       (dtk-interp-set-punctuations save-punctuation-mode))))

;;}}}
;;{{{ silence

(defsubst dtk-interp-silence (duration &optional force)
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "sh %d%s\n"
                               duration
                               (if force "\nd" ""))))

;;}}}
;;{{{  tone

(defsubst dtk-interp-tone (pitch duration &optional force)
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "t %d %d%s\n"
                               pitch duration
                               (if force "\nd" ""))))
;;}}}
;;{{{  queue

(defsubst dtk-interp-queue (text)
  (cl-declare (special dtk-speaker-process))
  (unless (string-match "^[[:space:]]+$" text)
    (process-send-string dtk-speaker-process (format "q {%s }\n" text))))

(defsubst dtk-interp-queue-code (code)
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "c {%s }\n" code)))

;;}}}
;;{{{  speak

(defsubst dtk-interp-speak ()
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process "d\n"))

;;}}}
;;{{{ say

(defsubst dtk-interp-say (string)
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process (format "tts_say { %s}\n" string)))

;;}}}
;;{{{ stop

(defsubst dtk-interp-stop ()
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process "s\n"))

;;}}}
;;{{{ sync

(defsubst dtk-interp-sync ()
  (cl-declare (special dtk-speaker-process
                       dtk-punctuation-mode dtk-speech-rate dtk-split-caps))
  (process-send-string
   dtk-speaker-process
   (format "tts_sync_state %s %s %s\n"
           dtk-punctuation-mode
           (if dtk-split-caps 1 0)
           dtk-speech-rate)))

;;}}}
;;{{{  letter

(defsubst dtk-interp-letter (letter)
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "l {%s}\n" letter)))

;;}}}
;;{{{  language

(defsubst dtk-interp-next-language (&optional say_it)
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "set_next_lang %s\n" say_it)))

(defsubst dtk-interp-previous-language (&optional say_it)
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "set_previous_lang %s\n" say_it)))

(defsubst dtk-interp-language (language say_it)
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "set_lang %s %s \n" language say_it)))

(defsubst dtk-interp-preferred-language (alias language)
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "set_preferred_lang %s %s \n" alias language)))

(defsubst dtk-interp-list-language ()
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "list_lang\n")))

;;}}}
;;{{{  Version, rate

(defsubst dtk-interp-say-version ()
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process "version\n"))

(defsubst dtk-interp-set-rate (rate)
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "tts_set_speech_rate %s\n"
                               rate)))

;;}}}
;;{{{ character scale

(defsubst dtk-interp-set-character-scale (factor)
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "tts_set_character_scale %s\n"
                               factor)))

;;}}}
;;{{{  split caps

(defsubst dtk-interp-toggle-split-caps (flag)
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "tts_split_caps %s\n"
                               (if flag 1 0))))

;;}}}
;;{{{ punctuations

(defsubst dtk-interp-set-punctuations (mode)
  (cl-declare (special dtk-speaker-process))
  (process-send-string
   dtk-speaker-process
   (format "tts_set_punctuations %s\nd\n" mode)))

;;}}}
;;{{{ reset

(defsubst dtk-interp-reset-state ()
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process "tts_reset \n"))

;;}}}
;;; Include dtk-unicode.el

;;; dtk-unicode.el --- Pronounce Unicode characters
;;{{{ Header: Lukas

;; Copyright 2007, 2011 Lukas Loehrer
;;; TVR: Integrated into Emacspeak July 6, 2008
;;; Using patch from Lukas.
;;
;; Author: Lukas Loehrer <loehrerl |at| gmx.net>
;; Version: $Id$
;; Keywords:  TTS, Unicode

;;}}}
;;{{{ Customizations

(defcustom dtk-unicode-character-replacement-alist
  '(
    (? . "-")                       ; START OF GUARDED AREA
    (?━ . "-")                          ; horiz bars
    (?┃ . "|")                          ; vertical block
    (?° . " degrees ")                  ; degree sign
    (?℃ . "Degree C")                   ; celsius
    (?℉ . "Degree F ")                  ; Fahrenheit
    (?“ . "\"")                         ;LEFT DOUBLE QUOTATION MARK
    (?” . "\"")                         ; RIGHT DOUBLE QUOTATION MARK
    (?⋆ . "*")                          ; STAR OPERATOR
    (?­ . "-")                          ; soft-hyphen
    (?‘ . "`")                          ; LEFT SINGLE QUOTATION MARK
    (?’ . "'")                          ; right SINGLE QUOTATION MARK
    (?‐ . "-")                          ; hyphenm
    (?– . " -- ")                       ; n-dash
    (?— . " --- ")                      ; m-dash
    (?  . " ")                          ; hair space
    (?﻿ . " ")                           ; zero-width  no-break space
    (?‌ . "") ; zero width non-joiner
    (?​ . " ")                           ; zero-width space
    (?  . " ")                          ; thin space
    (?― . "----")                       ; horizontal bar
    (?‖ . "||")                         ; vertical bar
    (?… . "...")                        ; ellipses
    (?• . " bullet ")                   ; bullet
    (? . " ... ")                   ; message-waiting
    (?™ . "TM")                         ; trademark
    (?ﬀ . "ff")                         ; latin small ligature ff
    (?ﬁ . "fi")                         ; latin small ligature fi
    (?ﬂ . "fl")                         ; latin small ligature fl
    (?ﬃ . "ffi")                        ; latin small ligature ffi
    (?ﬄ . "Ffl")                        ; latin small ligature ffl
    )
  "Replacements for  characters."
  :group 'dtk
  :type '(alist
          :key-type (character :tag "character")
          :value-type (string :tag "replacement")))

(defcustom dtk-unicode-name-transformation-rules-alist
  '(
    ("BOX DRAWING" . (lambda (_ignored) "."))
    ("^Mathematical Sans-Serif\\( small\\| capital\\)? letter \\(.*\\)$"
     . (lambda (s) (match-string 2 s)))
    ("^greek\\( small\\| capital\\)? letter \\(.*\\)$"
     . (lambda (s) (match-string 2 s)))
    ("^latin\\( small\\| capital\\)? letter \\(.*\\)$" . (lambda (s)
                                                           (match-string 2 s)))
    ("^DEVANAGARI \\(sign\\|vowel sign\\|letter\\)? \\(.*\\)$"
     . (lambda (s) (match-string 2 s)))

    )
  "Alist of character name transformation rules."
  :group 'dtk
  :type
  '(repeat
    (cons :value ("." . identity)
          (regexp :tag "pattern")
          (function :tag "transformation"))))

;;}}}
;;{{{ Variables

(defcustom dtk-unicode-untouched-charsets
  '(ascii latin-iso8859-1)
  "Characters of these charsets are  ignored by
  dtk-unicode-replace-chars."
  :group 'dtk
  :type '(repeat symbol))

(defvar dtk-unicode-handlers
  '(dtk-unicode-user-table-handler dtk-unicode-full-table-handler)
  "List of functions which are called in this order for replacing
an unspeakable character.  A handler returns a non-nil value if
the replacement was successful, nil otherwise.")

;;}}}
;;{{{ Helper functions

(defun dtk-unicode-charset-limits (charset)
  "Return rough lower and upper limits for character codes in CHARSET."
  (cond
   ((eq charset 'ascii)
    (list 0 127))
   ((eq charset 'eight-bit-control)
    (list 128 159))
   ((eq charset 'eight-bit-graphic)
    (list 160 255))
   (t
    (let* ((chars (charset-chars charset))
           min max)
      (if (eq chars 96)
          (setq min 32 max 127)
        (setq min 33 max 126))
      (list (make-char charset min min) (make-char charset max max))))))

(defun dtk-unicode-build-skip-regexp (charsets)
  "Construct regexp to match all but the characters in
dtk-unicode-untouched-charsets."
  (format "[^%s]"
          (cl-loop for charset in charsets
                   when (charsetp charset)
                   concat
                   (apply
                    #'format
                    "%c-%c" (dtk-unicode-charset-limits charset)))))

(defvar dtk-unicode-charset-filter-regexp
  (dtk-unicode-build-skip-regexp dtk-unicode-untouched-charsets)
  "Regular exppression that matches characters not in
  dtk-unicode-untouched-charsets.")

(defun dtk-unicode-update-untouched-charsets (charsets)
  "Update list of charsets we will not touch."
  (setq dtk-unicode-untouched-charsets charsets)
  (setq dtk-unicode-charset-filter-regexp
        (dtk-unicode-build-skip-regexp dtk-unicode-untouched-charsets)))
;;; Execute BODY like `progn' with CHARSETS at the front of priority list.
;;; CHARSETS is a list of charsets.  See
;;; `set-charset-priority'.  This affects the implicit sorting of lists of
;;; charsets returned by operations such as `find-charset-region'.

(defmacro dtk--with-charset-priority (charsets &rest body)
  (declare (indent 1) (debug t))
  (let ((current (make-symbol "current")))
    `(let ((,current (charset-priority-list)))
       (apply #'set-charset-priority ,charsets)
       (unwind-protect
           (progn ,@body)
         (apply #'set-charset-priority ,current)))))
;;; Now use it:

(defun dtk-unicode-char-in-charsets-p (char charsets)
  "Return t if CHAR is a member of one in the charsets in CHARSETS."
  (dtk--with-charset-priority charsets (memq (char-charset char) charsets)))

(defun dtk-unicode-char-untouched-p (char)
  "Return t if char is a member of one of the charsets in
dtk-unicode-untouched-charsets."
  (dtk-unicode-char-in-charsets-p char dtk-unicode-untouched-charsets))

(defvar dtk-unicode-cache (make-hash-table)
  "Cache for unicode data lookups.")

(defadvice describe-char-unicode-data (around emacspeak pre act comp)
  "Cache result."
  (let* ((char (ad-get-arg 0))
         (result (gethash char dtk-unicode-cache 'not-found)))
    (if (eq result 'not-found)
        (progn
          ad-do-it
          (puthash char ad-return-value dtk-unicode-cache))
      (setq ad-return-value result))))

(defun dtk-unicode-name-for-char (char)
  "Return unicode name for character CHAR. "
  (cond
   ((= char 128) "")
   (t
    (downcase
     (or
      (get-char-code-property char 'name)
      (get-char-code-property char 'old-name)
      (format "%c" char))))))

(defun dtk-unicode-char-punctuation-p (char)
  "Use unicode properties to determine whether CHAR is a
ppunctuation character."
  (let ((category (get-char-code-property char 'category))
        (case-fold-search t))
    (when (stringp category)
      (string-match "punctuation" category))))

(defun dtk-unicode-apply-name-transformation-rules (name)
  "Apply transformation rules in
dtk-unicode-name-transformation-rules-alist to NAME."
  (let ((case-fold-search t))
    (funcall
     (or
      (assoc-default
       name
       dtk-unicode-name-transformation-rules-alist 'string-match)
      'identity)
     name)))

(defun dtk-unicode-uncustomize-char (char)
  "Delete custom replacement for CHAR.

When called interactively, CHAR defaults to the character after point."
  (interactive (list (following-char)))
  (setq dtk-unicode-character-replacement-alist
        (cl-loop for elem in dtk-unicode-character-replacement-alist
                 unless (eq (car elem) char) collect elem)))

(defun dtk-unicode-customize-char (char replacement)
  "Add a custom replacement string for CHAR.

When called interactively, CHAR defaults to the character after point."
  (interactive
   (let ((char (following-char)))
     (list char
           (read-string
            (format
             "Replacement for %c (0x%x) from charset %s: "
             char char (char-charset char))))))
  (push (cons char replacement) dtk-unicode-character-replacement-alist))

;;}}}
;;{{{ Character replacement handlers

(defun dtk-unicode-user-table-handler (char)
  "Return user defined replacement character if it exists."
  (cdr (assq char dtk-unicode-character-replacement-alist)))

(defun dtk-unicode-full-table-handler (char)
  "Uses the unicode data file to find the name of CHAR."
  (let ((char-desc (dtk-unicode-name-for-char char)))
    (when char-desc
      (format
       " %s " (dtk-unicode-apply-name-transformation-rules char-desc)))))

;;}}}
;;{{{ External interface

(defun dtk-unicode-full-name-for-char (char)
  "Return full name of CHAR. "
  (dtk-unicode-name-for-char char))
(defun dtk-unicode-short-name-for-char (char)
  "Return short name of CHAR. "
  (if (memq char dtk-unicode-untouched-charsets)
      (char-to-string char)
    (dtk-unicode-name-for-char char)))

(defun dtk-unicode-replace-chars (mode)
  "Replace unicode characters with something  TTS friendly. "
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward dtk-unicode-charset-filter-regexp nil t)
        (let* ((pos (match-beginning 0))
               (char (char-after pos))
               (props (text-properties-at pos))
               (replacement
                (save-match-data
                  (if (and
                       (memq mode '(some none))
                       (dtk-unicode-char-punctuation-p char))
                      " "
                    (run-hook-with-args-until-success
                     'dtk-unicode-handlers char)))))
          (replace-match replacement t t nil)
          (when props
            (set-text-properties pos (point) props)))))))

;;}}}
;;; dtk-unicode.el ends here

(provide 'dtk-speak)
;;{{{  emacs local variables

;;; local variables:
;;; coding: utf-8
;;; folded-file: t
;;; end:

;;}}}
