;;; dtk-tcl.el --- Interface to TCL speech server --uses module dtk-interp.el
;;; $Id$
;;; $Author$
;;; Description:  Interfacing to the Dectalk via TCL.
;;; Keywords: Dectalk, TCL
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision: 1.2 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2003, T. V. Raman 
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

;;{{{ introduction

;;; Commentary:

;;; Interface to speech server.

;;}}}
;;{{{  required modules

;;; Code:
(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'custom)
(require 'dtk-interp)
(require 'dtk-voices)
(require 'emacspeak-pronounce)
;; (eval-when (compile)                                            ;;
;;   (provide 'dtk-tcl) ;;keep byte compiler from recursing        ;;
;;   (require 'dtk-speak))                                         ;;

;;}}}
;;{{{  Introduction:

;;; This module defines the interface to the
;;; various speech servers written in tcl
;;; This module is TCL  specific.

;;}}}
;;{{{  User customizations:

(defvar dtk-quiet nil
  "Switch indicating if the speech synthesizer is to keep quiet.
Do not set this variable by hand.
See command `dtk-toggle-quiet' bound to \\[dtk-toggle-quiet].")

(defvar dtk-split-caps t
  "Flag indicating whether to use split caps when speaking.
Do not set this variable by hand, use command  `dtk-toggle-split-caps'
 bound to \\[dtk-toggle-split-caps].")

(defcustom dtk-cleanup-patterns
  (list
   "." "_" "-"  "=" "/"  "+" "*" ":" ";" "%"
   "{" "}" "~" "$" ")" "#" "/\\" "<>" )
  "List of repeating patterns to clean up.
You can use  command  `dtk-add-cleanup-pattern'
 bound to \\[dtk-add-cleanup-pattern]  to add more patterns.
Specify patterns that people use to decorate their ASCII files, and cause
untold pain to the speech synthesizer.

If more than 3 consecutive occurrences
of a specified pattern is found, the TTS engine replaces it
with a repeat count. "
  :type '(repeat  (string :tag "pattern"))
  :group 'tts)

;;}}}
;;{{{  internal variables

(defvar dtk-character-scale 1.25
  "Factor by which speech rate is scaled when characters are spoken.
Do not set this variable by hand, use command
`dtk-set-character-scale' bound to \\[dtk-set-character-scale].")

(defvar dtk-capitalize nil
  "Non-nil means produce a beep to indicate  capitalization.
Do not set this variable by hand, use command dtk-toggle-capitalization
bound to \\[dtk-toggle-capitalization].")

(defvar dtk-allcaps-beep nil
  "Option to indicate capitalization.
Non-nil means produce a beep to indicate upper case words in conjunction with
split caps Do not set this variable by hand, use command
`dtk-toggle-allcaps-beep' bound to \\[dtk-toggle-allcaps-beep].")

(defconst dtk-punctuation-mode-alist
  '(("some" . "some" )
    ("all" . "all")
    ("none" . "none"))
  "Alist of valid punctuation modes.")
(defconst dtk-pronunciation-mode-alist
  '(("math" . "math")
    ("europe" . "europe")
    ("spell" . "spell")
    ("name" . "name"))
  "Alist of valid pronunciation  modes.")

(defvar dtk-last-output nil
  "Variable holding last output.")

(defvar dtk-speech-rate 425
  "Rate at which tts talks.
Do not modify this variable directly; use command  `dtk-set-rate'
 bound to \\[dtk-set-rate].")

(make-variable-buffer-local 'dtk-speech-rate)

;;;declared here to help compilation
(defvar voice-lock-mode nil)

;;}}}
;;{{{  Helpers to handle invisible text:

(defsubst text-visible-p (position)
  (not (get-text-property position 'invisible )))

(defsubst text-invisible-p (position)
  (get-text-property position 'invisible ))

(defsubst skip-invisible-forward  ()
  (and (text-invisible-p (point))
       (goto-char
        (next-single-property-change (point) 'invisible
                                     (current-buffer) (point-max)))))
;;; Delete invisible text from current buffer:
(defsubst skip-invisible-backward  ()
  "Move backwards over invisible text."
  (and (text-invisible-p (point))
       (goto-char
        (previous-single-property-change (point) 'invisible
                                         (current-buffer) (point-min)))))

(defsubst delete-invisible-text ()
  "Delete invisible text."
  (goto-char (point-min))
  (let ((start  (point )))
    (while (not (eobp))
      (cond
       ((text-invisible-p (point ))
        (skip-invisible-forward)
        (delete-region  start (point ))
        (setq start (point )))
       (t (goto-char
           (or (next-single-property-change (point) 'invisible )
               (point-max )))
          (setq start (point)))))))

;;}}}
;;{{{  define inline functions first

(defsubst dtk-silence (duration &optional force)
  "Produce silence.
Argument DURATION  specifies number of milliseconds to pause.
Optional argument FORCE  flushes the command to the speech server."
  (declare (special dtk-quiet dtk-speaker-process
                    dtk-speak-server-initialized))
  (unless dtk-quiet
    (when dtk-speak-server-initialized
      (dtk-interp-silence duration
                          (if force "\nd" "")))))

(defsubst dtk-notes-initialize()
  "Initialize midi system."
  (dtk-interp-notes-initialize))

(defun dtk-notes-shutdown()
  "Shutdown midi system."
  (interactive)
  (dtk-interp-notes-shutdown))

(defsubst dtk-queue-note (instrument pitch duration
                                     &optional target step)
  "Queue a midi note.
Instrument  is the instrument number.
Pitch is specified as 60 for middle C.
Argument DURATION  is specified in seconds.
Optional arguments target and step let you play chords."
  (declare (special dtk-quiet 
                    dtk-speak-server-initialized))
  (unless dtk-quiet
    (when dtk-speak-server-initialized
      (dtk-interp-note instrument  pitch duration
                       target step))))

(defsubst dtk-force-note (instrument pitch duration
                                     &optional target step)
  "Play a note immediately."
  (dtk-interp-note  instrument pitch duration
                    target step 'force))

(defsubst dtk-tone (pitch duration &optional force)
  "Produce a tone.
Argument PITCH   is specified in hertz.
Argument DURATION  is specified in milliseconds.
Optional argument FORCE  flushes the command to the speech server."
  (declare (special dtk-quiet dtk-speaker-process
                    dtk-speak-server-initialized))
  (unless dtk-quiet
    (when dtk-speak-server-initialized
      (dtk-interp-tone pitch duration force))))

;;; helper function:

;;; Quote the string in current buffer so tcl does not barf.
;;; Fix brackets by changing to text.
 ;;; This is necessary because
;;;  [] marks dtk commands; {} is special to tcl
;;; Optionally post-process the text with cleanup function if one is specified.
(defconst dtk-bracket-regexp
  "[][{}<>\\|`#]"
  "Brackets and other chars  that are special to dtk and tcl.
{} is special to tcl.
[] is special to both dtk and tcl.
<> and | are fixed to improve pronunciation.
\\ is fixed because it tends to be a metacharacter")

(defsubst  dtk-fix-brackets (mode)
  "Quote any delimiters that need special treatment.
Argument MODE  specifies the current pronunciation mode."
  (declare  (special dtk-bracket-regexp ))
  (goto-char (point-min))
  (cond
   ((string=  "all"  mode )
    (let ((start nil)
          (personality nil))
      (while (re-search-forward dtk-bracket-regexp  nil t )
        (setq start (1- (point)))
        (setq personality
              (get-text-property
               start 'personality))
        (cond
         ((= ?| (char-after (match-beginning 0 )))
          (replace-match " pipe "))
         ((= ?< (char-after (match-beginning 0 )))
          (replace-match " less than "))
         ((= ?> (char-after (match-beginning 0 )))
          (replace-match " greater than "))
         ((= ?{ (char-after (match-beginning 0 )))
          (replace-match " left brace "))
         ((= ?} (char-after (match-beginning 0 )))
          (replace-match " right brace "))
         ((=  ?\] (char-after (match-beginning 0)))
          (replace-match " right bracket "))
         ((= ?\[ (char-after  (match-beginning 0)))
          (replace-match " left bracket "))
         ((= ?\\ (char-after (match-beginning 0 )))
          (replace-match " backslash "))
         ((= ?# (char-after (match-beginning 0 )))
          (replace-match " pound "))
         ((= ?` (char-after (match-beginning 0 )))
          (replace-match " backquote ")))
        (when personality
          (put-text-property start (point)
                             'personality personality)))))
   (t
    (while (re-search-forward dtk-bracket-regexp   nil t )
      (replace-match " " nil t )))))

(defcustom dtk-speak-nonprinting-chars nil
  "*Option that specifies handling of non-printing chars.
Non nil value means non printing characters  should be
spoken as their octal value.
Set this to t to avoid a dectalk bug that makes the speech box die if
it seems some accented characters in certain contexts."
  :type 'boolean
  :group 'dtk)

(make-variable-buffer-local 'dtk-speak-nonprinting-chars)

(defvar dtk-octal-chars 
  (if
      (and (boundp 'default-enable-multibyte-characters)
           default-enable-multibyte-characters)
      "[\000-\010\013\014\016-\037\177-\377]"
    "[\000-\010\013\014\016-\037]")
  "Regular expression matching control chars.
Set this once per emacspeak session for efficiency.")

(defsubst dtk-fix-control-chars ()
  "Handle control characters in speech stream."
  (declare (special dtk-character-to-speech-table
                    dtk-octal-chars
                    dtk-speak-nonprinting-chars))
  (let ((char nil))
    (goto-char (point-min ))
    (when  dtk-speak-nonprinting-chars
      (while (re-search-forward dtk-octal-chars nil t )
        (setq char (char-after (match-beginning 0)))
        (replace-match
         (format " %s " (aref  dtk-character-to-speech-table char)))))))

;;; Takes a string, and replaces occurences of this pattern
;;; that are longer than 3 by a string of the form \"count
;;; string\". Second argument, mode, is the pronunciation
;;; mode being used to speak.  Removing repeated chars, and
;;; replacing them by a count:

(defsubst dtk-replace-duplicates (string mode)
  "Replace repeating patterns.
Argument STRING  specifies the repeating string to replace.
Argument MODE  specifies the current pronunciation mode."
  (let* ((len (length string))
         (pattern (regexp-quote string))
         (reg (concat
               pattern pattern
               "\\(" pattern  "\\)+"))
         (start nil)
         (personality nil)
         (replacement nil))
    (while (re-search-forward reg nil t)
      (setq personality
            (get-text-property (point) 'personality))
      (setq replacement
            (if  (string= "all" mode)
                (format " aw %s %s"
                        (/ (- (match-end 0 ) (match-beginning 0))
                           len)
                        (if (string= " " pattern)
                            " space " string))
              ""))
      (replace-match replacement)
      (setq start (- (point) (length replacement)))
      (when personality
        (put-text-property start (point)
                           'personality personality)))
    (goto-char (point-min))))

(defsubst  dtk-quote(mode )
  (declare (special dtk-cleanup-patterns))
  (goto-char (point-min))
      ;;; First cleanup  repeated patterns:
  (mapc
   (function (lambda (str)
               (dtk-replace-duplicates str mode )))
   dtk-cleanup-patterns )
    ;;; dtk will think it's processing a command otherwise:
  (dtk-fix-brackets mode)
  ;;; fix control chars
  (dtk-fix-control-chars))

(defsubst dtk-fix-backslash ()
  "Quote backslash characters as appropriate."
  (goto-char (point-min))
  (while (search-forward "\\" nil t)
    (replace-match " backslash ")))

;;; efficient quoting function for use in dtk-say
(defsubst  dtk-quick-quote(string )
  (let ((dtk-scratch-buffer (get-buffer-create " *dtk-scratch-buffer* "))
        (inhibit-read-only t))
    (save-excursion
      (set-buffer dtk-scratch-buffer))
    (erase-buffer)
    (insert string)
    (goto-char (point-min))
    ;;; dtk will think it's processing a command otherwise:
    (dtk-fix-brackets "all")
    (dtk-fix-backslash)
  ;;; fix control chars
    (dtk-fix-control-chars)))

;;; Moving  across a chunk of text.
;;; A chunk  is specified by a punctuation followed by whitespace
;;; or  multiple blank lines
;;; or a comment start or end
;;; or a parenthesis grouping start or end
;;; leaves point at the end of the chunk.
;;; returns  distance moved; nil if stationery
(defvar dtk-chunk-separator-syntax ".>)$\""
  "Syntax string to identify chunks when splitting text.")
                                        ; make it buffer local:
(make-variable-buffer-local 'dtk-chunk-separator-syntax)
(defsubst dtk-complement-chunk-separator-syntax ()
  "Return complement of syntactic class that splits clauses."
  (declare (special dtk-chunk-separator-syntax ))
  (concat "^" dtk-chunk-separator-syntax ))

;;; set chunk separator to match both whitespace and punctuations:
(defsubst dtk-chunk-on-white-space-and-punctuations()
  (declare (special dtk-chunk-separator-syntax))
  (setq dtk-chunk-separator-syntax
        (concat  dtk-chunk-separator-syntax " " )))

(defsubst dtk-chunk-only-on-punctuations()
  (declare (special dtk-chunk-separator-syntax))
  (setq dtk-chunk-separator-syntax
        (remove-if (function (lambda (x)
                               (= x 32 )))
                   dtk-chunk-separator-syntax)))

;;; invariance: looking at complement
;;; move across the complement and the following separator
;;; return value is a boolean indicating if we moved.
;;; side-effect is to move across a chunk
(defsubst  dtk-move-across-a-chunk (separator complement )
  "Move over a chunk of text.
Chunks are defined  based on major modes.
Argument SEPARATOR  is the syntax class of chunk separators.
Argument COMPLEMENT  is the complement of separator."
  (> (+ (skip-syntax-forward complement)
        (skip-syntax-forward separator))
     0))
      
;;; efficient way of voice changing

(defsubst dtk-speak-using-voice (voice text)
  "Use voice VOICE to speak text TEXT."
  (declare (special tts-voice-reset-code))
  (unless (eq 'inaudible voice )
    (dtk-interp-queue
     (format "%s%s %s \n"
             (tts-get-voice-command voice )
             text
             tts-voice-reset-code))))

;;;Internal function used by dtk-speak to send text out.
;;;Handles voice locking etc.
;;; assumes in dtk-scratch-buffer
;;;start and end give the extent of the
;;;text to be spoken.
;;; note that property auditory-icon at the start  of a clause
;;; causes the sound
;;; to be queued.

(defsubst tts-get-overlay-personality (position)
  "Return personality at the front of the overlay list at position."
  (car
   (remove nil
           (mapcar
            #'(lambda (o)
                (overlay-get o 'personality))
	    (overlays-at position)))))

(defsubst tts-get-overlay-auditory-icon (position)
  "Return auditory icon  at the front of the overlay list at position."
  (car
   (remove nil
           (mapcar
            #'(lambda (o)
                (overlay-get o 'auditory-icon))
	    (overlays-at position)))))

(defsubst dtk-format-text-and-speak (start end )
  "Format and speak text.
Arguments START and END specify region to speak."
  (declare (special voice-lock-mode dtk-speaker-process
                    emacspeak-use-auditory-icons))
  (when (and emacspeak-use-auditory-icons
             (get-text-property start 'auditory-icon))
    (emacspeak-queue-auditory-icon
     (get-text-property start 'auditory-icon)))
  (cond
   (voice-lock-mode
    (let ((last  nil)
          (personality (get-text-property start 'personality )))
      (while (and (< start end )
                  (setq last
                        (next-single-property-change  start 'personality
                                                      (current-buffer) end)))
        (if personality
            (dtk-speak-using-voice personality
                                   (buffer-substring start last ))
          (dtk-interp-queue (buffer-substring  start last)))
        (setq start  last
              personality
	      (get-text-property last  'personality))) ; end while
      ))                                ; end clause
   (t (dtk-interp-queue (buffer-substring start end  )))))

                                        ;Force the speech.
(defsubst dtk-force ()
  "Cause server to process all queued requests."
  (declare (special dtk-speaker-process))
  (dtk-interp-speak))

                                        ;Write out the string to the tts via TCLSH.
                                        ;No quoting is done, if want to quote the text, see dtk-speak
(defsubst dtk-dispatch (string)
  "Send request STRING to speech server."
  (declare (special dtk-speaker-process
                    dtk-speak-server-initialized
                    dtk-quiet))
  (unless dtk-quiet
    (when dtk-speak-server-initialized
      (dtk-interp-dispatch string ))))

(defsubst dtk-stop ()
  "Stop speech now."
  (interactive)
  (declare (special dtk-speaker-process))
  (dtk-interp-stop))

(defsubst dtk-reset-default-voice()
  (declare (special tts-default-voice ))
  (dtk-dispatch (tts-get-voice-command tts-default-voice )))

;;}}}
;;{{{  adding cleanup patterns:

(defun dtk-add-cleanup-pattern (&optional delete )
  "Add this pattern to the list of repeating patterns that
are cleaned up.  Optional interactive prefix arg deletes
this pattern if previously added.  Cleaning up repeated
patterns results in emacspeak speaking the pattern followed
by a repeat count instead of speaking all the characters
making up the pattern.  Thus, by adding the repeating
pattern `.' (this is already added by default) emacspeak
will say ``aw fifteen dot'' when speaking the string
``...............'' instead of ``period period period period
''"

  (interactive "P")
  (declare (special dtk-cleanup-patterns ))
  (cond
   (delete
    (setq dtk-cleanup-patterns
          (delete
           (read-from-minibuffer "Specify repeating pattern to delete: ")
           dtk-cleanup-patterns)))
   (t (setq dtk-cleanup-patterns
            (cons
             (read-from-minibuffer "Specify repeating pattern: ")
             dtk-cleanup-patterns )))))

;;}}}
;;{{{  producing output 

;;; Filter function to record last output from tcl

(defsubst dtk-filter-function (proc output)
  "Filter function for speech server.
Argument PROC is the server process.
Argument OUTPUT is the newly arrived output."
  (declare (special dtk-last-output))
  (setq dtk-last-output output))
;;; Uses the syntax table belonging to the buffer that owns the text
;;; to parse and speak the text intelligently.

(defvar dtk-speak-treat-embedded-punctuations-specially t
  "*If T then speech not split at embedded punctuations.")

(defvar dtk-speak-skim-scale 1.2
  "*Scale factor applied to speech rate when skimming.")

;;}}}
;;{{{  sending commands

(defun dtk-set-rate (rate    &optional prefix)
  "Set speaking RATE for the tts.
Interactive PREFIX arg means set   the global default value, and then set the
current local  value to the result."
  (interactive 
   (list
    (read-from-minibuffer "Enter new rate: ")
    current-prefix-arg))
  (declare (special dtk-speech-rate dtk-speaker-process
                    tts-default-speech-rate
                    dtk-program dtk-speak-server-initialized))
  (when dtk-speak-server-initialized
    (cond
     (prefix
      (setq tts-default-speech-rate rate)
      (setq-default dtk-speech-rate rate )
      (setq dtk-speech-rate rate))
     (t (setq dtk-speech-rate rate)))
    (dtk-interp-set-rate rate)
    (when prefix
      (tts-configure-synthesis-setup dtk-program))
    (when (interactive-p)
      (message "Set speech rate to %s %s"
               rate
               (if prefix "" "locally")))))

(defun dtk-set-predefined-speech-rate (&optional prefix)
  "Set speech rate to one of nine predefined levels.
Interactive PREFIX arg says to set the rate globally.
Formula used is:
rate = dtk-speech-rate-base + dtk-speech-rate-step * level."
  (interactive "P")
  (declare (special dtk-speech-rate-step
		    dtk-speech-rate-base
                    last-input-char))
  (let ((level
         (condition-case nil
             (read (format "%c" last-input-char ))
           (error nil ))))
    (or (numberp level)
        (setq level
              (read-minibuffer "Enter level between 1 and 9 to set
speech rate:")))
    (cond
     ((or (not (numberp level))
          (< level 0)
          (> level  9))
      (error "Invalid level %s" level ))
     (t (dtk-set-rate
         (+ dtk-speech-rate-base
            (* dtk-speech-rate-step  level ))
         prefix )
        (when (interactive-p)
          (message "Set speech rate to level %s %s"
		   level
		   (if prefix " globaly " " locally ")))))))

(defun dtk-set-character-scale (factor &optional prefix)
  "Set scale FACTOR for   speech rate.
Speech rate is scaled by this factor
when speaking characters.
Interactive PREFIX arg means set   the global default value, and then set the
current local  value to the result."
  (interactive "nEnter new factor:\nP")
  (declare (special dtk-character-scale dtk-speaker-process
                    dtk-speak-server-initialized))
  (when dtk-speak-server-initialized
    (cond
     (prefix
      (setq-default dtk-character-scale factor)
      (setq dtk-character-scale factor))
     (t (make-local-variable 'dtk-character-scale)
        (setq dtk-character-scale factor)))
    (dtk-interp-set-character-scale dtk-character-scale)
    (message "Set character scale factor to %s %s"
             dtk-character-scale
             (if  prefix ""  "locally"))))

(defun dtk-toggle-quiet (&optional prefix )
  "Toggle state of the speech device between being quiet and talkative.
Useful if you want to continue using an Emacs session that has
emacspeak loaded but wish to make the speech shut up.
Optional argument PREFIX specifies whether speech is turned off in the current buffer o rin all buffers."
  (interactive "P")
  (declare (special dtk-speaker-process dtk-quiet ))
  (and (not dtk-quiet)
       (message "Turning  off  speech synthesizer %s "
                (if prefix "" " locally")))
  (cond
   (prefix
    (setq-default  dtk-quiet
                   (not  (default-value 'dtk-quiet )))
    (setq dtk-quiet (default-value 'dtk-quiet )))
   (t (make-local-variable 'dtk-quiet)
      (setq dtk-quiet
            (not dtk-quiet ))))
  (and (not dtk-quiet)
       (message "Turned   on  speech synthesizer %s"
                (if prefix "" " locally"))))

(defun dtk-toggle-stop-immediately-while-typing  (&optional prefix)
  "Toggle state of variable `dtk-stop-immediately-while-typing'.
As the name implies, if T then speech flushes immediately as you
type.
Optional argument PREFIX specifies if the setting applies to all buffers."
  (interactive "P")
  (declare (special dtk-speaker-process dtk-stop-immediately-while-typing ))
  (cond
   (prefix
    (setq-default  dtk-stop-immediately-while-typing
                   (not  (default-value 'dtk-stop-immediately-while-typing )))
    (setq dtk-stop-immediately-while-typing (default-value 'dtk-stop-immediately-while-typing )))
   (t (make-local-variable 'dtk-stop-immediately-while-typing)
      (setq dtk-stop-immediately-while-typing
            (not dtk-stop-immediately-while-typing ))))
  (message "%s turned %s immediate flushing of speech when typing "
           (if prefix "" " locally")
           (if dtk-stop-immediately-while-typing "on" "off" )))

(defun dtk-toggle-split-caps (&optional prefix )
  "Toggle split caps mode.
Split caps mode is useful when reading
Hungarian notation in program source code.  Interactive PREFIX arg
means toggle the global default value, and then set the current local
value to the result."
  (interactive "P")
  (declare (special dtk-speaker-process dtk-split-caps
                    dtk-speak-server-initialized))
  (when dtk-speak-server-initialized
    (cond
     (prefix
      (setq-default  dtk-split-caps
                     (not  (default-value 'dtk-split-caps )))
      (setq dtk-split-caps (default-value 'dtk-split-caps )))
     (t (make-local-variable 'dtk-split-caps)
        (setq dtk-split-caps
              (not dtk-split-caps ))))
    (dtk-interp-toggle-split-caps dtk-split-caps )
    (message "Turned %s split caps mode%s "
             (if dtk-split-caps "on" "off" )
             (if prefix "" " locally"))))

(defun dtk-toggle-capitalization  (&optional prefix)
  "Toggle capitalization.
when set, capitalization is indicated by a
short beep.  Interactive PREFIX arg means toggle the global default
value, and then set the current local value to the result."
  (interactive "P")
  (declare (special dtk-speaker-process dtk-capitalize
                    dtk-speak-server-initialized))
  (when dtk-speak-server-initialized
    (cond
     (prefix
      (setq-default  dtk-capitalize
                     (not  (default-value 'dtk-capitalize )))
      (setq dtk-capitalize (default-value 'dtk-capitalize )))
     (t (make-local-variable 'dtk-capitalize)
        (setq dtk-capitalize
              (not dtk-capitalize ))))
    (dtk-interp-toggle-capitalization dtk-capitalize  )
    (message "Turned %s capitalization  mode%s "
             (if dtk-capitalize  "on" "off" )
             (if prefix "" " locally"))))
(defun dtk-toggle-speak-nonprinting-chars  (&optional prefix)
  "Toggle speak-nonprinting-chars.
Switches behavior of how characters with the high bit set are handled.
Interactive PREFIX arg means toggle the global default
value, and then set the current local value to the result."
  (interactive "P")
  (declare (special dtk-speaker-process dtk-speak-nonprinting-chars
                    dtk-speak-server-initialized))
  (when dtk-speak-server-initialized
    (cond
     (prefix
      (setq-default  dtk-speak-nonprinting-chars
                     (not  (default-value 'dtk-speak-nonprinting-chars )))
      (setq dtk-speak-nonprinting-chars (default-value 'dtk-speak-nonprinting-chars )))
     (t (make-local-variable 'dtk-speak-nonprinting-chars)
        (setq dtk-speak-nonprinting-chars
              (not dtk-speak-nonprinting-chars ))))
    (emacspeak-auditory-icon
     (if dtk-speak-nonprinting-chars
         'on
       'off))
    (message "Turned %s speak-nonprinting-chars  mode%s "
             (if dtk-speak-nonprinting-chars  "on" "off" )
             (if prefix "" " locally"))))

(defun dtk-toggle-allcaps-beep  (&optional prefix)
  "Toggle allcaps-beep.
when set, allcaps words  are  indicated by a
short beep.  Interactive PREFIX arg means toggle the global default
value, and then set the current local value to the result.
Note that allcaps-beep is a very useful thing when programming.
However it is irritating to have it on when reading documents."
  (interactive "P")
  (declare (special dtk-speaker-process dtk-allcaps-beep
                    dtk-speak-server-initialized))
  (when dtk-speak-server-initialized
    (cond
     (prefix
      (setq-default  dtk-allcaps-beep
                     (not  (default-value 'dtk-allcaps-beep )))
      (setq dtk-allcaps-beep (default-value 'dtk-allcaps-beep )))
     (t (make-local-variable 'dtk-allcaps-beep)
        (setq dtk-allcaps-beep
              (not dtk-allcaps-beep ))))
    (dtk-interp-toggle-allcaps-beep dtk-allcaps-beep  )
    (message "Turned %s allcaps-beep  mode%s "
             (if dtk-allcaps-beep  "on" "off" )
             (if prefix "" " locally"))))

(defun dtk-set-punctuations  (mode &optional prefix )
  "Set punctuation mode to MODE.
Possible values are `some', `all', or `none'.
Interactive PREFIX arg means set   the global default value, and then set the
current local  value to the result."
  (interactive
   (list
    (completing-read  "Enter punctuation mode: "
                      dtk-punctuation-mode-alist
                      nil
                      t)
    current-prefix-arg))
  (declare (special dtk-punctuation-mode dtk-speaker-process
                    dtk-speak-server-initialized
                    dtk-punctuation-mode-alist))
  (when dtk-speak-server-initialized
    (cond
     (prefix
      (setq dtk-punctuation-mode mode )
      (setq-default dtk-punctuation-mode mode))
     (t (make-local-variable 'dtk-punctuation-mode)
        (setq dtk-punctuation-mode mode )))
    (dtk-interp-set-punctuations mode)
    (when (interactive-p)
      (message "set punctuation mode to %s %s"
	       mode
	       (if prefix "" "locally")))))

(defun dtk-set-punctuations-to-all (&optional prefix )
  "Set punctuation  mode to all.
Interactive PREFIX arg sets punctuation mode globally."
  (interactive "P")
  (dtk-set-punctuations "all" prefix))

(defun dtk-set-punctuations-to-some (&optional prefix )
  "Set punctuation  mode to some.
Interactive PREFIX arg sets punctuation mode globally."
  (interactive "P")
  (dtk-set-punctuations "some" prefix))
  

(defun dtk-toggle-punctuation-mode (&optional prefix)
  "Toggle punctuation mode between \"some\" and \"all\".
Interactive PREFIX arg makes the new setting global."
  (interactive "P")
  (declare (special dtk-punctuation-mode))
  (cond
   ((string= "all" dtk-punctuation-mode)
    (dtk-set-punctuations-to-some prefix ))
   ((string= "some" dtk-punctuation-mode )
    (dtk-set-punctuations-to-all prefix )))
  (when (interactive-p)
    (message "set punctuation mode to %s %s"
             dtk-punctuation-mode
             (if prefix "" "locally"))))

(defun dtk-set-pronunciation-mode  (mode state  )
  "Set pronunciation MODE.
This command is valid only for newer
Dectalks, e.g.  the Dectalk Express.  Possible values are `math, name,
europe, spell', all of which can be turned on or off.
Argument STATE specifies new state."

  (interactive
   (list
    (completing-read  "Enter pronunciation  mode: "
                      dtk-pronunciation-mode-alist nil t)
    (y-or-n-p "Turn it on? ")))
  (declare (special dtk-pronunciation-mode-alist))
  (dtk-dispatch
   (format "[:mode %s %s]"
           mode
           (if state "on" "off"))))

(defun dtk-reset-state ()
  "Restore sanity to the Dectalk.
Typically used after the Dectalk has been power   cycled."
  (interactive)
  (declare (special  dtk-speaker-process
                     dtk-speak-server-initialized))
  (when dtk-speak-server-initialized
    (dtk-interp-reset-state)))

;;}}}
;;{{{  pause and resume
(defvar dtk-paused nil
  "Records if speech has been paused.")

(defun dtk-pause (&optional prefix)
  "Pause ongoing speech.
The speech can be resumed with command `dtk-resume'
normally bound to \\[dtk-resume].  Pausing speech is useful when one needs to
perform a few actions before continuing to read a large document.  Emacspeak
gives you speech feedback as usual once speech has been paused.  `dtk-resume'
continues the interrupted speech irrespective of the buffer
in which it is executed.
Optional PREFIX arg flushes any previously paused speech."
  (interactive "P")
  (declare (special dtk-paused))
  (cond
   ((not dtk-paused)
    (dtk-interp-pause)
    (setq dtk-paused t)
    (emacspeak-auditory-icon 'button))
   ((and (interactive-p)
         prefix
         dtk-paused)
    (dtk-interp-pause)
    (dtk-speak "Flushed previously paused speech ")
    (setq dtk-paused nil))
   ((and dtk-paused
         (interactive-p))
    (emacspeak-auditory-icon 'warn-user))))

(defcustom dtk-resume-should-toggle nil
  "*T means `dtk-resume' acts as a toggle."
  :type 'boolean
  :group 'tts)

(defun dtk-resume ()
  "Resume paused speech.
This command resumes  speech that has been suspended by executing
command `dtk-pause' bound to \\[dtk-pause].
If speech has not been paused,
and variable `dtk-resume-should-toggle' is t
 then this command will pause ongoing speech."
  (interactive)
  (declare (special dtk-speaker-process
                    dtk-resume-should-toggle
                    dtk-paused))
  (cond
   ((and dtk-resume-should-toggle
         (not dtk-paused))
    (dtk-pause))
   (t (dtk-interp-resume)
      (emacspeak-auditory-icon 'button)
      (setq dtk-paused nil))))

;;}}}
(provide 'dtk-tcl)
;;{{{  local variables

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}

