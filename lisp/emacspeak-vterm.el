;;; emacspeak-vterm.el --- Speech-enable VTERM  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable VTERM An Emacs Interface to vterm
;;; Keywords: Emacspeak,  Audio Desktop vterm
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
;;; MERCHANTABILITY or FITNVTERM FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; VTERM == vterm using native vterm library

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Map Faces:

(voice-setup-add-map
 '(
   (vterm-color-black voice-bolden)
   (vterm-color-blue voice-brighten)
   (vterm-color-cyan voice-smoothen)
   (vterm-color-default 'paul)
   (vterm-color-green voice-lighten)
   (vterm-color-inverse-video 'betty)
   (vterm-color-magenta voice-annotate)
   (vterm-color-underline voice-monotone)
   (vterm-color-white 'paul)
   (vterm-color-yellow voice-animate)))

;;}}}
;;{{{ Interactive Commands:

'(

  vterm-clear-scrollback
  vterm-copy-mode
  vterm-copy-mode-done
  vterm-mode
  vterm-module-compile
  vterm-other-window
  vterm-reset-cursor-point
  vterm-send-C-a
  vterm-send-C-b
  vterm-send-C-c
  vterm-send-C-d
  vterm-send-C-e
  vterm-send-C-f
  vterm-send-C-g
  vterm-send-C-h
  vterm-send-C-i
  vterm-send-C-j
  vterm-send-C-k
  vterm-send-C-l
  vterm-send-C-m
  vterm-send-C-n
  vterm-send-C-o
  vterm-send-C-p
  vterm-send-C-q
  vterm-send-C-r
  vterm-send-C-s
  vterm-send-C-t
  vterm-send-C-u
  vterm-send-C-v
  vterm-send-C-w
  vterm-send-C-x
  vterm-send-C-y
  vterm-send-C-z
  vterm-send-M-a
  vterm-send-M-b
  vterm-send-M-c
  vterm-send-M-d
  vterm-send-M-e
  vterm-send-M-f
  vterm-send-M-g
  vterm-send-M-h
  vterm-send-M-i
  vterm-send-M-j
  vterm-send-M-k
  vterm-send-M-l
  vterm-send-M-m
  vterm-send-M-n
  vterm-send-M-o
  vterm-send-M-p
  vterm-send-M-q
  vterm-send-M-r
  vterm-send-M-s
  vterm-send-M-t
  vterm-send-M-u
  vterm-send-M-v
  vterm-send-M-w
  vterm-send-M-x
  vterm-send-M-y
  vterm-send-M-z
  vterm-send-backspace
  vterm-send-ctrl-slash
  vterm-send-delete
  vterm-send-down
  vterm-send-escape
  vterm-send-left
  vterm-send-meta-backspace
  vterm-send-meta-comma
  vterm-send-meta-dot
  vterm-send-next
  vterm-send-prior
  vterm-send-return
  vterm-send-right
  vterm-send-space
  vterm-send-start
  vterm-send-stop
  vterm-send-tab
  vterm-send-up
  vterm-undo
  vterm-yank
  vterm-yank-pop
  vterm-yank-primary
  )

(defadvice vterm-clear (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (message "Cleared screen")))

(defadvice vterm-clear-scrollback (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (message "Cleared scrollback")))

(defadvice vterm-copy-mode-done (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-line)))

(with-eval-after-load "vterm"
  (cl-declaim (special vterm-mode-map))
  (define-key vterm-mode-map (ems-kbd "C-e") 'emacspeak-prefix-command)
  )
(defadvice vterm (after emacspeak pre act comp)
  "Provide auditory feedback."

  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(cl-loop
 for f in
 '(vterm-end-of-line vterm-beginning-of-line)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'large-movement)
       (emacspeak-speak-line)))))

(defadvice vterm-reset-cursor-point (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))


(cl-loop
 for f in
 '(vterm-previous-prompt vterm-next-prompt)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'large-movement)
       (emacspeak-speak-line)))))

;;}}}
;;{{{Handle output and terminal updates:
;;; This sends what you typed to the term process.  Handle terminal
;;; emulation logic here, as per term-emulate-term in emacspeak-eterm.
;;; Simpler because for now, we dont implement sub-windows etc.
;;; ad-do-it doesn't update for native module functions?
;;; tried with before/after advice pair, and we still dont see the
;;;updates, so  using before advice to record state,
;;; and an after advice on vterm--redraw to implement the spoken
;;;feedback loop.

(defvar ems--vterm-row nil
  "Cache row.")

(defvar ems--vterm-column nil
  "Cache vterm column.")

(defvar ems--vterm-char nil
  "Cache current char.")

(defadvice vterm--flush-output (before emacspeak pre act comp)
  "Cache state before input event is processed."
  (setq ems--vterm-row(1+ (count-lines (point-min) (point))) ;;; line number
        ems--vterm-column (current-column) ;;; column number
        ems--vterm-char (preceding-char)))

;;; speech-enable term update loop, using previously cached state.

(defadvice vterm--redraw (after emacspeak pre act comp)
  "Speech-enable term emulation."
  (let ((current-char ems--vterm-char)
        (row ems--vterm-row)
        (column ems--vterm-column)
        (new-row (1+ (count-lines (point-min) (point))))
        (new-column (current-column)))
    (ems-with-messages-silenced
        (message
         "Event: %c row: %d col: %d new-row: %d new-col: %d char: %c"
         last-command-event row column
         new-row new-column ems--vterm-char))
    (cond
     ((and ;;; backspace or 127
       (memq  last-command-event    '(127 backspace))
       (= new-row row) (= -1 (- new-column column)) ;;; backspace
       current-char)
      (dtk-tone-deletion)
      (emacspeak-speak-this-char current-char))
     ((and
       (= new-row row) (= 1 (- new-column column)));;; char insert
      (ems-with-messages-silenced (message "char insert"))
      (if (eq 32 last-command-event) ;;; word echo 
          (save-excursion (backward-char 2) (emacspeak-speak-word nil))
        (emacspeak-speak-this-char (preceding-char))))
     ((and
       (= new-row row) (= 1 (abs(- new-column column))))
      (ems-with-messages-silenced (message "char motion"))
      (emacspeak-speak-this-char (preceding-char)))
     ((= row new-row)
      (ems-with-messages-silenced (message "left/right motion"))
      (if (= 32 (following-char))
          (save-excursion ;;; speak word in vi word navigation
            (forward-char 1)
            (emacspeak-speak-word))
        (emacspeak-speak-word)))
     (t (emacspeak-speak-line)))))


;;}}}
(provide 'emacspeak-vterm)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
