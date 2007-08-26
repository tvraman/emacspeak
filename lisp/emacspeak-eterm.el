;;; emacspeak-eterm.el --- Speech enable eterm -- Emacs' terminal emulator  term.el
;;; $Id$
;;; $Author$ 
;;; Description:  Emacspeak extension to speech enable eterm. 
;;; Keywords: Emacspeak, Eterm, Terminal emulation, Spoken Output
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu 
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision: 4532 $ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2007, T. V. Raman 
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
;;{{{ required packages:

;;; Commentary:
;;{{{  Introduction:
;;; This module makes eterm talk.
;;; Eterm is the new terminal emulator for Emacs.
;;; Use of emacspeak with eterm really needs an info page.
;;; At present, the only documentation is the source level documentation.
;;; This module uses Control-t as an additional prefix key to allow the user
;;; To move around the terminal and have different parts spoken. 

;;}}}
;; 
;;; Code:

(require 'emacspeak-preamble)
(require 'term)
;;}}}
;;{{{ custom
;;;###autoload
(defgroup emacspeak-eterm nil
  "Terminal emulator for the Emacspeak Desktop."
  :group 'emacspeak
  :prefix "emacspeak-eterm-")

;;}}}
;;{{{  keybindings:

(defvar emacspeak-eterm-keymap (make-keymap)
  "Keymap used to navigate a terminal without moving the cursor.")
(defvar emacspeak-eterm-prefix "\C-t"
  "Prefix char used by emacspeak for navigating an eterm.")

(defun emacspeak-eterm-setup-keys()
  "Make eterm usable with emacspeak"
  (declare (special emacspeak-prefix emacspeak-eterm-prefix
                    emacspeak-eterm-keymap  term-mode-map ))
  (define-prefix-command 'emacspeak-eterm-prefix-command
    'emacspeak-eterm-keymap)
  (define-key term-mode-map emacspeak-eterm-prefix
    'emacspeak-eterm-prefix-command)
  (suppress-keymap emacspeak-eterm-keymap)
  (let  ((i 0))
    (while (< i 10)
      (define-key emacspeak-eterm-keymap
        (format "%s" i) 'emacspeak-eterm-speak-predefined-window )
      (incf i )))
  (define-key emacspeak-eterm-keymap "\C-i" 'emacspeak-eterm-speak-cursor )
  (define-key emacspeak-eterm-keymap "\C-q" 'emacspeak-toggle-eterm-autospeak)
  (define-key emacspeak-eterm-keymap " "  'emacspeak-eterm-speak-screen)
  (define-key emacspeak-eterm-keymap '[up] 'emacspeak-eterm-pointer-up )
  (define-key emacspeak-eterm-keymap '[down] 'emacspeak-eterm-pointer-down )
  (define-key emacspeak-eterm-keymap '[left] 'emacspeak-eterm-pointer-left )
  (define-key emacspeak-eterm-keymap '[right] 'emacspeak-eterm-pointer-right )
  (define-key emacspeak-eterm-keymap "a" 'emacspeak-eterm-pointer-to-left-edge)
  (define-key emacspeak-eterm-keymap "e" 'emacspeak-eterm-pointer-to-right-edge)
  (define-key emacspeak-eterm-keymap "\M-b" 'emacspeak-eterm-pointer-backward-word)
  (define-key emacspeak-eterm-keymap "\M-f"  'emacspeak-eterm-pointer-forward-word)
  (define-key emacspeak-eterm-keymap "." 'emacspeak-eterm-pointer-to-cursor )
  (define-key emacspeak-eterm-keymap "," 'emacspeak-eterm-speak-pointer)
  (define-key emacspeak-eterm-keymap "c" 'emacspeak-eterm-speak-pointer-char)
  (define-key emacspeak-eterm-keymap "w" 'emacspeak-eterm-speak-pointer-word)
  (define-key emacspeak-eterm-keymap "l" 'emacspeak-eterm-speak-pointer-line)
  (define-key emacspeak-eterm-keymap "p"  'emacspeak-eterm-pointer-up )
  (define-key emacspeak-eterm-keymap "n" 'emacspeak-eterm-pointer-down )
  (define-key emacspeak-eterm-keymap "b"  'emacspeak-eterm-pointer-left )
  (define-key emacspeak-eterm-keymap "f" 'emacspeak-eterm-pointer-right )
  (define-key emacspeak-eterm-keymap "h"
    'emacspeak-eterm-pointer-to-next-color-change)
  (define-key emacspeak-eterm-keymap "H"
    'emacspeak-eterm-pointer-to-previous-color-change)
  (define-key emacspeak-eterm-keymap "t" 'emacspeak-eterm-pointer-to-top)
  (define-key emacspeak-eterm-keymap "<" 'emacspeak-eterm-pointer-to-top)
  (define-key emacspeak-eterm-keymap ">" 'emacspeak-eterm-pointer-to-bottom)
  (define-key emacspeak-eterm-keymap "g" 'emacspeak-eterm-goto-line)
  (define-key emacspeak-eterm-keymap "s" 'emacspeak-eterm-search-forward )
                                        ;(define-key emacspeak-eterm-keymap "r" 'emacspeak-eterm-search-backward)
  (define-key emacspeak-eterm-keymap "y"
    'emacspeak-eterm-kill-ring-save-region)
  (define-key emacspeak-eterm-keymap "x"
    'emacspeak-eterm-copy-region-to-register)
  (define-key emacspeak-eterm-keymap "v" 'emacspeak-eterm-paste-register)
  (define-key emacspeak-eterm-keymap "m" 'emacspeak-eterm-set-marker )
  (define-key emacspeak-eterm-keymap "\C-p"
    'emacspeak-eterm-toggle-pointer-mode)
  (define-key emacspeak-eterm-keymap "\C-w" 'emacspeak-eterm-define-window)
  (define-key emacspeak-eterm-keymap "\C-y"
    'emacspeak-eterm-yank-window)
  (define-key emacspeak-eterm-keymap "f"
    'emacspeak-eterm-set-filter-window)
  (define-key emacspeak-eterm-keymap "\C-f"
    'emacspeak-eterm-set-focus-window)
  (define-key emacspeak-eterm-keymap "A" 'emacspeak-eterm-toggle-filter-window)
  (define-key emacspeak-eterm-keymap "\C-a" 'emacspeak-eterm-toggle-focus-window)
  (define-key emacspeak-eterm-keymap "\C-d" 'emacspeak-eterm-describe-window)
  (define-key emacspeak-eterm-keymap "\C-m" 'emacspeak-eterm-speak-window)
  (define-key emacspeak-eterm-keymap "r" 'emacspeak-eterm-toggle-review)
  (define-key emacspeak-eterm-keymap "q" 'emacspeak-eterm-toggle-review)
  (and term-raw-escape-map
       (mapcar
        (function 
         (lambda (key)
           (define-key term-raw-escape-map key 
             (lookup-key (current-global-map) key))))
        '("\M-x" "\C-h")))
  t)

(defvar emacspeak-eterm-raw-prefix
  "\C-r"
  "Prefix key to use  to send out raw term input. 
Useful when eterm is in review mode.")

(defun emacspeak-eterm-setup-raw-keys ()
  "Setup emacspeak keys for raw terminal mode."
  (declare (special term-raw-map
                    emacspeak-prefix term-raw-escape-map
                    emacspeak-eterm-keymap
                    emacspeak-eterm-raw-prefix))
  (when term-raw-map 
    (define-key term-raw-map emacspeak-prefix 'emacspeak-prefix-command)
    (define-key term-raw-map (concat emacspeak-prefix emacspeak-prefix)
      'emacspeak-eterm-maybe-send-raw)
    (define-key term-raw-map emacspeak-eterm-prefix
      'emacspeak-eterm-prefix-command)
    (define-key term-raw-map emacspeak-eterm-raw-prefix term-raw-map)
    (define-key term-raw-map
      (concat emacspeak-eterm-raw-prefix emacspeak-eterm-raw-prefix)
      'emacspeak-eterm-maybe-send-raw)
    (define-key term-raw-map
      (concat emacspeak-eterm-prefix emacspeak-eterm-prefix) 'emacspeak-eterm-maybe-send-raw)
    (define-key emacspeak-eterm-keymap emacspeak-eterm-raw-prefix
      term-raw-map)
;;; handle emacs 21 wierdness 
    (local-unset-key "\eO")
    (local-unset-key "\e[")
    ))

;;}}}
;;{{{  voice definitions  for eterm  highlight, underline etc

(defcustom emacspeak-eterm-highlight-personality voice-bolden
  "Personality to show terminal highlighting."
  :type 'symbol
  :group 'emacspeak-eterm)

(defcustom emacspeak-eterm-bold-personality voice-bolden
  "Personality to indicate terminal bold."
  :type 'symbol
  :group 'emacspeak-eterm)

(defcustom emacspeak-eterm-underline-personality 'ursula
  "Personality to indicate terminal underlining."
  :group 'emacspeak-eterm
  :type 'symbol)

(defcustom emacspeak-eterm-default-personality 'paul
  "Default personality for terminal."
  :type 'symbol
  :group 'emacspeak-eterm)

;;}}}
;;{{{  functions

;;; nuke term cache info 
(defsubst emacspeak-eterm-nuke-cached-info ()
  (declare (special term-current-row term-current-column ))
  (setq term-current-row nil
        term-current-column nil ))

;;;Send the last input character as a  raw key,
;;; ie without any interpretation.
;;; Ensure you're in a terminal before sending it through."
(defun emacspeak-eterm-maybe-send-raw ()
  "Send a raw character through if in the terminal buffer.
Execute end of line if
in a non eterm buffer if executed via C-e C-e"
  (interactive)
  (declare (special last-input-event))
  (cond
   ((or (eq major-mode 'term-mode)
        (eq major-mode 'tshell-mode))
    (term-send-raw ))
   ((= last-input-event 5) (end-of-line ))
   (t (beep))))

(defun emacspeak-eterm-speak-cursor ()
  "Speak cursor position."
  (interactive)
  (message
   "Cursor at Row %s Column %s"
   (term-current-row)
   (term-current-column)))

(defun emacspeak-eterm-speak-pointer ()
  "Speak current pointer position."
  (interactive)
  (declare (special emacspeak-eterm-pointer  ))
  (let ((coordinates (emacspeak-eterm-position-to-coordinates
                      (marker-position emacspeak-eterm-pointer ))))
    (message 
     "Pointer at row %s column %s "
     (cdr coordinates ) (car coordinates ))))

(defun emacspeak-eterm-speak-screen (&optional flag )
  "Speak the screen.  Default is to speak from the emacspeak pointer  to point.
Optional prefix arg FLAG causes region above
the Emacspeak pointer to be spoken."
  (interactive "P")
  (declare (special term-home-marker emacspeak-eterm-pointer ))
  (if flag
      (emacspeak-speak-region term-home-marker  emacspeak-eterm-pointer)
    (emacspeak-speak-region  emacspeak-eterm-pointer (point-max ))))

;;}}}
;;{{{  Speaking the screen pointer:

;;; The pointer is an invisible marker that is
;;; moved around to speak the screen.
;;; The pointer is emacspeak-eterm-pointer and starts off at the cursor.
;;; Speaking relative to the pointer:

(defun emacspeak-eterm-speak-pointer-line ()
  "Speak the line the pointer is on."
  (interactive)
  (declare (special emacspeak-eterm-pointer ))
  (save-excursion
    (goto-char emacspeak-eterm-pointer)
    (emacspeak-speak-line nil )))

(defun emacspeak-eterm-speak-pointer-word ()
  "Speak the word  the pointer is on."
  (interactive)
  (declare (special emacspeak-eterm-pointer ))
  (save-excursion
    (goto-char emacspeak-eterm-pointer)
    (emacspeak-speak-word nil )))

(defun emacspeak-eterm-speak-pointer-char (&optional prefix)
  "Speak char under eterm pointer.
Pronounces character phonetically unless  called with a PREFIX arg."
  (interactive "P")
  (declare (special emacspeak-eterm-pointer ))
  (save-excursion
    (goto-char emacspeak-eterm-pointer)
    (emacspeak-speak-char prefix)))

;;}}}
;;{{{  moving the screen pointer: 

(defun emacspeak-eterm-pointer-to-cursor ()
  "Move the pointer to the cursor."
  (interactive)
  (declare (special emacspeak-eterm-pointer ))
  (set-marker emacspeak-eterm-pointer (point ))
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-eterm-speak-cursor)))

(defun emacspeak-eterm-pointer-to-top () 
  "Move the pointer to the top of the screen."
  (interactive)
  (declare (special term-home-marker emacspeak-eterm-pointer ))
  (save-excursion
    (goto-char term-home-marker)  
    (set-marker emacspeak-eterm-pointer (point))
    (when (interactive-p)
      (emacspeak-auditory-icon 'large-movement)
      (emacspeak-speak-line nil ))))

(defun emacspeak-eterm-pointer-to-bottom  () 
  "Move the pointer to the bottom  of the screen."
  (interactive)
  (declare (special  emacspeak-eterm-pointer ))
  (save-excursion
    (goto-char (point-max))
    (set-marker emacspeak-eterm-pointer (point ))
    (when (interactive-p)
      (emacspeak-auditory-icon 'large-movement)
      (emacspeak-speak-line nil ))))

(defun emacspeak-eterm-pointer-up (count)
  "Move the pointer up a line.
Argument COUNT .specifies number of lines by which to move."
  (interactive "P")
  (declare (special emacspeak-eterm-pointer 
                    term-home-marker))
  (setq count (or count 1 ))
  (save-excursion
    (goto-char emacspeak-eterm-pointer )
    (forward-line (- count ))
    (beginning-of-line )
    (cond 
     ((<= (marker-position term-home-marker ) (point))
      (set-marker emacspeak-eterm-pointer (point ))
      (emacspeak-speak-line nil ))
     (t (error "At top of screen. "  )))))

(defun emacspeak-eterm-pointer-down (count )
  "Move the pointer down a line.
Argument COUNT specifies number of lines by which to move."
  (interactive "P")
  (declare (special emacspeak-eterm-pointer))
  (setq count (or count 1 ))
  (save-excursion
    (goto-char emacspeak-eterm-pointer )
    (forward-line count)
    (beginning-of-line )
    (cond 
     ((<= (point) (point-max)) 
      (set-marker emacspeak-eterm-pointer (point ))
      (emacspeak-speak-line nil ))
     (t (error "Not that many lines on the screen" )))))

(defun emacspeak-eterm-pointer-left (count)
  "Move the pointer left.
Argument COUNT specifies number of columns by which to move."
  (interactive "P")
  (declare (special emacspeak-eterm-pointer ))
  (setq count (or count 1 ))
  (save-excursion
    (goto-char emacspeak-eterm-pointer )
    (backward-char count)
    (set-marker emacspeak-eterm-pointer (point ))
    (when (interactive-p)
      (dtk-stop)
      (emacspeak-speak-char t ))))

(defun emacspeak-eterm-pointer-right (count)
  "Move the pointer right.
Argument COUNT specifies number of columns by which to move."
  (interactive "P")
  (declare (special emacspeak-eterm-pointer ))
  (setq count (or count 1 ))
  (save-excursion
    (goto-char emacspeak-eterm-pointer )
    (forward-char  count )
    (set-marker emacspeak-eterm-pointer (point ))
    (when (interactive-p)
      (dtk-stop)
      (emacspeak-speak-char t))))

(defun emacspeak-eterm-pointer-to-right-edge ()
  "Move the pointer to the right edge."
  (interactive)
  (declare (special emacspeak-eterm-pointer ))
  (save-excursion
    (goto-char emacspeak-eterm-pointer )
    (end-of-line)
    (set-marker emacspeak-eterm-pointer (point ))
    (when (interactive-p) 
      (dtk-stop)
      (emacspeak-auditory-icon 'large-movement)
      (emacspeak-speak-char t))))

(defun emacspeak-eterm-pointer-to-left-edge ()
  "Move the pointer to the right edge."
  (interactive)
  (declare (special emacspeak-eterm-pointer ))
  (save-excursion
    (goto-char emacspeak-eterm-pointer )
    (beginning-of-line)
    (set-marker emacspeak-eterm-pointer (point ))
    (when (interactive-p)
      (dtk-stop)
      (emacspeak-auditory-icon 'large-movement)
      (emacspeak-speak-char t))))

(defun emacspeak-eterm-pointer-backward-word (count)
  "Move the pointer backward  by words. 
Interactive numeric prefix arg specifies number of words to move.
Argument COUNT specifies number of words by which to move."
  (interactive "P")
  (declare (special emacspeak-eterm-pointer ))
  (setq count (or count 1 ))
  (save-excursion
    (goto-char emacspeak-eterm-pointer )
    (condition-case nil 
        (forward-word  (- count ))
      (error nil ))
    (set-marker emacspeak-eterm-pointer (point ))
    (when (interactive-p)
      (emacspeak-speak-word))))

(defun emacspeak-eterm-pointer-forward-word (count)
  "Move the pointer forward by words. 
Interactive numeric prefix arg specifies number of words to move.
Argument COUNT specifies number of words by which to move."
  (interactive "P")
  (declare (special emacspeak-eterm-pointer ))
  (setq count (or count 1 ))
  (save-excursion
    (goto-char emacspeak-eterm-pointer )
    (condition-case nil 
        (forward-word  count )
      (error nil ))
    (skip-syntax-forward " ")
    (set-marker emacspeak-eterm-pointer (point ))
    (when (interactive-p)
      (emacspeak-speak-word))))

(defun emacspeak-eterm-goto-line (line)
  "Move emacspeak eterm pointer to a specified LINE."
  (interactive "nGo to line:")
  (declare (special emacspeak-eterm-pointer
                    term-home-marker))
  (save-excursion 
    (goto-char term-home-marker )
    (forward-line line)
    (set-marker emacspeak-eterm-pointer (point ))
    (emacspeak-auditory-icon 'large-movement )
    (emacspeak-speak-line)))

(defun emacspeak-eterm-search-forward ()
  "Search forward on the terminal."
  (interactive)
  (emacspeak-eterm-search 1))

(defun emacspeak-eterm-search-backward ()
  "Search backward on the terminal."
  (interactive)
  (emacspeak-eterm-search -1))

;;; Helper function for searching:

(defun emacspeak-eterm-search(direction)
  "Prompt for a string,
and try and locate it on the terminal.
If found, the Emacspeak pointer is left at the hit. "
  (declare (special emacspeak-eterm-pointer
                    term-home-marker ))
  (let ((found nil)
        (start nil)
        (end nil)
        (string (read-from-minibuffer "Enter search string: ")))
    (if (= 1 direction)                 ; forward search
        (setq start  (marker-position emacspeak-eterm-pointer )
              end (point-max))
                                        ; backward search 
      (setq start (marker-position emacspeak-eterm-pointer )
            end (marker-position term-home-marker )))
    (save-excursion
      (goto-char start)
      (save-restriction
        (narrow-to-region start end )
        (save-match-data 
          (if (= 1 direction)           ;forward search 
              (setq found (search-forward  string  end t))
            (setq found (search-backward   string end t))))
        (cond
         (found (set-marker emacspeak-eterm-pointer (point-marker))
                (emacspeak-auditory-icon 'search-hit)
                (emacspeak-eterm-speak-pointer-line))
         (t(emacspeak-auditory-icon 'search-miss)
           (message "%s not found " string )))))))

;;}}}
;;{{{  Highlight tracking:

;;; Moving pointer  to the next highlighted portion of the screen:

(defun emacspeak-eterm-pointer-to-next-color-change  (&optional count)
  "Move the eterm pointer to the next color change.
This allows you to move between highlighted regions of the screen.
Optional argument COUNT specifies how many changes to skip."
  (interactive "p")
  (declare (special emacspeak-eterm-pointer))
  (setq count (or count 1 ))
  (let ((current (get-text-property emacspeak-eterm-pointer 'personality ))
        (found nil))
    (save-excursion
      (goto-char emacspeak-eterm-pointer)
      (setq found (text-property-not-all (point) (point-max)
                                         'personality current ))
      (cond
       (found (set-marker emacspeak-eterm-pointer found )
              (emacspeak-auditory-icon 'large-movement)
              (emacspeak-eterm-speak-pointer-line))
       (t (message "No color change found on the screen "))))))

(defun emacspeak-eterm-pointer-to-previous-color-change  (&optional count)
  "Move the eterm pointer to the next color change.
This allows you to move between highlighted regions of the screen.
Optional argument COUNT specifies how many changes to skip."
  (interactive "p")
  (declare (special emacspeak-eterm-pointer term-home-marker))
  (setq count (or count 1 ))
  (let ((current (get-text-property emacspeak-eterm-pointer 'personality ))
        (found nil))
    (save-excursion
      (goto-char emacspeak-eterm-pointer)
      (setq found (text-property-not-all (point)  term-home-marker
                                         'personality current ))
      (cond
       (found (set-marker emacspeak-eterm-pointer found )
              (emacspeak-auditory-icon 'large-movement)
              (emacspeak-eterm-speak-pointer-line))
       (t (message "No color change found on the screen "))))))

;;}}}
;;{{{  reviewing the terminal:

(defvar emacspeak-eterm-review-p nil 
  "T if eterm is in review mode. 
In review mode, you can move around the terminal and listen to parts of it.
Do not set this variable by hand.
Use command \\[emacspeak-eterm-toggle-review].")

(defun emacspeak-eterm-toggle-review ()
  "Toggle state of eterm review. 
In review mode, you can move around the terminal and listen to the contnets 
without sending input to the terminal itself."
  (interactive)
  (declare (special emacspeak-eterm-review-p 
                    eterm-char-mode
                    buffer-read-only emacspeak-eterm-keymap term-raw-map))
  (emacspeak-eterm-nuke-cached-info )
  (setq mode-line-process
        '("review"))
  (if eterm-char-mode 
      (cond 
       (emacspeak-eterm-review-p        ;turn it off 
        (message "Returning to terminal character mode ")
        (setq emacspeak-eterm-review-p nil)
        (use-local-map term-raw-map))
       (t                               ; turn it on
        (message "Entering terminal review mode press  q  to return to normal")
        (setq emacspeak-eterm-review-p t)
        (use-local-map emacspeak-eterm-keymap)))
    (message
     "Terminal review should be used when eterm is in character mode "))
  (emacspeak-auditory-icon
   (if emacspeak-eterm-review-p 'on 'off)))

;;}}}
;;{{{  Cut and paste while reviewing:

(defvar emacspeak-eterm-marker nil 
  "Marker used by emacspeak to yank when in eterm review mode.")

(defun emacspeak-eterm-set-marker ()
  "Set Emacspeak eterm marker.
This sets  the emacspeak eterm marker to the position pointed 
to by the emacspeak eterm pointer."
  (interactive)
  (declare (special emacspeak-eterm-pointer 
                    emacspeak-eterm-marker))
  (let ((coordinates nil))
    (set-marker emacspeak-eterm-marker 
                (marker-position emacspeak-eterm-pointer ))
    (setq coordinates
          (emacspeak-eterm-position-to-coordinates
           (marker-position emacspeak-eterm-pointer)))
    (when (interactive-p)
      (emacspeak-auditory-icon 'mark-object)
      (dtk-stop)
      (message "Set eterm mark at row %s column %s"
               (cdr coordinates)
               (car coordinates)))))

(defun emacspeak-eterm-kill-ring-save-region  ()
  "Copy text from terminal to kill ring.
This copies  region delimited by the emacspeak eterm marker 
set by command \\[emacspeak-eterm-set-marker] and the 
emacspeak eterm pointer."
  (interactive)
  (declare (special emacspeak-eterm-marker 
                    emacspeak-eterm-pointer ))
  (kill-ring-save (marker-position emacspeak-eterm-marker )
                  (marker-position emacspeak-eterm-pointer))
  (emacspeak-auditory-icon 'mark-object)
  (message "Snarfed %s characters "
           (abs (- (marker-position emacspeak-eterm-marker )
                   (marker-position emacspeak-eterm-pointer)))))

(defun emacspeak-eterm-copy-region-to-register  (register)
  "Copy text from terminal to an Emacs REGISTER.
This copies  region delimited by the emacspeak eterm marker 
set by command \\[emacspeak-eterm-set-marker] and the 
emacspeak eterm pointer to a register."
  (interactive "cCopy to register:")
  (declare (special emacspeak-eterm-marker 
                    emacspeak-eterm-pointer ))
  (copy-to-register register 
                    (marker-position emacspeak-eterm-marker)
                    (marker-position emacspeak-eterm-pointer)
                    nil)
  (emacspeak-auditory-icon 'mark-object)
  (message "Snarfed %s characters to register %c "
           (abs (- (marker-position emacspeak-eterm-marker )
                   (marker-position emacspeak-eterm-pointer)))
           register))

(defun emacspeak-eterm-paste-register (register)
  "Paste contents of REGISTER at current location.
If the specified register contains text, then that text is
sent to the terminal as if it were typed by the user."
  (interactive "*cRegister to paste: ")
  (let ((contents (get-register register)))
    (cond
     ((stringp contents)
      (term-send-raw-string contents)
      (emacspeak-auditory-icon 'yank-object))
     (t (error "Register %c does not contain text"
               register)))))

;;}}}
;;{{{  Defining and speaking terminal windows:

;;; A window structure is of the form
;;; [column row right-stretch left-stretch ]

(defsubst emacspeak-eterm-make-window (top-left bottom-right right-stretch left-stretch )
  (let ((win (make-vector 4  nil)))
    (aset win 0 top-left )
    (aset win 1 bottom-right )
    (aset win 2 right-stretch )
    (aset win 3 left-stretch )
    win))

  
(defsubst emacspeak-eterm-window-top-left (w) (aref w 0 ))
(defsubst emacspeak-eterm-window-bottom-right (w) (aref w 1 ))
(defsubst emacspeak-eterm-window-right-stretch (w) (aref w 2 ))
(defsubst emacspeak-eterm-window-left-stretch  (w) (aref w 3 ))

(defun  emacspeak-eterm-coordinate-within-window-p (coordinate id  )
  "Predicate to test if COORDINATE is within window.
Argument ID specifies the window."
  (when (and coordinate id)
    (let*  ((window  (emacspeak-eterm-get-window id ))
            (row (cdr coordinate))
            (column (car coordinate ))
            (left-stretch (emacspeak-eterm-window-left-stretch window ))
            (right-stretch (emacspeak-eterm-window-right-stretch window ))
            (top-left-row (cdr
                           (emacspeak-eterm-window-top-left window )))
            (top-left-column (car
                              (emacspeak-eterm-window-top-left window)))
            (bottom-right-row (cdr 
                               (emacspeak-eterm-window-bottom-right window )))
            (bottom-right-column (car
                                  (emacspeak-eterm-window-bottom-right window  ))))
      (not
       (or  (< row top-left-row )
            (> row bottom-right-row )
            (and (not left-stretch ) (< column top-left-column ))
            (and (not right-stretch ) (> column bottom-right-column )))))))
 
;;; Translate a screen position to a buffer position

(defun emacspeak-eterm-coordinates-to-position (coordinates)
  "Translate screen COORDINATES to buffer position.
This translate  screen coordinates specified
as a cons cell (column .  row ) to a buffer position in the eterm buffer"
  (declare (special term-home-marker))
  (let ((column (car coordinates ))
        (row (cdr coordinates )))
    (save-excursion
      (save-restriction
        (emacspeak-eterm-nuke-cached-info)
        (narrow-to-region term-home-marker (point-max ))
        (term-goto row column )
        (emacspeak-eterm-nuke-cached-info)
        (point )))))

;;; Translate buffer position to screen coordinates.
;;; returns a cons cell (column . row )
(defun emacspeak-eterm-position-to-coordinates (position)
  "Translate a buffer POSITION in the eterm buffer to screen coordinates."
  (declare (special term-home-marker))
  (save-excursion
    (save-restriction
      (narrow-to-region term-home-marker (point-max ))
      (goto-char position)
      (emacspeak-eterm-nuke-cached-info)
      (let ((coordinates (cons
                          (term-current-column)
                          (term-current-row ))))
        (emacspeak-eterm-nuke-cached-info)
        coordinates ))))

;;; return contents of a term window 
(defun emacspeak-eterm-return-window-contents (eterm-window)
  "Return  the contents of a window as a string.
Argument ETERM-WINDOW specifies a predefined eterm window."
  (declare (special term-home-marker))
  (let ((start nil)
        (end nil)
        (right-stretch (emacspeak-eterm-window-right-stretch eterm-window))
        (left-stretch (emacspeak-eterm-window-left-stretch eterm-window))
        (contents nil)
        (top-left
         (emacspeak-eterm-window-top-left eterm-window))
        (bottom-right
         (emacspeak-eterm-window-bottom-right eterm-window)))
    (save-excursion
      (save-restriction
        (narrow-to-region term-home-marker (point-max))
        (setq start (emacspeak-eterm-coordinates-to-position top-left)
              end (emacspeak-eterm-coordinates-to-position bottom-right))
        (setq contents
              (cond
               ((and left-stretch right-stretch ) ;; stretchable window
                (goto-char start )
                (beginning-of-line )
                (setq start (point))
                (goto-char end)
                (end-of-line )
                (buffer-substring start (point)))
               (right-stretch
                (let  ((lines nil))
                  (goto-char start )
                  (while (< start end )
                    (end-of-line)
                    (push
                     (buffer-substring start (point))
                     lines)
                    (forward-line 1)
                    (beginning-of-line)
                    (forward-char (car top-left))
                    (setq start (point)))
                  (setq lines (nreverse lines ))
                  (mapconcat 'identity
                             lines " \n ")))
               (left-stretch
                (let  ((lines nil))
                  (goto-char start )
                  (beginning-of-line)
                  (setq start (point))
                  (while (< start end )
                    (forward-char (car bottom-right ))
                    (push
                     (buffer-substring start (point ))
                     lines)
                    (forward-line 1)
                    (beginning-of-line)
                    (setq start (point)))
                  (setq lines (nreverse lines ))
                  (mapconcat 'identity
                             lines " \n ")))
               (t (mapconcat 'identity
                             (extract-rectangle start end )
                             " \n "))))
        (emacspeak-eterm-nuke-cached-info)
        contents ))))

(defvar emacspeak-eterm-maximum-windows 20
  "Variable specifying how many windows can be defined.")

(defvar emacspeak-eterm-window-table
  (make-vector emacspeak-eterm-maximum-windows  nil)
  "Vector of window positions.
A terminal window is recorded by the  positions of its top left
and bottom right.")
;;;###autoload
(defun emacspeak-eterm-record-window  (window-id top-left bottom-right
                                                 &optional right-stretch left-stretch )
  "Insert this window definition into the table of terminal windows.
Argument WINDOW-ID specifies the window.
Argument TOP-LEFT  specifies top-left of window.
Argument BOTTOM-RIGHT  specifies bottom right of window.
Optional argument RIGHT-STRETCH  specifies if the window stretches to the right.
Optional argument LEFT-STRETCH  specifies if the window stretches to the left."
  (declare (special emacspeak-eterm-window-table
                    emacspeak-eterm-maximum-windows))
  (assert (< window-id emacspeak-eterm-maximum-windows)  t
          "Your installation of Emacspeak only supports %d windows"
          emacspeak-eterm-maximum-windows )
  (aset emacspeak-eterm-window-table window-id 
        (emacspeak-eterm-make-window top-left bottom-right
                                     right-stretch left-stretch)))

(defun emacspeak-eterm-get-window (id)
  "Retrieve a window.
Argument ID specifies window whose definition is being requested."
  (declare (special emacspeak-eterm-window-table
                    emacspeak-eterm-maximum-windows))
  (assert (<  id emacspeak-eterm-maximum-windows)  t
          "Your installation of Emacspeak only supports %d windows"
          emacspeak-eterm-maximum-windows )
  (or (aref emacspeak-eterm-window-table  id)
      (error "Window %s is not defined" id )))

(defun emacspeak-eterm-define-window (id)
  "Prompt for a window ID.
The window is then define to be
the rectangle delimited by point and eterm mark.  This is to
be used when emacspeak is set to review mode inside an
eterm."

  (interactive "nDefine window: ")
  (declare (special emacspeak-eterm-marker emacspeak-eterm-pointer
                    emacspeak-eterm-maximum-windows))
  (assert (<  id emacspeak-eterm-maximum-windows)  t
          "Your installation of Emacspeak only supports %d windows"
          emacspeak-eterm-maximum-windows )
  (let  ((top-left
          (emacspeak-eterm-position-to-coordinates (marker-position emacspeak-eterm-marker)))
         (bottom-right
          (emacspeak-eterm-position-to-coordinates (marker-position
                                                    emacspeak-eterm-pointer)))
         (right-stretch
          (y-or-n-p "Should the window stretch to the right as required "))
         (left-stretch
          (y-or-n-p "Should the window stretch to the left as required ")))
    (emacspeak-eterm-record-window  id top-left bottom-right
                                    right-stretch left-stretch)
    (message "Defined %s window %s
with top left at %s %s
and bottom right at %s %s"
             (cond
              ((and left-stretch right-stretch)
               " stretchable ")
              (left-stretch " left stretchable")
              (right-stretch " right stretchable ")
              (t " "))
             id (cdr top-left) (car top-left)
             (cdr bottom-right) (car bottom-right ))))

(defun emacspeak-eterm-speak-window (id)
  "Speak an eterm window.
Argument ID specifies the window."
  (interactive "nSpeak window")
  (declare (special emacspeak-eterm-maximum-windows
                    term-home-marker ))
  (assert (<  id emacspeak-eterm-maximum-windows)  t
          "Your installation of Emacspeak only supports %d windows"
          emacspeak-eterm-maximum-windows )
  (save-excursion
    (save-restriction
      (narrow-to-region term-home-marker (point-max ))
      (dtk-speak
       (emacspeak-eterm-return-window-contents
        (emacspeak-eterm-get-window id ))))))

(defun emacspeak-eterm-yank-window (id)
  "Yank contents of  an eterm window at point."
  (interactive "nYank contents of window")
  (declare (special emacspeak-eterm-maximum-windows
                    term-home-marker ))
  (assert (<  id emacspeak-eterm-maximum-windows)  t
          "Your installation of Emacspeak only supports %d windows"
          emacspeak-eterm-maximum-windows )
  (insert 
   (save-excursion
     (save-restriction
       (narrow-to-region term-home-marker (point-max ))
       (emacspeak-eterm-return-window-contents
        (emacspeak-eterm-get-window id )))))
  (emacspeak-auditory-icon 'yank-object)
  (message "Yanked contents of window %s at point" id ))

(defun emacspeak-eterm-describe-window  (id)
  "Describe an eterm  window.
Description indicates eterm window coordinates and whether it is stretchable"
  (interactive "nDescribe window: ")
  (let* ((window (emacspeak-eterm-get-window id ))
         (top-left (emacspeak-eterm-window-top-left window))
         (bottom-right (emacspeak-eterm-window-bottom-right window ))
         (right-stretch (emacspeak-eterm-window-right-stretch window))
         (left-stretch (emacspeak-eterm-window-left-stretch window )))
    (message " %s window %s
has  top left at %s %s
and bottom right at %s %s"
             (cond
              ((and left-stretch right-stretch)
               " stretchable ")
              (left-stretch " left stretchable")
              (right-stretch " right stretchable ")
              (t " "))
             id (cdr top-left) (car top-left)
             (cdr bottom-right) (car bottom-right ))))

(defvar emacspeak-eterm-focus-window nil
  "Current window that emacspeak eterm focuses on")
(make-variable-buffer-local 'emacspeak-eterm-filter-window)

(defun emacspeak-eterm-set-focus-window (flag)
  "Prompt for the id of a predefined window,
and set the `focus' window to it.
Non-nil interactive prefix arg `unsets' the focus window;
this is equivalent to having the entire terminal as the focus window (this is
what eterm starts up with).
Setting the focus window results in emacspeak  monitoring screen
and speaking that window upon seeing screen activity."
  (interactive "P")
  (declare (special emacspeak-eterm-focus-window ))
  (let  ((window-id nil))
    (cond
     ( flag (setq emacspeak-eterm-focus-window nil)
            (message "Emacspeak eterm focus set to entire screen "))
     (t
      (setq window-id
            (read-minibuffer  "Specify eterm window to focus on "))
      (assert (numberp window-id) t
              "Please specify a valid window id, a
non-negative integer ")
      (cond
       ((= 0 window-id)
        (message "Unset focus window.")
        (setq emacspeak-eterm-focus-window nil))
       (t 
        (setq emacspeak-eterm-focus-window window-id )
        (message "Set emacspeak eterm focus window  to %d "
                 window-id )))))))

(defvar emacspeak-eterm-filter-window nil
  "Window id used to filter screen activity.")

(make-variable-buffer-local 'emacspeak-eterm-filter-window)
(defun emacspeak-eterm-set-filter-window (flag)
  "Prompt for the id of a predefined window,
and set the `filter' window to it.
Non-nil interactive prefix arg `unsets' the filter window;
this is equivalent to having the entire terminal as the filter window (this is
what eterm starts up with).
Setting the filter window results in emacspeak  only monitoring screen
activity within the filter window."
  (interactive "P")
  (declare (special emacspeak-eterm-filter-window ))
  (let  ((window-id nil))
    (cond
     ( flag (setq emacspeak-eterm-filter-window nil)
            (message "Emacspeak eterm filter set to entire screen "))
     (t
      (setq window-id
            (read-minibuffer  "Specify eterm window to filter on "))
      (assert (numberp window-id) t
              "Please specify a valid window id, a non-negative integer ")
      (cond
       ((= 0 window-id)
        (message "Unset filter window.")
        (setq emacspeak-eterm-filter-window nil))
       (t 
        (setq emacspeak-eterm-filter-window window-id )
        (message "Set emacspeak eterm filter window  to %d " window-id )))))))

(defun emacspeak-eterm-toggle-focus-window ()
  "Toggle active state of focus window."
  (interactive)
  (declare (special emacspeak-eterm-focus-window))
  (if emacspeak-eterm-focus-window
      (setq emacspeak-eterm-focus-window nil)
    (setq emacspeak-eterm-focus-window 1))
  (dtk-stop)
  (emacspeak-auditory-icon
   (if emacspeak-eterm-focus-window
       'on 'off)))

(defun emacspeak-eterm-toggle-filter-window ()
  "Toggle active state of filter window."
  (interactive)
  (declare (special emacspeak-eterm-filter-window))
  (if emacspeak-eterm-filter-window
      (setq emacspeak-eterm-filter-window nil)
    (setq emacspeak-eterm-filter-window 1))
  (dtk-stop)
  (emacspeak-auditory-icon
   (if emacspeak-eterm-filter-window
       'on 'off)))

(defun emacspeak-eterm-speak-predefined-window ()
  "Speak a predefined eterm window between 1 and 10."
  (interactive)
  (emacspeak-eterm-speak-window
   (condition-case nil
       (read (format "%c" last-input-event ))
     (error nil ))))

;;}}}
;;{{{  advice emulator

(defvar eterm-current-personality nil
  "Current personality for eterm. ")

(defadvice term (before emacspeak pre act)
  "Single window please!"
  (delete-other-windows))

(defadvice ansi-term (before emacspeak pre act)
  "Single window please!"
  (delete-other-windows))

(defadvice term-mode   (after emacspeak pre act )
  "Customize eterm to work with Emacspeak.
Additional commands provided by emacspeak under eterm are
available with the prefix emacspeak-eterm-prefix and are listed below:
\\{emacspeak-eterm-keymap}"
  (declare (special emacspeak-eterm-pointer emacspeak-eterm-marker))
  (emacspeak-eterm-setup-keys )
  (emacspeak-eterm-setup-raw-keys)
  (make-local-variable 'eterm-current-personality)
  (setq eterm-current-personality emacspeak-eterm-default-personality)
  (voice-lock-mode 1)
  (modify-syntax-entry 10 ">")
  (make-local-variable 'emacspeak-eterm-pointer)
  (setq emacspeak-eterm-pointer  (copy-marker (point)))
  (make-local-variable 'emacspeak-eterm-marker)
  (setq emacspeak-eterm-marker  (copy-marker (point))))

(defvar emacspeak-eterm-row nil
  "Record the eterm row last spoken")

(defvar emacspeak-eterm-column nil
  "Record the column last spoken")

(defvar emacspeak-eterm-pointer nil
  "Terminal pointer. Can be moved around to listen to the contents of the
terminal. See commands provided by the emacspeak extension to eterm:
\\{emacspeak-eterm-keymap}.
Each term-mode buffer has a buffer local value of this variable. ")

(defvar emacspeak-eterm-marker nil
  "Mark set in an eterm buffer. Used to cut and paste from the terminal.")

(defvar emacspeak-eterm-autospeak t
  "Tells if eterm output is automatically spoken when in line mode.
Use command emacspeak-toggle-eterm-autospeak bound to
\\[emacspeak-toggle-eterm-autospeak] to set this.")
  
(make-variable-buffer-local 'emacspeak-eterm-autospeak)
  
(ems-generate-switcher 'emacspeak-toggle-eterm-autospeak
                       'emacspeak-eterm-autospeak
                       "Toggle state of eterm autospeak.
When eterm autospeak is turned on and the terminal is in line mode,
all output to the terminal is automatically spoken. 
  Interactive prefix arg means toggle  the global default value, and then set the
  current local  value to the result. ")

(defvar eterm-line-mode nil 
  "T if eterm is in line mode.")

(defvar eterm-char-mode t
  "Flag indicating if eterm is in char mode.")

(defvar emacspeak-eterm-pointer-mode t 
  "If T then the emacspeak pointer will not track the terminal cursor.
Do not set this by hand.
Use command emacspeak-eterm-toggle-pointer-mode bound to
\\[emacspeak-eterm-toggle-pointer-mode].")

(defadvice  term-emulate-terminal (around emacspeak pre act compile )
  "Record position, emulate, then speak what happened.
Also keep track of terminal highlighting etc.  Feedback is
limited to current window If a `current window` is set (see
command emacspeak-eterm-set-filter-window bound to
\\[emacspeak-eterm-set-filter-window].  How output is spoken
depends on whether the terminal is in character or line mode.

When in character mode, output is spoken like off a real
terminal.  When in line mode, behavior resembles that of comint
mode; i.e. you hear the output if emacspeak-eterm-autospeak is t.
Do not set this variable by hand: See command
emacspeak-toggle-eterm-autospeak bound to
\\[emacspeak-toggle-eterm-autospeak]"
  (declare (special emacspeak-eterm-row emacspeak-eterm-column
                    eterm-line-mode eterm-char-mode
                    emacspeak-eterm-filter-window emacspeak-eterm-pointer-mode
                    emacspeak-eterm-autospeak 
                    term-current-row term-current-column))
  (let ((emacspeak-eterm-row (term-current-row ))
        (emacspeak-eterm-column (term-current-column ))
        (end (point-max))
        (current-char (preceding-char ))
        (new-end nil)
        (new-row nil)
        (new-column nil )
        (old-point (point))
        (dtk-stop-immediately (not eterm-line-mode)))
    ad-do-it
    (setq new-row (term-current-row )
          new-column (term-current-column )
          new-end (point-max))
    (when (and  emacspeak-eterm-autospeak
                (window-live-p
                 (get-buffer-window (process-buffer (ad-get-arg 0))))
                (or  (not emacspeak-eterm-focus-window )
                     (and (emacspeak-eterm-coordinate-within-window-p
                           (cons new-column new-row )
                           emacspeak-eterm-focus-window )
                          (emacspeak-eterm-coordinate-within-window-p
                           (cons (term-current-column) (term-current-row))
                           emacspeak-eterm-focus-window ))
                     (and (emacspeak-eterm-coordinate-within-window-p
                           (cons new-column new-row )
                           emacspeak-eterm-filter-window )
                          (emacspeak-eterm-coordinate-within-window-p
                           (cons (term-current-column) (term-current-row))
                           emacspeak-eterm-filter-window ))))
      (cond
       ((and eterm-char-mode
             emacspeak-eterm-filter-window
             (not (and (emacspeak-eterm-coordinate-within-window-p
                        (cons new-column new-row )
                        emacspeak-eterm-filter-window )
                       (emacspeak-eterm-coordinate-within-window-p
                        (cons (term-current-column) (term-current-row))
                        emacspeak-eterm-filter-window )))) nil)
       ((and  eterm-line-mode
              emacspeak-eterm-autospeak)
        (setq dtk-stop-immediately nil)
        (emacspeak-dtk-sync)
        (condition-case nil 
            (emacspeak-speak-region
             (1- old-point)
             (1- (point )))
          (error nil )))
       (emacspeak-eterm-focus-window
        (emacspeak-eterm-speak-window emacspeak-eterm-focus-window))
       ((and (or (eq last-command-event 127) ; xterm/console sends 127
                 (eq last-command-event 'backspace)) ; X sends 'backspace
             (= new-row emacspeak-eterm-row )
             (= -1 (- new-column emacspeak-eterm-column ))
             current-char)              ;you backspaced?
        (emacspeak-speak-this-char current-char)
        (delete-char  1)
        (dtk-tone 500 50))
       ((and (= new-row emacspeak-eterm-row )
             (= 1 (- new-column emacspeak-eterm-column ))) ;you inserted a character:
        (if (eq 32 last-command-event )
            (save-excursion
              (backward-char 2)
              (emacspeak-speak-word nil))
          (emacspeak-speak-this-char (preceding-char ))))
       ((and (= new-row emacspeak-eterm-row )
             (= 1 (abs(- new-column emacspeak-eterm-column ))))
        (emacspeak-speak-this-char (following-char )))
       ((= emacspeak-eterm-row new-row)
        (if (= 32 (following-char))
            (save-excursion (forward-char 1)
                            (emacspeak-speak-word))        
          (emacspeak-speak-word )))
       (t (emacspeak-speak-line nil )))
      (when (and (not  emacspeak-eterm-pointer-mode )
                 emacspeak-eterm-pointer)
        (emacspeak-eterm-pointer-to-cursor)))))

(ems-generate-switcher 'emacspeak-eterm-toggle-pointer-mode
                       'emacspeak-eterm-pointer-mode
                       "Toggle emacspeak eterm pointer mode.
With optional interactive prefix  arg, turn it on.
When emacspeak eterm is in pointer mode, the eterm read pointer
stays where it is rather than automatically moving to the terminal cursor when
there is terminal activity.")

   
(defadvice term-dynamic-complete (around emacspeak pre act)
  "Speak the completion. "
  (declare (special emacspeak-eterm-row term-current-row))
  (let  ((saved-point (point)))
    ad-do-it
    (unless (= saved-point (point))
      (emacspeak-speak-region saved-point (point)))
    ad-return-value )
  )
(voice-setup-add-map
 '(
   (term-underline voice-brighten-medium)
   ))
(defadvice term-line-mode (after emacspeak pre act)
  "Announce that you entered line mode. "
  (make-local-variable 'eterm-line-mode)
  (setq mode-line-process
        '("line"))
  (setq eterm-char-mode nil 
        eterm-line-mode t )
  (when (interactive-p)
    (dtk-speak "Terminal line mode ")))

(defadvice term-char-mode (after emacspeak pre act)
  "Announce you entered character mode. "
  (setq mode-line-process
        '("char"))
  (setq eterm-char-mode t
        eterm-line-mode nil )
  (emacspeak-eterm-setup-raw-keys)
  (when (interactive-p)
    (dtk-speak "Terminal character mode ")))

;;}}}
;;{{{  Advice term functions that duplicate functionality of their comint counterparts

(defadvice term-next-input (after emacspeak pre act)
  "Speak the line. "
  (when (interactive-p)
    (emacspeak-speak-line )))

(defadvice term-next-matching-input (after emacspeak pre act)
  "Speak the line. "
  (when (interactive-p)
    (emacspeak-speak-line )))

(defadvice term-previous-input (after emacspeak pre act)
  "Speak the line. "
  (when (interactive-p)
    (emacspeak-speak-line )))

(defadvice term-previous-matching-input (after emacspeak pre act)
  "Speak the line. "
  (when (interactive-p)
    (emacspeak-speak-line )))

(defadvice term-send-input (after emacspeak pre act)
  "Flush any ongoing speech"
  (when (interactive-p)
    (dtk-stop)))

(defadvice term-previous-prompt (after emacspeak pre act )
  "Provide spoken feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (if (eolp)
        (emacspeak-speak-line)
      (emacspeak-speak-line 1))))

(defadvice term-next-prompt (after emacspeak pre act )
  "Provide spoken feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (if (eolp)
        (emacspeak-speak-line)
      (emacspeak-speak-line 1))))
(defadvice term-dynamic-list-input-ring (after emacspeak pre act)
  "Provide auditory feedback"
  (message  "Switch to the other window to browse the input history "))

(defadvice term-kill-output (after emacspeak pre act )
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (message "Nuked output of last command ")))

(defadvice term-quit-subjob (after emacspeak pre act )
  "Provide auditory feedback"
  (when (interactive-p)
    (message "Sent quit signal to subjob ")))

(defadvice term-stop-subjob (after emacspeak pre act )
  "Provide auditory feedback"
  (when (interactive-p)
    (message "Stopped the subjob")))

(defadvice term-interrupt-subjob (after emacspeak pre act )
  "Provide auditory feedback"
  (when (interactive-p)
    (message "Interrupted  the subjob")))

(defadvice term-kill-input (before emacspeak pre act )
  "Provide spoken feedback"
  (when (interactive-p)
    (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
      (when  (> (point) (marker-position pmark))
        (emacspeak-auditory-icon 'delete-object )
        (emacspeak-speak-region  pmark (point))))))

(defadvice term-dynamic-list-filename-completions (after emacspeak pre act )
  "Provide auditory feedback"
  (when (interactive-p)
    (message "Switch to the completions window to browse the possible
completions for filename at point")))

;;}}}
;;{{{  Launch remote terminals
;;;###autoload
(defvar emacspeak-eterm-remote-hosts-table
  (make-vector 127 0)
  "obarray used for completing hostnames when prompting for a remote
host. Hosts are added whenever a new hostname is encountered, and the
list of known hostnames is persisted in file named by
emacspeak-eterm-remote-hostnames")
(declaim (special emacspeak-resource-directory))
(defcustom emacspeak-eterm-remote-hosts-cache
  (expand-file-name ".hosts" emacspeak-resource-directory)
  "File where list of known remote hosts is cached"
  :type 'file
  :group 'emacspeak-eterm)

(defun emacspeak-eterm-load-remote-hosts-cache ()
  "Load cached remote hostnames"
  (declare (special emacspeak-eterm-remote-hosts-table
                    emacspeak-eterm-remote-hosts-cache))
  (when (file-exists-p emacspeak-eterm-remote-hosts-cache )
    (let ((host nil)
          (hosts (find-file-noselect emacspeak-eterm-remote-hosts-cache)))
      (save-excursion
        (set-buffer hosts)
        (goto-char (point-min))
        (while (not (eobp))
          (setq host
                (buffer-substring
                 (point)
                 (progn (end-of-line) (point))))
          (intern host emacspeak-eterm-remote-hosts-table)
          (forward-line 1))))))

(eval-when (load)
  (emacspeak-eterm-load-remote-hosts-cache))
;;;###autoload
(defun emacspeak-eterm-cache-remote-host (host)
  "Add this hostname to cache of remote hostnames"
  (declare (special emacspeak-eterm-remote-hosts-table
                    emacspeak-eterm-remote-hosts-cache))
  (let ((buffer (find-file-noselect
                 emacspeak-eterm-remote-hosts-cache)))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-max))
      (insert (format "%s\n" host))
      (save-buffer))
    (intern host emacspeak-eterm-remote-hosts-table)))
;;;###autoload
(defun emacspeak-eterm-remote-term (host )
  "Start a terminal-emulator in a new buffer."
  (interactive
   (list
    (completing-read "Remote host: "
                     emacspeak-eterm-remote-hosts-table)))
  (declare (special emacspeak-eterm-remote-hosts-table))
  (require 'term)
  (set-buffer (make-term (format "%s-terminal" host)
                         "rlogin"nil  host))
  (term-char-mode)
  (unless (intern-soft host emacspeak-eterm-remote-hosts-table)
    (emacspeak-eterm-cache-remote-host host))
  (switch-to-buffer (format "*%s-terminal*" host) ))
     
;;}}}
(provide 'emacspeak-eterm)
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
