;;; voice-setup.el --- Setup voices for voice-lock
;;; $Id$
;;; $Author$ 
;;; Description:  Voice lock mode for Emacspeak
;;{{{  LCD Archive entry: 
;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu 
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision$ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;;Copyright (C) 1995 -- 2001, T. V. Raman 
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

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
;;{{{  voice aadditions

(require 'cl)
;;; Commentary:
;;; A voice is to audio as a font is to a visual display.
;;; A personality is to audio as a face is to a visual display. 
;;; 
;; Voice-lock-mode is a minor mode that causes your comments to be 
;; spoken in one personality, strings in another, reserved words in another,
;; documentation strings in another, and so on.
;;
;; Comments will be spoken in `voice-lock-comment-personality'.
;; Strings will be spoken in `voice-lock-string-personality'.
;; Doc strings will be spoken in `voice-lock-doc-string-personality'.
;; Function and variable names (in their defining forms) will be
;;  spoken in `voice-lock-function-name-personality'.
;; Reserved words will be spoken in `voice-lock-keyword-personality'.
;;
;; To make the text you type be voiceified, use M-x voice-lock-mode.
;; When this minor mode is on, the voices of the current line are
;; updated with every insertion or deletion.
;;
;; To define new reserved words or other patterns to highlight, use
;; the `voice-lock-keywords' variable.  This should be mode-local.
;;
;;{{{  Define some voice personalities:

(defvar voice-lock-comment-personality
  'paul-monotone  
  "Personality to use for comments.")
(defvar voice-lock-underline-personality 
  'paul-animated 
  "Personality to use for underline text.")

(defvar voice-lock-bold-personality 
  'harry
  "Personality to use for bold  text.")

(defvar voice-lock-italic-personality 
  'paul-italic 
  "Personality to use for italic  text.")

(defvar voice-lock-doc-string-personality
  'dennis  
  "Personality to use for documentation strings.")

(defvar voice-lock-string-personality
  'betty 
  "Personality to use for string constants.")

(defvar voice-lock-function-name-personality
  'harry 
  "Personality to use for function names.")

(defvar voice-lock-warning-personality
  'paul-angry
  "Personality to use for function names.")

(defvar voice-lock-keyword-personality
  'ursula  
  "Personality to use for keywords.")

(defvar voice-lock-builtin-personality
  'harry
  "Personality to use for keywords.")
(defvar voice-lock-variable-name-personality
  'paul-animated
  "Personality to use for keywords.")

(defvar voice-lock-type-personality
  'paul-smooth 
  "Personality to use for data types.")

(defvar voice-lock-reference-personality
  'paul-animated
  "Personality to use for comments.")

;;}}}

;;}}}
;;{{{  additional convenience functions:

(defun voice-lock-set-personality (start end personality)
  "Set personality on region"
  (unwind-protect 
       (let    ((save-read-only buffer-read-only)
                (buffer-read-only nil )
                (inhibit-read-only t)
                (inhibit-point-motion-hooks t)
                (modification-flag (buffer-modified-p)))
         (unwind-protect
             (put-text-property start end
                                'personality personality)
           (setq buffer-read-only save-read-only
                 inhibit-read-only nil
                 inhibit-point-motion-hooks nil)
           (set-buffer-modified-p modification-flag )))
     (setq inhibit-read-only nil)))

;;}}}
(provide 'voice-setup)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

