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

;;;Copyright (C) 1995 -- 2002, T. V. Raman 
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
;;{{{ Introduction 

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

;;

;;; How faces map to voices:
;;; TTS engine specific modules e.g., dtk-voices.el and
;;; outloud-voices.el 
;;; define a standard set of voice names.
;;; This module maps standard "personality" names to these pre-defined
;;; voices.
;;; It  does this via special form def-voice-font 
;;; which takes a personality name, a voice name and a face name to
;;; set up the mapping between face and personality, and personality
;;; and voice.
;;; See many instances of this usage in this module.
;;; This special form is available for use from other emacspeak
;;; modules.

;;; Special form def-voice-font sets up the personality name to be
;;; available via custom.

;;; new voices can be defined using CSS style specifications 
;;; see special form defvoice
;;; Voices defined via defvoice can be customized via custom 
;;; see the documentation for defvoice.

;;}}}
;;{{{ Required modules

;;; Code:
(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'custom)
(require 'backquote)
(require 'acss-structure)
(require 'outloud-voices)
(require 'dtk-voices)

;;}}}
;;{{{ customization group 
(defgroup voice-fonts nil
  "Customization group for setting voices."
  :group 'emacspeak)

;;}}}

;;{{{  helper for voice custom items:

(defalias 'tts-list-voices 'dtk-list-voices)
(defun voice-setup-custom-menu ()
  (let ((v (tts-list-voices)))
    (setq v
          (cons 
           (list 'symbol :tag "Other")
           v))
    (cons 'choice v)))

;;}}}
;;{{{ map faces to voices 

(defvar voice-setup-face-voice-table (make-hash-table)
  "Hash table holding face to voice mapping.")

(defsubst voice-setup-set-voice-for-face (face voice)
  "Map face --a symbol-- to relevant voice."
  (declare (special  voice-setup-face-voice-table))
  (setf (gethash face voice-setup-face-voice-table) voice))

(defsubst voice-setup-get-voice-for-face (face)
  "Map face --a symbol-- to relevant voice."
  (declare (special  voice-setup-face-voice-table))
  (symbol-value (gethash face voice-setup-face-voice-table)))
    

;;; voiceifies faces not already voiceified as specified in
;;; voice-setup-face-voice-table

(defun voice-setup-face-to-voice (start end)
  "Voiceify faces in specified region that are not already voicefied.
Face to voice mapping is specified in
voice-setup-face-voice-table.
This function forces voice-lock mode on."
  (interactive "r")
  (declare (special buffer-read-only
                    inhibit-read-only
                    inhibit-point-motion-hooks
                    voice-lock-mode))
  (setq voice-lock-mode t)
  ;;; this is ems-modify-buffer-safely inlined 
  (unwind-protect
      (let    ((save-read-only buffer-read-only)
               (buffer-read-only nil )
               (save-inhibit-read-only inhibit-read-only)
               (inhibit-read-only t)
               (save-inhibit-point-motion-hooks (if (boundp 'inhibit-point-motion-hooks)
                                                    inhibit-point-motion-hooks
                                                  nil))
               (inhibit-point-motion-hooks t)
               (modification-flag (buffer-modified-p)))
        (unwind-protect
               ;;; body
            (save-excursion
              (goto-char start)
              (let ((face nil )
                    (voice nil))
                (goto-char start)
                (while (and  (not (eobp))
                             (< start end))
                  (setq face (get-text-property (point) 'face ))
                  (goto-char
                   (or
                    (next-single-property-change (point) 'face
                                                 (current-buffer) end)
                    end))
                  (when(and face
                            (symbolp face)
                            (setq voice
                                  (voice-setup-get-voice-for-face face))
                            (not (get-text-property (point) 'personality)))
                    (put-text-property start  (point)
                                       'personality voice))
                  (setq start (point)))
                (setq buffer-read-only save-read-only
                      inhibit-read-only save-inhibit-read-only
                      inhibit-point-motion-hooks save-inhibit-point-motion-hooks)
                (set-buffer-modified-p modification-flag )))))))

;;}}}
;;{{{ special form def-voice-font 

(defmacro  def-voice-font (personality voice face doc &rest args)
  "Define personality and map it to specified face."
  (`
   (defcustom (, personality)
     (, voice)
     (, doc)
     :type (voice-setup-custom-menu)
     :group 'voice-fonts
     :set '(lambda  (sym val)
             (voice-setup-set-voice-for-face (, face) '(, personality))
             (set-default sym val))
     (,@ args))))

;;}}}
;;{{{  special form defvoice 
(defvar voice-setup-personality-table (make-hash-table)
  "Maps personality names to ACSS  settings.
Keys are personality names.")

(defsubst voice-setup-personality-from-style (style-list)
  "Define a personality given a list of speech style settings."
  (declare (special voice-setup-personality-table))
  (let ((voice
         (acss-personality-from-speech-style
          (make-acss
           :family (first style-list)
           :average-pitch (second style-list)
           :pitch-range (third style-list)
           :stress (fourth style-list)
           :richness (fifth style-list)))))
    (puthash  voice style-list voice-setup-personality-table)
    voice))

(defmacro defvoice (personality settings doc)
  "Define voice using CSS setting.  Setting is a list of the form
(list paul 5 5 5 5 ) which defines a standard male voice.  Once
defined, the newly declared personality can be customized by calling
command \\[customize-variable] on <personality>-settings."
  (`
   (defcustom (, (intern (format "%s-settings"  personality)))
     (, settings)
     (, doc)
     :type  '(list
              (choice :tag "Family"
               (const :tag "Unspecified" nil)
               (symbol :tag "Name"))
              (choice :tag "Average Pitch"
               (const :tag "Unspecified" nil)
               (integer :tag "Number"))
              (choice :tag "Pitch Range"
               (const :tag "Unspecified" nil)
               (integer :tag "Number"))
              (choice :tag "Stress"
               (const :tag "Unspecified" nil)
               (integer :tag "Number"))
              (choice :tag "Richness"
               (const :tag "Unspecified" nil)
               (integer :tag "Number")))
     :group 'voice-fonts
     :set
     '(lambda  (sym val)
        (let ((voice-name
               (voice-setup-personality-from-style val)))
          (setq (, personality) voice-name)
          (set-default sym val))))))

;;}}}
;;{{{ voices defined using ACSS.

;;; these voices are device independent.
;;; they will eventually replace most of the device specific voices 

(defvoice  voice-monotone
  (list nil nil 0 0 nil)
  "Turns current voice into a monotone.")

(defvoice  voice-monotone-medium
  (list nil nil 2 2  nil)
  "Turns current voice into a medium monotone.")

(defvoice voice-animate
  (list nil 7 7 6)
  "Animates current voice.")

(defvoice voice-animate-medium
  (list nil 6 6  5)
  "Adds medium animation  current voice.")

(defvoice voice-animate-extra
  (list nil 8 8 7 )
  "Adds extra animation  current voice.")

(defvoice voice-smoothen 
  (list nil nil nil 3 4)
  "Smoothen current voice.")

(defvoice voice-smoothen-extra
  (list nil nil nil 3 2)
  "Extra smoothen current voice.")

(defvoice voice-smoothen-medium
  (list nil nil nil 3 3)
  "Extra smoothen current voice.")

(defvoice voice-brighten-medium 
  (list nil nil nil 5 6)
  "Brighten current voice.")

(defvoice voice-brighten 
  (list nil nil nil 6 7)
  "Brighten current voice.")

(defvoice voice-brighten-extra
  (list nil nil nil 7 8 )
  "Extra brighten current voice.")

(defvoice voice-bolden 
  (list nil 1 6 6  nil)
  "Bolden current voice.")

(defvoice voice-bolden-extra
  (list nil 0 6 7 8 )
  "Extra bolden current voice.")

(defvoice voice-bolden-medium
  (list nil 3 6 6  nil)
  "Add medium bolden current voice.")

(defvoice voice-lighten
  (list nil 7 7  1  nil)
  "Lighten current voice.")

(defvoice voice-lighten-medium
  (list nil 6 6 3  nil)
  "Add medium lighten current voice.")

(defvoice voice-lighten-extra
  (list nil 8 8 0   nil)
  "Add extra lighten current voice.")

(defvoice voice-bolden-and-animate
  (list nil 8 8 8 8 )
  "Bolden and animate  current voice.")

;;}}}
;;{{{  Define some voice personalities:

(def-voice-font voice-lock-highlight-personality voice-animate
  'highlight
  "Personality used for highlighting text.")

(def-voice-font voice-lock-comment-personality voice-monotone
  'font-lock-comment-face
  "Personality to use for comments.")
          
(def-voice-font voice-lock-underline-personality voice-brighten-medium
  'underline
  "Personality to use for underline text.")
  
(def-voice-font voice-lock-bold-personality voice-bolden
  'bold
  "Personality to use for bold  text.")

(def-voice-font voice-lock-italic-personality 
  voice-animate
  'italic
  "Personality to use for italic  text.")

(def-voice-font voice-lock-bold-italic-personality 
  voice-bolden-and-animate
  'bold-italic
  "Personality to use for bold  text.")
  
(def-voice-font voice-lock-doc-string-personality
  voice-smoothen-extra
  'font-lock-doc-string-face
  "Personality to use for documentation strings.")
  
(def-voice-font voice-lock-constant-personality voice-lighten
  'font-lock-constant-face
  "Personality to use for  constants.")
  
(def-voice-font voice-lock-string-personality 'betty
  'font-lock-string-face
  "Personality to use for string constants.")

(def-voice-font voice-lock-function-name-personality voice-bolden-medium
  'font-lock-function-name-face
  "Personality to use for function names.")
  
(def-voice-font voice-lock-warning-personality voice-bolden-and-animate
  'font-lock-warning-face
  "Personality to use for warnings.")

(def-voice-font voice-lock-keyword-personality 'ursula
  'font-lock-keyword-face
  "Personality to use for keywords.")
  
(def-voice-font voice-lock-builtin-personality 'harry
  'font-lock-builtin-face
  "Personality to use for built-in keywords.")

(def-voice-font voice-lock-variable-name-personality voice-animate
  'font-lock-variable-name-face
  "Personality to use for variables.")
  
(def-voice-font voice-lock-type-personality voice-smoothen
  'font-lock-type-face
  "Personality to use for data types.")
  
(def-voice-font voice-lock-reference-personality voice-animate-medium
  'font-lock-reference-face
  "Personality to use for references.")

;;}}}
;;{{{ setup standard mappings

;; (voice-setup-set-voice-for-face 'bold 'bold)
;; (voice-setup-set-voice-for-face 'italic 'italic)
;; (voice-setup-set-voice-for-face 'underline 'underline)
;; (voice-setup-set-voice-for-face 'underlined 'underline)
;; (voice-setup-set-voice-for-face 'bold-italic 'bold-italic)

;; (voice-setup-set-voice-for-face  'font-lock-comment-face 'voice-lock-comment-personality)
;; (voice-setup-set-voice-for-face 'font-lock-underline-face   'voice-lock-underline-personality )
;; (voice-setup-set-voice-for-face 'font-lock-bold-face   'voice-lock-bold-personality )
;; (voice-setup-set-voice-for-face 'font-lock-italic-face   'voice-lock-italic-personality )
;; (voice-setup-set-voice-for-face 'font-lock-doc-string-personality   'voice-lock-doc-string-personality)
;; (voice-setup-set-voice-for-face'font-lock-constant-face   'voice-lock-constant-personality)
;; (voice-setup-set-voice-for-face'font-lock-string-face   'voice-lock-string-personality)
;; (voice-setup-set-voice-for-face'font-lock-function-name-face 'voice-lock-function-name-personality)
;; (voice-setup-set-voice-for-face'font-lock-warning-face   'voice-lock-warning-personality)
;; (voice-setup-set-voice-for-face'font-lock-keyword-face   'voice-lock-keyword-personality)
;; (voice-setup-set-voice-for-face'font-lock-builtin-face   'voice-lock-builtin-personality)
;; (voice-setup-set-voice-for-face'font-lock-variable-name-face 'voice-lock-variable-name-personality)
;; (voice-setup-set-voice-for-face'font-lock-type-face   'voice-lock-type-personality)
;; (voice-setup-set-voice-for-face'font-lock-reference-face   'voice-lock-reference-personality)

;;}}}
(provide 'voice-setup)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

