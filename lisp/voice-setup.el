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

;;;Copyright (C) 1995 -- 2006, T. V. Raman
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
;; Comments will be spoken in `emacspeak-voice-lock-comment-personality'.
;; Strings will be spoken in `emacspeak-voice-lock-string-personality'.
;; Doc strings will be spoken in `emacspeak-voice-lock-doc-string-personality'.
;; Function and variable names (in their defining forms) will be
;;  spoken in `emacspeak-voice-lock-function-name-personality'.
;; Reserved words will be spoken in `emacspeak-voice-lock-keyword-personality'.
;;
;; To make the text you type be voiceified, use M-x voice-lock-mode.
;; When this minor mode is on, the voices of the current line are
;; updated with every insertion or deletion.
;;

;;

;;; How faces map to voices: TTS engine specific modules e.g.,
;;; dectalk-voices.el and outloud-voices.el define a standard set
;;; of voice names.  This module maps standard "personality"
;;; names to these pre-defined voices.  It does this via special
;;; form def-voice-font which takes a personality name, a voice
;;; name and a face name to set up the mapping between face and
;;; personality, and personality and voice.
;;; Newer Emacspeak modules should use voice-setup-add-map when
;;; defining face->personality mappings.
;;; Older code calls def-voice-font directly, but over time those
;;; calls will be changed to the more succinct form provided by
;;; voice-setup-add-map. For use from other moduels, also see
;;; function voice-setup-map-face which is useful when mapping a
;;; single face.
;;; Both voice-setup-add-map and voice-setup-map-face call
;;; special form def-voice-font.

;;; Special form def-voice-font sets up the personality name to
;;; be available via custom.  new voices can be defined using CSS
;;; style specifications see special form defvoice Voices defined
;;; via defvoice can be customized via custom see the
;;; documentation for defvoice.

;;}}}
;;{{{ Required modules

;;; Code:
(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'custom)
(require 'backquote)
(require 'acss-structure)
(require 'outloud-voices)
(require 'multispeech-voices)
(require 'dectalk-voices)
(require 'string)

;;}}}
;;{{{ customization group

(defgroup voice-fonts nil
  "Customization group for setting voices."
  :group 'emacspeak)

;;}}}
;;{{{  helper for voice custom items:

(defalias 'tts-list-voices 'dectalk-list-voices)
(defun voice-setup-custom-menu ()
  "Return a choice widget used in selecting voices."
  (let ((v (tts-list-voices))
        (menu nil))
    (setq menu
          (mapcar
           #'(lambda (voice)
               (list 'const voice))
           v))
    (setq menu
          (cons
           (list 'symbol :tag "Other")
           menu))
    (cons 'choice menu)))

(defun voice-setup-read-personality (&optional prompt)
  "Read name of a pre-defined personality using completion."
  (let ((table (mapcar
                #'(lambda (v)
                    (cons
                     (format "%s" v)
                     (format "%s" v)))
                (tts-list-voices))))
    (read
     (completing-read
      (or prompt "Personality: ")
      table))))

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
  (gethash face voice-setup-face-voice-table))

(defun voice-setup-show-rogue-voices ()
  "Return list of voices that map to non-existent faces."
  (declare (special voice-setup-face-voice-table))
  (loop
   for v being the hash-keys of
   voice-setup-face-voice-table
   unless (facep v)
   collect v))
        
;;}}}
;;{{{ special form def-voice-font

;;; note that when defined, personalities are registered as
;;; observers with the voice they use this gets unregistered when
;;; the mapping is changed via custom.

(defmacro  def-voice-font (personality voice face doc &rest args)
  "Define personality and map it to specified face."
  (let ((documentation
         (concat
          doc
          (format "\nThis personality uses  %s whose  effect can be changed globally by customizing %s-settings."
                  voice  voice))))
    (`
     (progn
       (unless (boundp '(, personality))
;;; New Personality
         (defcustom  (, personality)
           (, voice)
           (, documentation)
           :type (voice-setup-custom-menu)
           :group 'voice-fonts
           :set '(lambda (sym val)
                   (let ((observing  (get sym 'observing)))
                     (when (and (symbolp sym)
                                (symbolp observing))
                       (remprop observing sym))
                     (set-default sym val)))
           (,@ args)))
;;; other actions performed at define time
       (voice-setup-set-voice-for-face (, face) '(, personality))
;;;record  personality as an
;;;observer of  voice and vice versa
       (when (symbolp '(, personality))
         (put  '(, personality) 'observing '(, voice)))
       (when (symbolp '(, voice))
         (put  '(, voice) '(, personality) t))))))
(defsubst voice-setup-name-personality (face-name)
  "Compute personality name to use."
  (let ((name nil))
    (setq name
          (or
           (string-replace-match "face$" face-name "personality")
           face-name))
    (setq name
          (or
           (string-replace-match "font" name "voice")
           name))
    (when (string-equal name face-name)
      (setq name (format "%s-voice" name)))
    (concat "emacspeak-" name)))

(defun voice-setup-map-face (face voice)
  "Invoke def-voice-font with appropriately generated personality name."
  (let ((doc (format "Personality used for %s" face))
        (personality
         (intern (voice-setup-name-personality (symbol-name face)))))
    (eval
     `(def-voice-font ,personality ,voice  ',face  ,doc))))

(defun voice-setup-add-map (fv-alist )
  "Sets up face to voice mapping given in fv-alist."
  (loop for fv in fv-alist
        do
        (voice-setup-map-face (first fv) (second fv))))

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
           :family (nth 0 style-list)
           :average-pitch (nth 1 style-list)
           :pitch-range (nth 2 style-list)
           :stress (nth 3 style-list)
           :richness (nth 4  style-list)
           :punctuations (nth 5  style-list)))))
    (puthash  voice style-list voice-setup-personality-table)
    voice))

(defsubst voice-setup-observing-personalities  (voice-name)
  "Return a list of personalities that are `observing' VOICE-NAME.
Observing personalities are automatically updated when settings for
VOICE-NAME are  changed."
  (let* ((plist (symbol-plist voice-name))
         (l (1- (length plist))))
    (loop for i from 0 to l by 2
          collect (nth i plist))))

(defun voice-setup-update-personalities (personality)
  "Update  personalities  that use this voice to  new setting."
  (let ((value (symbol-value personality))
        (observers (voice-setup-observing-personalities personality)))
    (loop for o in observers
          do                            ;o is already quoted
          (set o value))))

;;; note that for now we dont use  gain settings

(defmacro defvoice (personality settings doc)
  "Define voice using CSS setting.  Setting is a list of the form
(list paul 5 5 5 5 'all) which defines a standard male voice
that speaks `all' punctuations.  Once
defined, the newly declared personality can be customized by calling
command \\[customize-variable] on <personality>-settings.. "
  (`
   (progn
     (defvar  (, personality)
       (voice-setup-personality-from-style (, settings))
       (concat
        (, doc)
        (, (format "Customize this overlay via %s-settings."
                 personality ))))
     (defcustom (, (intern (format "%s-settings"  personality)))
       (, settings)
       (, doc)
       :type  '(list
                (choice :tag "Family"
                        (const :tag "Unspecified" nil)
                        (const  :tag "Paul" paul)
(const :tag "Harry" harry)
(const :tag "Betty" betty))
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
        (integer :tag "Number"))
(choice :tag "Punctuation Mode "
        (const :tag "Unspecified" nil)
        (const :tag "All punctuations" all)
        (const :tag "Some punctuations" some)
        (const :tag "No punctuations" none)))
:group 'voice-fonts
:set
'(lambda  (sym val)
(let ((voice-name (voice-setup-personality-from-style val)))
  (setq (, personality) voice-name)
;;; update all observers                ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
  (voice-setup-update-personalities '(, personality))
  (set-default sym val)))))))

;;}}}                                   ; ;
;;{{{ voices defined using ACSS         ; ;

;;; these voices are device independent ; ;

(defvoice  voice-punctuations-all (list nil nil nil nil  nil 'all)
  "Turns current voice into one that  speaks all
punctuations.")

(defvoice  voice-punctuations-some (list nil nil nil nil  nil 'some)
  "Turns current voice into one that  speaks some
punctuations.")

(defvoice  voice-punctuations-none (list nil nil nil nil  nil "none")
  "Turns current voice into one that  speaks no punctuations.")

(defvoice  voice-monotone (list nil nil 0 0 nil 'all)
  "Turns current voice into a monotone and speaks all punctuations.")

(defvoice  voice-monotone-medium (list nil nil 2 2  nil 'all)
  "Turns current voice into a medium monotone.")

(defvoice voice-animate (list nil 7 7 6)
  "Animates current voice.")

(defvoice voice-animate-medium (list nil 6 6  5)
  "Adds medium animation  current voice.")

(defvoice voice-animate-extra (list nil 8 8 7 )
  "Adds extra animation  current voice.")

(defvoice voice-smoothen (list nil nil nil 3 4)
  "Smoothen current voice.")

(defvoice voice-smoothen-extra (list nil nil nil 3 2)
  "Extra smoothen current voice.")

(defvoice voice-smoothen-medium (list nil nil nil 3 3)
  "Add medium smoothen current voice.")

(defvoice voice-brighten-medium (list nil nil nil 5 6)
  "Brighten current voice.")

(defvoice voice-brighten (list nil nil nil 6 7)
  "Brighten current voice.")

(defvoice voice-brighten-extra (list nil nil nil 7 8 )
  "Extra brighten current voice.")

(defvoice voice-bolden (list nil 1 6 6  nil)
  "Bolden current voice.")

(defvoice voice-bolden-extra (list nil 0 6 7 8 )
  "Extra bolden current voice.")

(defvoice voice-bolden-medium (list nil 3 6 6  nil)
  "Add medium bolden current voice.")

(defvoice voice-lighten (list nil 7 7  1  nil)
  "Lighten current voice.")

(defvoice voice-lighten-medium (list nil 6 6 3  nil)
  "Add medium lighten current voice.")

(defvoice voice-lighten-extra (list nil 9 8 7   9)
  "Add extra lighten current voice.")

(defvoice voice-bolden-and-animate (list nil 8 8 8 8 )
  "Bolden and animate  current voice.")

(defvoice voice-womanize-1 (list 'betty 5 nil nil nil nil)
  "Apply first female voice.")

;;}}}
;;{{{  indentation and annotation

(defvoice voice-indent (list nil nil 3 1 3 )
  "Indicate indentation .")

(defvoice voice-annotate (list nil nil 4 0 4)
  "Indicate annotation.")

;;}}}
;;{{{ voice overlays

;;; these are suitable to use as "overlay voices".
(defvoice voice-lock-overlay-0
  (list nil 8 nil nil nil nil)
  "Overlay voice that sets dimension 0 of ACSS structure to 8.")

(defvoice voice-lock-overlay-1
  (list nil nil 8 nil nil nil)
  "Overlay voice that sets dimension 1 of ACSS structure to 8.")

(defvoice voice-lock-overlay-2
  (list nil nil nil 8 nil nil)
  "Overlay voice that sets dimension 2 of ACSS structure to 8.")

(defvoice voice-lock-overlay-3
  (list nil  nil nil nil 8 nil)
  "Overlay voice that sets dimension 3 of ACSS structure to 8.")

;;}}}
;;{{{  Define some voice personalities:
(voice-setup-add-map
 '(
   (bold voice-bolden)
   (bold-italic voice-bolden-and-animate)
   (button voice-bolden)
   (fixed voice-monotone)
   (fixed-pitch voice-monotone)
   (font-lock-builtin-face voice-bolden)
   (font-lock-comment-face voice-monotone)
   (font-lock-comment-delimiter-face voice-monotone-medium)
   (font-lock-regexp-grouping-construct voice-smoothen)
   (font-lock-regexp-grouping-backslash voice-smoothen-extra)
   (font-lock-negation-char-face voice-brighten-extra)
   (font-lock-constant-face voice-lighten)
   (font-lock-doc-face voice-monotone-medium)
   (font-lock-doc-string-face voice-smoothen-extra)
   (font-lock-function-name-face voice-bolden-medium)
   (font-lock-keyword-face voice-animate-extra)
   (font-lock-preprocessor-face voice-monotone-medium)
   (font-lock-reference-face voice-animate-medium)
   (font-lock-string-face voice-lighten-extra)
   (font-lock-type-face voice-smoothen)
   (font-lock-variable-name-face voice-bolden)
   (font-lock-warning-face voice-bolden-and-animate)
   (help-argument-name voice-smoothen)
   (highlight voice-animate)
   (italic voice-animate)
   (match voice-animate)
   (region voice-brighten)
   (underline voice-lighten-medium)
   ))

;;}}}
;;{{{ new light-weight voice lock

(defcustom voice-lock-mode t
  "Determines  if property personality results in text being
voicified."
  :type 'boolean
  :group 'emacspeak)

;;;###autoload
(defun voice-lock-mode (&optional arg)
  "Toggle Voice Lock mode.
With arg, turn Voice Lock mode on if and only if arg is positive.

This light-weight voice lock engine leverages work already done by
font-lock.  Voicification is effective only if font lock is on."
  (interactive "P")
  ;; Don't turn on Voice Lock mode if we don't have a display (we're running a
  ;; batch job) or if the buffer is invisible (the name starts with a space).
  (let ((on-p (and (not noninteractive)
                   (not (eq (aref (buffer-name) 0) ?\ ))
                   (if arg
                       (> (prefix-numeric-value arg) 0)
                     (not voice-lock-mode)))))
    (set (make-local-variable 'voice-lock-mode) on-p)
    ;; Turn on Voice Lock mode.
    (when on-p
      )
    ;; Turn off Voice Lock mode.
    (force-mode-line-update))
  (when (interactive-p)
    (message
     (format "Turned %s voice lock mode"
             (if voice-lock-mode "on" "off")))
    (emacspeak-auditory-icon
     (if voice-lock-mode
         'on 'off ))))
;;;###autoload
(defun turn-on-voice-lock ()
  "Turn on Voice Lock mode ."
  (unless voice-lock-mode (voice-lock-mode)))

;; Install ourselves:
(declaim (special text-property-default-nonsticky))
(unless (assoc 'personality text-property-default-nonsticky)
  (push  (cons 'personality t) text-property-default-nonsticky))

(unless (assq 'voice-lock-mode minor-mode-alist)
  (setq minor-mode-alist (cons '(voice-lock-mode " Voice") minor-mode-alist)))

;;}}}
;;{{{ list-voices-display

(defcustom voice-setup-sample-text
  "Emacspeak --- The Complete Audio Desktop!"
  "Sample text used  when displaying available voices."
  :type 'string
  :group 'voice-fonts)

(defun voice-setup-list-voices (pattern)
  "Show all defined voice-face mappings  in a help buffer.
Sample text to use comes from variable
  `voice-setup-sample-text'. "
  (interactive (list (and current-prefix-arg
                          (read-string "List faces matching regexp: "))))
  (declare (special voice-setup-sample-text
                    list-faces-sample-text))
  (let ((list-faces-sample-text voice-setup-sample-text))
    (list-faces-display pattern)
    (message "Displayed voice-face mappings in other window.")))

;;}}}
;;{{{ interactively silence personalities 
(defvar voice-setup-buffer-face-voice-table (make-hash-table)
  "Hash table used to store buffer local face->personality mappings.")
;;;###autoload
(defun voice-setup-toggle-silence-personality ()
  "Toggle audibility of personality under point  .
If personality at point is currently audible, its
face->personality map is cached in a buffer local variable, and
its face->personality map is replaced by face->inaudible.  If
personality at point is inaudible, and there is a cached value,
then the original face->personality mapping is restored.  In
either case, the buffer is refontified to have the new mapping
take effect."
  (interactive)
  (declare (special voice-setup-buffer-face-voice-table))
  (let* ((personality (get-text-property (point) 'personality))
         (face (get-text-property (point) 'face))
         (orig (gethash face voice-setup-buffer-face-voice-table)))
    (cond
     ((eq personality  'inaudible)
      (voice-setup-set-voice-for-face face  orig)
      (emacspeak-auditory-icon 'open-object))    
     (t (voice-setup-set-voice-for-face face 'inaudible)
        (setf
         (gethash face voice-setup-buffer-face-voice-table)
         personality)
        (emacspeak-auditory-icon 'close-object)))
    (normal-mode)))

;;}}}
(provide 'voice-setup)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
