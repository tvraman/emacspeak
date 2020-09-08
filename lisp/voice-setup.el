;;; voice-setup.el --- Setup voices for voice-lock  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Voice lock mode for Emacspeak
;;{{{  LCD Archive entry:
;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-09-01 15:30:13 -0700 (Sat, 01 Sep 2007) $ |
;;;  $Revision: 4672 $ |
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
;;{{{ Introduction

;;; Commentary:

;;; A voice is to audio as a font is to a visual display.
;;; A personality is to audio as a face is to a visual display.
;;;
;; Voice-lock-mode is a minor mode that causes your comments to be
;; spoken in one personality, strings in another, reserved words in another,
;; documentation strings in another, and so on.
;;
;; Comments will be spoken in `voice-comment-personality'.
;; Strings will be spoken in `voice-string-personality'.
;; Function and variable names (in their defining forms) will be
;;  spoken in `voice-function-name-personality'.
;; Reserved words will be spoken in `voice-keyword-personality'.
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
;;; voice-setup-add-map. For use from other modules, also see
;;; function voice-setup-map-face which is useful when mapping a
;;; single face.
;;; Both voice-setup-add-map and voice-setup-map-face call
;;; special form def-voice-font.

;;; Special form def-voice-font sets up the personality name to
;;; be available via custom.  new voices can be defined using CSS
;;; style specifications see special form defvoice Voices defined
;;; via defvoice can be customized via custom see the
;;; documentation for defvoice.
;;; Code:

;;}}}
;;{{{ Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(eval-when-compile (require 'easy-mmode))
(require 'custom)
(require 'acss-structure)
(require 'tts)
(require 'outloud-voices)
(require 'mac-voices)
(require 'espeak-voices)
(require 'dectalk-voices)
(require 'emacspeak-sounds)
;;}}}
;;{{{ customization group

(defgroup voice-fonts nil
  "Customization group for setting voices."
  :group 'emacspeak)

(declare-function tts-list-voices "dectalk-voices")

;;}}}
;;{{{  helper for voice custom items:
(unless (fboundp 'tts-list-voices)
  (fset 'tts-list-voices #'dectalk-list-voices))

(defun voice-setup-custom-menu ()
  "Return a choice widget used in selecting voices."
  `(choice
    (symbol :tag "Other")
    ,@(mapcar 
       #'(lambda (voice)(list 'const voice))
       (tts-list-voices))))

(defun voice-setup-read-personality (&optional prompt)
  "Read name of a pre-defined personality using completion."
  (read (completing-read (or prompt "Personality: ")
                         (tts-list-voices))))

;;}}}
;;{{{ map faces to voices

(defvar voice-setup-face-voice-table (make-hash-table)
  "Hash table holding face to voice mapping.")

(defun voice-setup-set-voice-for-face (face voice)
  "Map face --a symbol-- to relevant voice."
  (cl-declare (special  voice-setup-face-voice-table))
  (setf (gethash face voice-setup-face-voice-table) voice))

(defun voice-setup-get-voice-for-face (face)
  "Map face --a symbol-- to relevant voice."
  (cl-declare (special  voice-setup-face-voice-table))
  (gethash face voice-setup-face-voice-table))

(defun voice-setup-show-rogue-faces ()
  "Return list of voices that map to non-existent faces."
  (cl-declare (special voice-setup-face-voice-table))
  (cl-loop for f being the hash-keys of voice-setup-face-voice-table
           unless (facep f) collect f))

;;}}}
;;{{{ special form def-voice-font

;;; note that when defined, personalities are registered as
;;; observers with the voice they use this gets unregistered when
;;; the mapping is changed via custom.

(defmacro  def-voice-font (personality voice face doc &rest args)
  "Define personality and map it to specified face."
  (declare (indent 1) (debug t))
  (let ((documentation
         (concat
          doc
          (format "\nThis personality uses  %s whose  effect can be changed globally by customizing %s-settings."
                  voice  voice))))
    `(progn
       (unless (boundp ',personality)
;;; New Personality
         (defcustom  ,personality
           ,voice
           ,documentation
           :type (voice-setup-custom-menu)
           :group 'voice-fonts
           :set '(lambda (sym val)
                   (let ((observing  (get sym 'observing)))
                     (when (and (symbolp sym)
                                (symbolp observing))
                       (cl-remprop observing sym))
                     (set-default sym val)))
           ,@args))
;;; other actions performed at define time
       (voice-setup-set-voice-for-face ,face ',personality)
;;;record  personality as an
;;;observer of  voice and vice versa
       (when (symbolp ',personality)
         (put  ',personality 'observing ',voice))
       (when (symbolp ',voice)
         (put  ',voice ',personality t)))))

(defun voice-setup-name-personality (face-name)
  "Compute personality name to use."
  (let ((name nil))
    (setq name
          (or
           (replace-regexp-in-string "face$" "personality" face-name)
           face-name))
    (setq name
          (or
           (replace-regexp-in-string "font-lock" "voice" name
                                     (replace-regexp-in-string "font" "voice" name))
           name))
    (setq name
          (or
           (replace-regexp-in-string "font" "voice" name)
           name))
    (when (string-equal name face-name)
      (setq name (format "voice-%s" name)))
    name))

(defun voice-setup-map-face (face voice)
  "Invoke def-voice-font with appropriately generated personality name."
  (let ((doc (format "Personality used for %s" face))
        (personality
         (intern (voice-setup-name-personality (symbol-name face)))))
    (eval
     `(def-voice-font ,personality ,voice  ',face  ,doc))))

(defun voice-setup-add-map (fv-alist)
  "Sets up face to voice mapping given in fv-alist."
  (cl-loop
   for fv in fv-alist
   do
   (voice-setup-map-face (cl-first fv) (cl-second fv))))

;;}}}
;;{{{  special form defvoice

(defvar voice-setup-personality-table (make-hash-table)
  "Maps personality names to ACSS  settings.
Keys are personality names.")

(defun voice-setup-personality-from-style (style-list)
  "Define a personality given a list of speech style settings."
  (cl-declare (special voice-setup-personality-table))
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

(defun voice-setup-observing-personalities  (voice-name)
  "Return a list of personalities that are `observing' VOICE-NAME.
Observing personalities are automatically updated when settings for
VOICE-NAME are  changed."
  (let* ((plist (symbol-plist voice-name))
         (l (1- (length plist))))
    (cl-loop for i from 0 to l by 2
             collect (nth i plist))))

(defun voice-setup-update-personalities (personality)
  "Update  personalities  that use this voice to  new setting."
  (let ((value (symbol-value personality))
        (observers (voice-setup-observing-personalities personality)))
    (cl-loop for o in observers
             do                            ;o is already quoted
             (set o value))))

;;; note that for now we dont use  gain settings

(defmacro defvoice (personality settings doc)
  "Define voice using CSS setting.  Setting is a list of the form
(list paul 5 5 5 5 'all) which defines a standard male voice
that speaks `all' punctuations.  Once
defined, the newly declared personality can be customized by calling
command \\[customize-variable] on <personality>-settings. "
  (declare (indent 1) (debug t))
  `(progn
     (defvar  ,personality
       (voice-setup-personality-from-style ,settings)
       ,(concat
         doc
         (format "Customize this overlay via %s-settings."
                 personality)))
     (defcustom ,(intern (format "%s-settings"  personality))
       ,settings
       ,doc
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
            (setq ,personality voice-name)
;;; update all observers
            (voice-setup-update-personalities ',personality)
            (set-default sym val))))))

;;}}}                                   ; ; ; ;
;;{{{ voices defined using ACSS         

;;; these voices are device independent 

(defvoice  voice-punctuations-all (list nil nil nil nil  nil 'all)
  "Turns current voice into one that  speaks all punctuations.")

(defvoice  voice-punctuations-some (list nil nil nil nil  nil 'some)
  "Turns current voice into one that  speaks some punctuations.")

(defvoice  voice-punctuations-none (list nil nil nil nil  nil "none")
  "Turns current voice into one that  speaks no punctuations.")

(defvoice  voice-monotone (list nil nil 0 0 nil 'all)
  "Turns current voice into a monotone and speaks all punctuations.")

(defvoice  voice-monotone-medium (list nil nil 2 2  nil 'all)
  "Turns current voice into a medium monotone.")

(defvoice  voice-monotone-light (list nil nil 4 4   nil 'all)
  "Turns current voice into a light monotone.")

(defvoice voice-animate-extra (list nil 8 8 6)
  "Adds extra animation  to current voice.")

(defvoice voice-animate (list nil 7 7 4)
  "Animates current voice.")

(defvoice voice-animate-medium (list nil 6 6  3)
  "Adds medium animation  current voice.")

(defvoice voice-smoothen-extra (list nil nil nil 4 5)
  "Extra smoothen current voice.")

(defvoice voice-smoothen-medium (list nil nil nil 3 4)
  "Add medium smoothen current voice.")

(defvoice voice-smoothen (list nil nil  2 2)
  "Smoothen current voice.")

(defvoice voice-brighten-medium (list nil nil nil 5 6)
  "Brighten  (medium) current voice.")

(defvoice voice-brighten (list nil nil nil 6 7)
  "Brighten current voice.")

(defvoice voice-brighten-extra (list nil nil nil 7 8)
  "Extra brighten current voice.")

(defvoice voice-bolden (list nil 3 6 6  6)
  "Bolden current voice.")

(defvoice voice-bolden-medium (list nil 2 6 7  7)
  "Add medium bolden current voice.")

(defvoice voice-bolden-extra (list nil 1 6 7 8)
  "Extra bolden current voice.")

(defvoice voice-lighten (list nil 6 6 2   nil)
  "Lighten current voice.")

(defvoice voice-lighten-medium (list nil 7 7 3  nil)
  "Add medium lightness to  current voice.")

(defvoice voice-lighten-extra (list nil 9 8 7   nil)
  "Add extra lightness to  current voice.")

(defvoice voice-bolden-and-animate (list nil 3 8 8 8)
  "Bolden and animate  current voice.")

(defvoice voice-womanize-1 (list 'betty 5 nil nil nil nil)
  "Apply first female voice.")

;;}}}
;;{{{  indentation and annotation

(defvoice voice-indent (list nil nil 3 1 3)
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
   (variable-pitch voice-animate)
   (shr-link voice-bolden)
   (bold voice-bolden)
   (bold-italic voice-bolden-and-animate)
   (button voice-bolden-medium)
   (link voice-bolden-medium)
   (link-visited voice-bolden)
   (success voice-brighten-extra)
   (error voice-animate)
   (warning voice-smoothen)
   (fixed-pitch voice-monotone)
   (font-lock-builtin-face voice-bolden)
   (font-lock-comment-face voice-monotone)
   (font-lock-comment-delimiter-face voice-smoothen-medium)
   (font-lock-regexp-grouping-construct voice-smoothen)
   (font-lock-regexp-grouping-backslash voice-smoothen-extra)
   (font-lock-negation-char-face voice-brighten-extra)
   (font-lock-constant-face voice-lighten)
   (font-lock-doc-face voice-monotone-medium)
   (font-lock-function-name-face voice-bolden-medium)
   (font-lock-keyword-face voice-animate-extra)
   (font-lock-preprocessor-face voice-monotone-medium)
   (shadow voice-monotone-medium)
   (file-name-shadow voice-monotone-medium)
   (fringe voice-monotone-medium)
   (secondary-selection voice-lighten-medium)
   (font-lock-string-face voice-lighten-extra)
   (font-lock-type-face voice-smoothen)
   (font-lock-variable-name-face voice-bolden-extra)
   (font-lock-warning-face voice-bolden-and-animate)
   (help-argument-name voice-smoothen)
   (query-replace voice-bolden)
   (match voice-lighten)
   (isearch voice-bolden)
   (isearch-fail voice-monotone)
   (highlight voice-animate)
   (italic voice-animate)
   (match voice-animate)
   (region voice-brighten)
   (underline voice-lighten-extra)
   ))

;;}}}
;;{{{ new light-weight voice lock

;;;###autoload
(define-minor-mode voice-lock-mode
  "Toggle voice lock mode."
  t nil nil
  (when (called-interactively-p 'interactive)
    (let ((state (if voice-lock-mode 'on 'off)))
        (emacspeak-auditory-icon state))))

(defun voice-lock-mode--turn-on ()
  "Turn on Voice Lock mode ."
  (interactive)
  (voice-lock-mode))
;;;###autoload
(define-globalized-minor-mode global-voice-lock-mode
  voice-lock-mode
  voice-lock-mode--turn-on
  :init-value t
  :group 'voice-lock
  (when (called-interactively-p 'interactive)
    (let ((state (if global-voice-lock-mode 'on 'off)))
        (emacspeak-auditory-icon state)))
  )

;; Install ourselves:
(cl-declaim (special text-property-default-nonsticky))
(unless (assq 'personality text-property-default-nonsticky)
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
  (cl-declare (special voice-setup-sample-text))
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
  (cl-declare (special voice-setup-buffer-face-voice-table))
  (let* ((personality  (dtk-get-style))
         (face (get-text-property (point) 'face))
         (orig (gethash face voice-setup-buffer-face-voice-table)))
    (cond
     ((null personality) (message "No personality here."))
     ((eq personality  'inaudible)
      (voice-setup-set-voice-for-face face  orig)
      (message "Made personality %s audible." orig)
      (emacspeak-auditory-icon 'open-object))    
     (t (voice-setup-set-voice-for-face face 'inaudible)
        (setf
         (gethash face voice-setup-buffer-face-voice-table)
         personality)
        (message "Silenced personality %s" personality)
        (emacspeak-auditory-icon 'close-object)))
    (when (buffer-file-name) (normal-mode))))

;;}}}
;;{{{ Helper: voice-setup-defined-voices 

(defun voice-setup-defined-voices ()
  "Return list of voices defined via defvoice."
  (let ((result nil))
    (mapatoms
     #'(lambda (s)
         (when  
             (and
              (string-match "^voice-"  (symbol-name s))
              (boundp s)
              (symbolp (symbol-value s))
              (string-match  "^acss-" (symbol-name  (symbol-value s))))
           (push s result))))
    result))

;;}}}
;;{{{ describe-voice at point:

(defun voice-setup-describe-personality(personality)
  "Describe specified voice --- analogous to \\[describe-face].
When called interactively, `personality' defaults to first personality at point.
If there are multiple personalities at point,
these are available via minibuffer history."
  (interactive
   (list
    (let* ((v (dtk-get-style)))
      (setq v
            (if (listp v)
                (mapcar #'symbol-name v)
              (symbol-name v)))
      (when (listp v) (setq v (cl-first v)))
      (read-from-minibuffer
       "Personality: "
       nil nil 'read nil  v))))
  (let ((voice (get personality 'observing))
        (settings nil)
        (n '(family average-pitch pitch-range stress richness punctuations))
        (values nil))
    (when voice (setq settings (intern (format "%s-settings" voice))))
    (cond
     ((symbol-value settings) ;;; globally bound, display it
      (setq values (symbol-value settings))
      (with-help-window (help-buffer)
        (with-current-buffer standard-output
          (insert (format "Personality: %s\tVoice:%s\n\n" personality voice))
          (put-text-property (point-min) (point)
                             'personality personality)
          (cl-loop
           for i from 0 to (1- (length n))do
           (insert (format "%s: %s\n"
                           (elt n i) (elt values i))))))
      (when (called-interactively-p 'interactive)
        (emacspeak-speak-help)))
     (t (message "%s doesn't look like a valid personality." personality)))))

;;}}}
(provide 'voice-setup)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
