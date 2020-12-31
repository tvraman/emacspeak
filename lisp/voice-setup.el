;;; voice-setup.el --- Setup voices for voice-lock  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Voice lock mode for Emacspeak
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
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
;;; Voice-lock-mode is a minor mode that causes your comments to be
;;; spoken in one personality, strings in another, reserved words in another,
;;; documentation strings in another, and so on.
;;;
;;; Comments will be spoken in `voice-comment-personality'.
;;; Strings will be spoken in `voice-string-personality'.
;;; Function  (in their defining forms) will be
;;;  spoken in `voice-function-name-personality'.
;;; Reserved words will be spoken in `voice-keyword-personality'.
;;;
;;; To audio-format  text , use M-x voice-lock-mode.
;;; When this minor mode is on, the voices of the current line are
;;; updated with every insertion or deletion.
;;;
;;; @subsection Voice-Lock And Aural CSS
;;; The CSS Speech Style Sheet specification defines a number of
;;; abstract device independent voice properties.
;;; A setting conforming to the CSS speech specification can be
;;; represented in elisp as a structure.
;;;
;;; We will refer to this structure as a "speech style".  This
;;; structure needs to be mapped to device dependent codes to produce
;;; the desired effect.  This module forms a bridge between emacs
;;; packages    that wish to implement audio formatting 
;;; and Emacspeak's TTS module.  Emacspeak produces voice
;;; change effects by examining the value of text-property
;;; 'personality', as well as the face/font at point.
;;;
;;; Think of a buffer of formatted text along with the text-property
;;; 'personality appropriately set as a "aural display list".  Module
;;; voice-setup.el help applications like EWW produce audio-formatted
;;; output by calling function acss-personality-from-speech-style with
;;; a "speech-style" --a structure as defined in this module and get
;;; back a symbol that they assign to the value of property
;;; 'personality.  Emacspeak's rendering engine then does the needful
;;; at the time speech is produced.  Function
;;; acss-personality-from-speech-style does the following: Takes as
;;; input a "speech style" (1) Computes a symbol that will be used to
;;; refer to this specific speech style.  (2) Examines emacspeak's
;;; internal voice table to see if this speech style has a voice
;;; already defined.  If so it returns immediately.  Otherwise, it
;;; does the additional work of defining a -voice for future use.  See
;;; its use in this module to see how voices are defined independent
;;; of a given TTS engine.  How faces map to voices: TTS engine
;;; specific modules e.g., dectalk-voices.el and outloud-voices.el map
;;; ACSS dimensions to engine-specific codes. It does this via special
;;; form def-voice-font which takes a personality name, a voice name
;;; and a face name to set up the mapping between face and
;;; personality, and personality and voice.  Emacspeak modules use
;;; voice-setup-add-map when defining face->personality mappings.  For
;;; use from other modules, also see function voice-setup-map-face
;;; which is useful when mapping a single face.  Both
;;; voice-setup-add-map and voice-setup-map-face call special form
;;; def-voice-font.
;;;
;;;   new voices can be defined using CSS styles,
;;;  see special form defvoice. Voices defined
;;; via defvoice can be customized via custom see the
;;; documentation for defvoice.

;;; Code:

;;}}}
;;{{{ Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(eval-when-compile (require 'easy-mmode))

;;}}}
;;{{{ customization group

(defgroup voice-fonts nil
  "Voices"
  :group 'emacspeak)

;;}}}
;;{{{Configure:

;;; This configures Emacspeak for the TTS engine used at start.
;;; Subsequent switches to other engines  causes that engine to get
;;; configured --- see the various tts-engine startup  commands, e.g.,
;;; outloud, dectalk, espeak.
;;; Whenever we switch engines, we load voice-definitions for that
;;; engine by reloading module voice-defs.


(cond
   ((string-match "outloud" dtk-program)
    (require 'outloud-voices)
    (outloud-configure-tts))
   ((string-match "dtk" dtk-program)
    (require 'dectalk-voices)
    (dectalk-configure-tts))
   ((string-match "mac$" dtk-program)
    (require 'mac-voices)
    (mac-configure-tts))
   ((string-match "espeak$" dtk-program)
    (require 'espeak-voices)
    (espeak-configure-tts))
   (t
    (require 'plain-voices)
    (plain-configure-tts)))

(defun acss-personality-from-speech-style (style)
  "First compute a symbol that will be name for this STYLE.
Define a voice for it if needed, then return the symbol."
  (cond
   ((and (acss-gain style) (= 0 (acss-gain style))) 'inaudible)
   (t
    (let ((f (acss-family style))
          (a (acss-average-pitch style))
          (p (acss-pitch-range style))
          (s (acss-stress style))
          (r (acss-richness style))
          (m (acss-punctuations style))
          (name nil))
      (setq name 
            (intern
             (format "acss%s%s%s%s%s%s"
                     (if f (format "-%s" f) "")
                     (if a (format "-a%s" a) "")
                     (if p (format "-p%s" p) "")
                     (if s (format "-s%s" s) "")
                     (if r (format "-r%s" r) "")
                     (if m (format "-%s" m) ""))))
      (unless (tts-voice-defined-p name)
        (tts-define-voice-from-speech-style name style))
      name))))

(defun tts-list-voices ()
  "List  voices."
  (cl-declare (special dectalk-voice-table espeak-voice-table
                       plain-voice-table mac-voice-table
                       outloud-voice-table dtk-program))
  (let ((voice-table
          (cond
           ((string-match "outloud" dtk-program) outloud-voice-table)
           ((string-match "dtk" dtk-program) dectalk-voice-table)
           ((string-match "mac$" dtk-program) mac-voice-table)
           ((string-match "espeak$" dtk-program) espeak-voice-table)
           (t plain-voice-table))))
    (cl-loop for k being the hash-keys of voice-table collect   k)))

;;}}}
;;{{{ map faces to voices

(defvar voice-setup-face-voice-table (make-hash-table :test #'eq)
  "Face to voice mapping.")

(defsubst voice-setup-set-voice-for-face (face voice)
  "Map face  to  voice."
  (cl-declare (special  voice-setup-face-voice-table))
  (setf (gethash face voice-setup-face-voice-table) voice))

(defsubst voice-setup-get-voice-for-face (face)
  "Return face to  voice."
  (cl-declare (special  voice-setup-face-voice-table))
  (gethash face voice-setup-face-voice-table))

;;}}}
;;{{{ special form def-voice-font

;;; note that when defined, personalities are registered as
;;; observers with the voice they use this gets unregistered when
;;; the mapping is changed via custom.

(defmacro  def-voice-font (personality voice face doc)
  "Define personality and map it to face."
  (declare (indent 1) (debug t))
  (let ((documentation
         (concat
          doc
          (format "\nUses %s: Customize via %s-settings."
                  voice  voice))))
    `(progn
       (unless (boundp ',personality)
;;; New Personality
         (defvar  ,personality
           ,voice
           ,documentation))
;;; other actions performed at define time
       (voice-setup-set-voice-for-face ,face ',personality)
;;;record  personality as an
;;;observer of  voice 
       (when (symbolp ',personality)
         (put  ',personality 'observing ',voice))
       (when (symbolp ',voice)
         (put  ',voice ',personality t)))))

(defun voice-setup-name-personality (face-name)
  "Get personality name to use."
  (with-temp-buffer 
    (erase-buffer)
    (insert face-name)
    (let ((orig face-name))
      (goto-char (point-min))
      (while (search-forward "face" nil t) (replace-match "personality"))
      (goto-char (point-min))
      (while (search-forward "font-lock" nil t) (replace-match "voice"))
      (when (string= orig (buffer-string))
        (insert "voice-")))
    (buffer-string)))

(defun voice-setup-map-face (face voice)
  "Invoke def-voice-font with  generated personality name."
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

(defvar voice-setup-style-table (make-hash-table :test #'eq)
  "Maps ACSS names to ACSS  settings. ")

(defun voice-setup-acss-from-style (style-list)
  "Define an ACSS-voice  given a list of speech style settings."
  (cl-declare (special voice-setup-style-table))
  (let ((voice
         (acss-personality-from-speech-style
          (make-acss
           :family (nth 0 style-list)
           :average-pitch (nth 1 style-list)
           :pitch-range (nth 2 style-list)
           :stress (nth 3 style-list)
           :richness (nth 4  style-list)
           :punctuations (nth 5  style-list)))))
    (puthash  voice style-list voice-setup-style-table)
    voice))

(defun voice-setup-observing-personalities  (voice-name)
  "Return a list of personalities that are `observing' VOICE-NAME. "
  (let* ((plist (symbol-plist voice-name))
         (l (1- (length plist))))
    (cl-loop for i from 0 to l by 2
             collect (nth i plist))))

(defun voice-setup-update-personalities (personality)
  "Update  personalities  whose  setting just changed."
  (let ((value (symbol-value personality))
        (observers (voice-setup-observing-personalities personality)))
    (cl-loop for o in observers
             do                            ;o is already quoted
             (set o value))))

(defmacro defvoice (personality settings doc)
  "Define voice using ACSS setting.  Setting is a list of the form
(list paul 5 5 5 5 'all) which defines a standard male voice
that speaks `all' punctuations.  Once
defined, the newly declared personality can be customized by calling
command \\[customize-variable] on <personality>-settings. "
  (declare (indent 1) (debug t))
  `(progn
     (defvar  ,personality
       (voice-setup-acss-from-style ,settings)
       ,(concat
         doc
         (format "\nCustomize  via %s-settings."
                 personality)))
     (defcustom ,(intern (format "%s-settings"  personality))
       ,settings
       ,doc
       :type  '(list
                        (const :tag "Unspecified" nil)
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
       #'(lambda  (sym val)
          (let ((acss-name (voice-setup-acss-from-style val)))
            (setq ,personality acss-name)
;;; update all observers
            (voice-setup-update-personalities ',personality)
            (set-default sym val))))))

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
;;{{{ interactively silence personalities 

(defvar voice-setup-buffer-face-voice-table (make-hash-table :test #'eq)
  "Buffer local face->personality.")

;;; If personality at point is currently audible, its
;;; face->personality map is cached in a buffer local variable, and
;;; its face->personality map is replaced by face->inaudible.  If
;;; personality at point is inaudible, and there is a cached value,
;;; then the original face->personality mapping is restored.  In
;;; either case, the buffer is refontified to have the new mapping take effect.

;;;###autoload
(defun voice-setup-toggle-silence-personality ()
  "Toggle audibility of personality under point  . "
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
(provide 'voice-setup)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
