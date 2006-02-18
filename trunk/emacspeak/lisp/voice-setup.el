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

;;;Copyright (C) 1995 -- 2004, T. V. Raman 
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

;;; How faces map to voices: TTS engine specific modules e.g.,
;;; dectalk-voices.el and outloud-voices.el define a standard set
;;; of voice names.  This module maps standard "personality"
;;; names to these pre-defined voices.  It does this via special
;;; form def-voice-font which takes a personality name, a voice
;;; name and a face name to set up the mapping between face and
;;; personality, and personality and voice.  See many instances
;;; of this usage in this module.  This special form is available
;;; for use from other emacspeak modules.

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
(require 'dectalk-voices)
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
;;{{{ elib:string:string-replace-match

;;; copied over from elib/string.el so we have it 

(defun elib-string-expand-newtext ()
  (declare (special string newtext))
  ;; Expand \& and \1..\9 (referring to STRING) in NEWTEXT.
  ;; Uses match data and fluid vars `newtext', `string'.
  ;; Note that in Emacs 18 match data are clipped to current buffer
  ;; size...so the buffer should better not be smaller than STRING.
  (let ((pos 0)
        (len (length newtext))
        (expanded-newtext ""))
    (while (< pos len)
      (setq expanded-newtext
            (concat expanded-newtext
                    (let ((c (aref newtext pos)))
                      (if (= ?\\ c)
                          (cond ((= ?\& (setq c (aref newtext
                                                      (setq pos (1+ pos)))))
                                 (substring string
                                            (match-beginning 0)
                                            (match-end 0)))
                                ((and (>= c ?1) 
                                      (<= c ?9))
                                 ;; return empty string if N'th
                                 ;; sub-regexp did not match:
                                 (let ((n (- c ?0)))
                                   (if (match-beginning n)
                                       (substring string
                                                  (match-beginning n)
                                                  (match-end n))
                                     "")))
                                (t (char-to-string c)))
                        (char-to-string c)))))
      (setq pos (1+ pos)))
    expanded-newtext))

;; This function is a near-equivalent of the elisp function replace-match
;; which work on strings instead of a buffer.  The FIXEDCASE parameter
;; of replace-match is not implemented.

(defun string-replace-match (regexp string newtext &optional literal global)
  "Replace first match of REGEXP in STRING with NEWTEXT.
If no match is found, nil is returned instead of the new string.

Optional arg LITERAL non-nil means to take NEWTEXT literally. If LITERAL is 
nil, character `\\' is the start of one of the following sequences:
  \\\\   will be replaced by a single \\.
  \\&   will be replaced by the text which matched the regexp.
  \\N   where N is a number and 1 <= N <= 9, will be replaced
       by the Nth subexpression in REGEXP. Subexpressions are grouped
       inside \\( \\).

Optional arg GLOBAL means to replace all matches instead of only the first."

  (let ((data (match-data)))
    (unwind-protect

        (if global
            (let ((result "") 
                  (start 0)
                  matchbeginning
                  matchend)
              (while (string-match regexp string start)
                (setq matchbeginning (match-beginning 0)
                      matchend (match-end 0)
                      result (concat result
                                     (substring string start matchbeginning)
                                     (if literal
                                         newtext
                                       (elib-string-expand-newtext)))
                      start matchend))

              (if matchbeginning        ; matched at least once
                  (concat result (substring string start))
                nil))

          ;; not GLOBAL
          (if (not (string-match regexp string 0))
              nil
            (concat (substring string 0 (match-beginning 0))
                    (if literal newtext (elib-string-expand-newtext))
                    (substring string (match-end 0)))))
      (store-match-data data))))

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

(defun voice-setup-map-face (face voice)
  "Invoke def-voice-font with appropriately generated personality name."
  (let ((doc (format "Personality used for %s" face))
        (personality nil)
        (name nil))
          (setq name 
                  (or
                   (string-replace-match "face$"
                                         (symbol-name face)
                                         "personality")
                   (symbol-name face)))
          (setq name 
                  (or
                   (string-replace-match "font"
                                         name
                                         "voice")
                   name))
          (setq personality (intern name))
    (eval
     `(def-voice-font ,personality ,voice  ',face  ,doc))))

(defun voice-setup-add-map (fv-alist)
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
;;; update all observers                ; ; ; ; ; ; ; ; ;
          (voice-setup-update-personalities '(, personality))
          (set-default sym val))))))

;;}}}                                   ; ; ; ; ; ; ; ; ; ;
;;{{{ voices defined using ACSS         ; ; ;

;;; these voices are device independent ; ; ;

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
  "Extra smoothen current voice.")

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

(def-voice-font voice-lock-highlight-personality voice-bolden
  'match
  "Personality used for  matched text.")

(def-voice-font voice-lock-highlight-personality voice-bolden
  'highlight
  "Personality used for highlighting text.")

(def-voice-font voice-lock-highlight-personality voice-animate
  'highlight-face
  "Personality used for highlighting text.")

(def-voice-font voice-lock-region-personality voice-brighten
  'region
  "Personality used for highlighting region.")
(def-voice-font voice-lock-button-personality voice-bolden
  'button
  "Personality used for button text.")
(def-voice-font voice-lock-fixed-personality voice-monotone
  'fixed
  "Personality to use for fixed pitch  text.")

(def-voice-font voice-lock-fixed-pitch-personality voice-monotone
  'fixed-pitch
  "Personality to use for fixed pitch  text.")

(def-voice-font voice-lock-comment-personality voice-monotone
  'font-lock-comment-face
  "Personality to use for comments.")

(def-voice-font voice-lock-doc-personality voice-monotone-medium
  'font-lock-doc-face
  "Personality to use for documentation.")

          
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
  "Personality to use for bold  italic text.")

  
(def-voice-font voice-lock-doc-string-personality
  voice-smoothen-extra
  'font-lock-doc-string-face
  "Personality to use for documentation strings.")
  
(def-voice-font voice-lock-constant-personality voice-lighten
  'font-lock-constant-face
  "Personality to use for  constants.")
  
(def-voice-font voice-lock-string-personality voice-lighten-extra
  'font-lock-string-face
  "Personality to use for string constants.")

(def-voice-font voice-lock-function-name-personality voice-bolden-medium
  'font-lock-function-name-face
  "Personality to use for function names.")
  
(def-voice-font voice-lock-warning-personality voice-bolden-and-animate
  'font-lock-warning-face
  "Personality to use for warnings.")

(def-voice-font voice-lock-preprocessor-personality voice-monotone-medium
  'font-lock-preprocessor-face
  "Personality to use for preprocessor directives.")

(def-voice-font voice-lock-keyword-personality voice-animate-extra
  'font-lock-keyword-face
  "Personality to use for keywords.")
  
(def-voice-font voice-lock-builtin-personality voice-bolden
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
(provide 'voice-setup)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
(def-voice-font voice-lock-button-personality voice-bolden
  'button
  "Personality for buttons.")

(def-voice-font voice-lock-gui-button-personality voice-bolden
  'gui-button
  "Personality for buttons.")
