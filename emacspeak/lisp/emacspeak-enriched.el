;;; emacspeak-enriched.el --- Audio Formatting for Emacs' WYSIWYG RichText  mode
;;; $Id$
;;; $Author$ 
;;; Description: Emacspeak module to speak voicify rich text
;;; Keywords:emacspeak, audio interface to emacs rich text
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
;;; Copyright (c) 1995 by T. V. Raman  
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

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'dtk-css-speech)
(require 'emacspeak-sounds)
(require 'emacspeak-speak)
;;{{{  Introduction

;;; emacspeak extensions to voicify rich  text.

;;}}}
;;{{{ voicify-faces 
(defvar emacspeak-enriched-font-faces-to-voiceify
  (list 'bold 'italic   'bold-italic 'underlined)
  "List of font faces we voiceify")


(defun emacspeak-enriched-voiceify-faces (start end)
  "Map base fonts to voices.
Useful in voicifying rich text."
  (interactive "r")
  (declare (special
            emacspeak-enriched-font-faces-to-voiceify))
  (set (make-local-variable 'voice-lock-mode) t)
  (ems-modify-buffer-safely
   (save-excursion
     (goto-char start)
     (let ((face nil )
           (orig start)
           (pos nil)
           (justification-type nil))
       (unless (get-text-property (point) 'justification)
         (goto-char
          (or
           (next-single-property-change (point) 'justification
                                        (current-buffer) end)
           end)))
       (while (and  (not (eobp))
                    (< start end))
         (setq justification-type (get-text-property (point) 'justification))
         (save-excursion
           (beginning-of-line)
           (setq pos (point)))
         (goto-char
          (or
           (next-single-property-change (point) 'justification
                                        (current-buffer) end)
           end))
         (when justification-type
           (put-text-property pos (point)
                              'auditory-icon
                              justification-type))
         (setq start (point)))
       (goto-char orig)
       (while (and  (not (eobp))
                    (< start end))
         (setq face (get-text-property (point) 'face ))
         (goto-char
          (or
           (next-single-property-change (point) 'face
                                        (current-buffer) end)
           end))
         (when face 
           (put-text-property start  (point)
                              'personality
                              (if (listp face)
                                  (loop for f in emacspeak-enriched-font-faces-to-voiceify
                                        thereis (find f face))
                                face )))
         (setq start (point))))))
  (message "voicified faces"))

;;}}}
;;{{{ advice enriched to automatically map faces to voices

(defadvice enriched-decode (after emacspeak pre act comp)
  "Map faces to voices. "
  (let ((start (ad-get-arg 0))
        (end (ad-get-arg 1)))
    (emacspeak-enriched-voiceify-faces start end )
    ad-return-value))

(defadvice enriched-mode (after emacspeak pre act comp)
  "Map faces to voices. "
  (cond
   (enriched-mode
       (emacspeak-enriched-voiceify-faces (point-min) (point-max))))
  ad-return-value)

;;}}}
;;{{{ hooks
(add-hook 'enriched-mode-hook
          (function
           (lambda ()
             (or emacspeak-audio-indentation
                 (emacspeak-toggle-audio-indentation)))))
;;}}}
(provide  'emacspeak-enriched)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
