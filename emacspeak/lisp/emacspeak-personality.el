;;; emacspeak-personality.el ---Emacspeak's new personality interface
;;; $Id$
;;; $Author$
;;; Description:  Contains the functions for speaking various chunks of text
;;; Keywords: Emacspeak,  Spoken Output
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  Introduction:

;;; Commentary:

;;; This module defines a personality interface for implementing voice
;;; lock via font lock.
;;; Context:

;;; At the time I implemented Emacspeak's voice lock feature in late
;;; 1994, font-lock was still evolving. Most packages that supported
;;; The font-lock module explicitly checked for windowing system and
;;; became active only when Emacs was running under a windowing
;;; system.
;;; Since I wanted emacspeak  to work both within and outside X, and I
;;; did not want to change any of Emacs' code, I implemented
;;; voice-lock  as a separate module.
;;; This also kept things stable as font-lock itself evolved and
;;; changed.


;;; 8 years later, font-lock is now stable.
;;; It is also active outside windowing systems, since Emacs can now
;;; colorize terminals.
;;; This module when complete will simplify the voice-lock code in
;;; Emacspeak by triggering voice locking directly from within the
;;; font-lock code.

;;; Emacspeak modules will still be able to voice lock independent of
;;; visual characteristics --this was a key goal of the original
;;; Emacspeak design and it will be preserved going forward.

;;; As I implement this module, I also plan to fix 
;;; one of the shortcomings in the present voice lock architecture,
;;; where 
;;; the value of property 'personality is a symbol.
;;; This makes it hard to apply voice lock properties cumulatively.
;;; When this update is complete, the Emacspeak core in module
;;; dtk-tcl.el ---function dtk-format-text-and-speak
;;; will be updated to handle the case where property 'personality
;;; holds either a symbol or a list.

;;; Finally, I may add better support for overlays --again this was a
;;; part of Emacs that was at its nascent stage in 1994, but is now
;;; stable.

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'custom)
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
(require 'advice)
(require 'voice-setup)
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

;; Install ourselves:
(declaim (special text-property-default-nonsticky))
(unless (assoc 'personality text-property-default-nonsticky)
  (push  (cons 'personality t) text-property-default-nonsticky))

(unless (assq 'voice-lock-mode minor-mode-alist)
  (setq minor-mode-alist (cons '(voice-lock-mode " Voice") minor-mode-alist)))


;;}}}
;;{{{ cumulative personalities 

(defun emacspeak-personality-append  (start end personality
                                            &optional object )
  "Append specified personality to text bounded by start and end.
Existing personality properties on the text range are preserved."
  (ems-modify-buffer-safely
   (let ((orig (get-text-property start 'personality))
         (new nil)
         (extent
          (next-single-property-change
           start 'personality object end)))
     (cond
      ((null orig)                      ;simple case
       (put-text-property start extent
                          'personality personality object)
       (when (< extent end)
         (emacspeak-personality-append extent end
                                       personality object)))
      (t                               ;accumulate the new personality
       (unless (or (eq personality orig)
                   (and (listp orig)
                        (member personality orig)))
         (setq new
               (remove-duplicates
               (append
                (if (listp orig) orig (list orig))
                    (if (listp personality) personality (list personality)))))
         (put-text-property start extent
                            'personality new object))
       (when (< extent end)
         (emacspeak-personality-append extent end
                                       personality object)))))))

(defun emacspeak-personality-prepend  (start end
                                             personality &optional object)
  "Prepend specified personality to text bounded by start and end.
Existing personality properties on the text range are preserved."
  (ems-modify-buffer-safely
   (let ((orig (get-text-property start 'personality))
         (new nil)
         (extent
          (next-single-property-change
           start 'personality object end)))
     (cond
      ((null orig)                      ;simple case
       (put-text-property start extent
                          'personality personality object)
       (when (< extent end)
         (emacspeak-personality-prepend extent end
                                        personality object)))
      (t                               ;accumulate the new personality
      (unless (or (eq personality orig)
                   (and (listp orig)
                        (member personality orig))) 
       (setq new
               (remove-duplicates
               (append
                (if (listp personality) personality (list personality))
                    (if (listp orig) orig (list orig)))))
       (put-text-property start extent
                          'personality new object))
       (when (< extent end)
         (emacspeak-personality-prepend extent end
                                        personality)))))))

(defun emacspeak-personality-remove  (start end
                                                        personality object)
  "Remove specified personality from text bounded by start and end.
Other existing personality properties on the text range are
preserved."
  (ems-modify-buffer-safely
  (let ((orig (get-text-property start 'personality))
        (new nil)
        (extent
         (next-single-property-change
          start 'personality (current-buffer) end)))
    (cond
     ((null orig)                       ;simple case
      (when (< extent end)
        (emacspeak-personality-remove extent end personality)))
     (t                                 ;remove the new personality
      (setq new
            (cond
             ((eq orig personality) nil)
             ((listp orig)
              (remove personality orig))
             (t nil)))
      (if new 
          (put-text-property start extent
                             'personality new object)
        (remove-text-properties start extent
                                (list 'personality )
                                object))
      (when (< extent end)
        (emacspeak-personality-remove extent end
                                                  personality)))))))

;;}}}
;;{{{ attach voice lock to global font lock

(defadvice global-font-lock-mode (after emacspeak pre act comp)
  "Attach voice lock to font lock."
  (when global-font-lock-mode
    (setq-default voice-lock-mode t)))

;;}}}
;;{{{ advice put-text-personality

(defcustom emacspeak-personality-voiceify-faces
  'emacspeak-personality-prepend
  "Determines how and if we voiceify faces.

None means that  faces are not mapped to voices.
Prepend means that the corresponding personality is prepended to the
existing personalities on the text.

Append means place corresponding personality at the end."
  :type '(choice :tag "Overlay Voiceification"
                 (const :tag "None" nil)
                 (const :tag "Prepend" emacspeak-personality-prepend)
                 (const :tag "Append" emacspeak-personality-append))
  :group 'emacspeak-personality)

(defcustom emacspeak-personality-show-unmapped-faces nil
  "If set, faces that dont have a corresponding personality are
displayed in the messages area."
  :type 'boolean
  :group 'emacspeak-personality)


(defvar emacspeak-personality-unmapped-faces (make-hash-table)
  "Records faces that we have not yet mapped to personalities.")


(defadvice put-text-property (after emacspeak-personality  pre act) 
  "Used by emacspeak to augment font lock."
  (let ((start (ad-get-arg 0))
        (end (ad-get-arg 1 ))
        (prop (ad-get-arg 2))
        (value (ad-get-arg 3 ))
        (object (ad-get-arg 4))
        (voice nil))
    (when (and  emacspeak-personality-voiceify-faces
(eq prop 'face))
      (cond
       ((symbolp value)
        (setq voice (voice-setup-get-voice-for-face   value)))
       ((listp value)
        (setq voice
              (delete nil 
                      (mapcar   #'voice-setup-get-voice-for-face value))))
       (t (message "Got %s" value)))
      (when voice
        (funcall emacspeak-personality-voiceify-faces start end voice object))
      (when (and emacspeak-personality-show-unmapped-faces
                 (not voice))
        (cond
         ((listp value)
                 (mapcar #'(lambda (v)
                             (puthash  v t emacspeak-personality-unmapped-faces))
                         value))
         (t (puthash  value t emacspeak-personality-unmapped-faces)))))))

;;}}}
;;{{{ advice overlay-put 

(defcustom emacspeak-personality-voiceify-overlays
  'emacspeak-personality-prepend
  "Determines how and if we voiceify overlays.

None means that overlay faces are not mapped to voices.
Prepend means that the corresponding personality is prepended to the
existing personalities on the text under overlay.

Append means place corresponding personality at the end."
  :type '(choice :tag "Overlay Voiceification"
                 (const :tag "None" nil)
                 (const :tag "Prepend" emacspeak-personality-prepend)
                 (const :tag "Append" emacspeak-personality-append))
  :group 'emacspeak-personality)

(defadvice overlay-put (after emacspeak-personality  pre act) 
  "Used by emacspeak to augment font lock."
  (let ((overlay (ad-get-arg 0))
        (prop (ad-get-arg 1))
        (value (ad-get-arg 2))
        (voice nil))
    (when (eq prop 'face)
      (cond
       ((symbolp value)
        (setq voice (voice-setup-get-voice-for-face   value)))
       ((listp value)
        (setq voice
              (delete nil
                      (mapcar
                       #'voice-setup-get-voice-for-face value))))
       (t (message "Got %s" value)))
      (when voice
        (and emacspeak-personality-voiceify-overlays
             (funcall emacspeak-personality-voiceify-overlays
         (overlay-start overlay)
         (overlay-end overlay)
         voice))
        (overlay-put overlay 'personality voice))
      (when (and emacspeak-personality-show-unmapped-faces
                 (not voice))
        (cond
         ((listp value)
                 (mapcar #'(lambda (v)
                             (puthash  v t emacspeak-personality-unmapped-faces))
                         value))
         (t (puthash  value t
                      emacspeak-personality-unmapped-faces)))))))
(defadvice move-overlay (before emacspeak-personality  pre act) 
  "Used by emacspeak to augment font lock."
  (let ((overlay (ad-get-arg 0))
        (beg (ad-get-arg 1))
        (end (ad-get-arg 2))
        (object (ad-get-arg 3))
        (voice nil))
    (setq voice (overlay-get  overlay 'personality))
    (when (and voice
               emacspeak-personality-voiceify-overlays)
      (emacspeak-personality-remove
       (overlay-start overlay)
       (overlay-end overlay)
       voice object)
      (funcall emacspeak-personality-voiceify-overlays
               beg end voice))))

;;}}}
(provide 'emacspeak-personality )
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
