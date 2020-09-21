;;; emacspeak-personality.el ---Voiceify Overlays  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Voice lock implementation
;;; Keywords: Emacspeak,  Spoken Output, audio formatting
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2007-08-25 18:28:19 -0700 (Sat, 25 Aug 2007) $ |
;;;  $Revision: 4555 $ |
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  Introduction:

;;; Commentary:
;;; Implementation Notes From 2018:

;;; After 3 years, variable emacspeak-personality-voiceify-faces has
;;; been removed,
;;; and the advice on put-text-property and friends removed.
;;; This module nowlimits itself to mapping face/font-lock properties
;;; from overlays to the associated text-property (personality).
;;; This mapping is done via emacspeak-personality-add.
;;; The options for cumulative personalities have been removed.

;;; Implementation Notes From 2015:

;;; Setting emacspeak-personality-voiceify-faces to
;;; nil  now results in dtk-speak falling back to the
;;; face->voice mapping defined via voice-setup for the face at
;;; point. What this means:
;;;
;;; 1. You always get voice-locking except
;;; when you set voice-lock-mode to nil.
;;;
;;; 2. The advice on
;;; put-text-property and friends become a no-op and we still get
;;; voice-locking.
;;;
;;; 3. Eventually this will become the default behavior
;;; for voice-locking.

;;; Implementation Notes from 2002.

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

;;; Finally, I am adding  better support for overlays --again this was a
;;; part of Emacs that was at its nascent stage in 1994, but is now
;;; stable.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'advice)
(require 'voice-setup)

;;}}}
;;{{{Apply Personality:
;;; Both functions below handle property changes in a "other" buffer correctly.
(defun emacspeak-personality-add (start end voice &optional object)
  "Apply personality VOICE to specified region in object,
over-writing any current personality settings."
  (when
      (and
       (integerp start) (integerp end)
       (not (= start end)))
    (with-current-buffer
        (if (bufferp object) object (current-buffer))
      (with-silent-modifications
        (put-text-property start end 'personality voice object)))))

(defun emacspeak-personality-remove  (start end voice &optional object)
  "Remove specified personality VOICE from text bounded by start and
end in object. "
  (when
      (and
       voice
       (integerp start) (integerp end)
       (not (= start end))
       (eq voice (get-text-property start 'personality object)))
      (with-current-buffer
          (if (bufferp object) object (current-buffer))
        (with-silent-modifications
          (put-text-property start end 'personality nil object)))))

;;}}}
;;{{{ advice overlays

(defvar ems--voiceify-overlays #'emacspeak-personality-add
  "Determines how and if we voiceify overlays. ")

;;; Needed for  outline support:
(defadvice remove-overlays (around emacspeak pre act comp)
  "Clean up properties mirrored from overlays."
  (let ((ems--voiceify-overlays  nil)
        (beg (or (ad-get-arg 0) (point-min)))
        (end (or (ad-get-arg 1) (point-max)))
        (name (ad-get-arg 2)))
    (when (zerop beg) (setq beg (point-min)))
    (with-silent-modifications          ; ignores value for now 
      (put-text-property beg end name nil))
    ad-do-it))

(defadvice delete-overlay (before emacspeak-personality  pre act)
  "Used by emacspeak to augment voice lock."
  (when ems--voiceify-overlays
    (let* ((o (ad-get-arg 0))
           (buffer (overlay-buffer o))
           (start (overlay-start o))
           (end (overlay-end o))
           (voice (dtk-get-voice-for-face (overlay-get o 'face)))
           (invisible (overlay-get o 'invisible)))
      (when  (and  start end voice buffer)
        (with-current-buffer buffer
          (save-restriction
            (widen)
            (emacspeak-personality-remove start end voice buffer))))
      (when  (and start end invisible)
        (with-silent-modifications
          (put-text-property start end 'invisible nil))))))

(defadvice overlay-put (after emacspeak-personality pre act)
  "Used by emacspeak to augment voice lock."
  (when (and (overlay-buffer (ad-get-arg 0)) ems--voiceify-overlays)
    (let* ((overlay (ad-get-arg 0))
           (prop (ad-get-arg 1))
           (value (ad-get-arg 2))
           (start (overlay-start overlay))
           (end (overlay-end overlay))
           (voice nil))
      (cond
       ((and
         (or (eq prop 'face)
             (eq prop 'font-lock-face)
             (and (eq prop 'category) (get value 'face)))
         (integerp start) (integerp end))
        (when (eq prop 'category) (setq value (get value 'face)))
        (setq voice (dtk-get-voice-for-face value))
        (when voice
            (funcall
             ems--voiceify-overlays
             start end voice (overlay-buffer overlay))))
       ((eq prop 'invisible)
        (with-current-buffer (overlay-buffer overlay)
          (with-silent-modifications
            (put-text-property start end 'invisible (or value nil)))))))))

(defadvice move-overlay (before emacspeak-personality pre act)
  "Used by emacspeak to augment voice lock."
  (when ems--voiceify-overlays
    (let*
        ((overlay (ad-get-arg 0))
         (beg (ad-get-arg 1))
         (end (ad-get-arg 2))
         (object (ad-get-arg 3))
         (buffer (overlay-buffer overlay))
         (voice (dtk-get-voice-for-face (overlay-get overlay 'face)))
         (invisible (overlay-get overlay 'invisible)))
      (unless object (setq object (or buffer (current-buffer))))
      (when
          (and voice
               (integerp (overlay-start overlay))
               (integerp (overlay-end overlay)))
        (emacspeak-personality-remove
         (overlay-start overlay) (overlay-end overlay) voice buffer)
        (funcall ems--voiceify-overlays beg end voice object))
      (when invisible
        (with-current-buffer buffer
          (with-silent-modifications
            (put-text-property
             (overlay-start overlay) (overlay-end overlay) 'invisible nil)))
        (with-current-buffer object
          (with-silent-modifications
            (put-text-property beg end 'invisible invisible)))))))

;;}}}
(provide 'emacspeak-personality)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
