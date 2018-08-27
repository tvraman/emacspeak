;;; emacspeak-personality.el ---Voiceify Overlays  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Voice lock implementation
;;; Keywords: Emacspeak,  Spoken Output, audio formatting
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-08-25 18:28:19 -0700 (Sat, 25 Aug 2007) $ |
;;;  $Revision: 4555 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2017, T. V. Raman
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
;;; Implementation Notes From2018:

;;; After 3 years, variable emacspeak-personality-voiceify-faces has
;;; been removed,
;;; and the advice on put-text-property and friends removed.
;;; This module nowlimits itself to mapping face/font-lock properties
;;; from overlays to the associated text-property (personality).

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

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'custom)
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
(require 'advice)
(require 'voice-setup)

;;}}}
;;{{{Face Helpers: 

(defsubst emacspeak-personality-plist-face-p (plist)
  "Check if plist contains a face setting."
  (or (memq 'face plist)
      (memq 'font-lock-face plist)))

;;}}}
;;{{{ cumulative personalities

;;;###autoload
(defun emacspeak-personality-put (start end personality &optional object)
  "Apply personality to specified region,
over-writing any current personality settings."
  (when
      (and personality
           (integer-or-marker-p start)
           (integer-or-marker-p end)
           (not (= start end)))
    (let ((v
           (if (listp personality)
               (cl-delete-duplicates personality :test #'eq)
             personality)))
      (with-silent-modifications
        (put-text-property start end 'personality v object)))))

;;;###autoload
(defun emacspeak-personality-append  (start end personality &optional object)
  "Append specified personality to text bounded by start and end.
Existing personality properties on the text range are preserved."
  (when
      (and personality
           (integer-or-marker-p start)
           (integer-or-marker-p end)
           (not (= start end)))
    (with-silent-modifications
      (let ((inhibit-read-only t)
            (v (if (listp personality)
                   (cl-delete-duplicates personality :test #'eq)
                 personality))
            (orig (get-text-property start 'personality object))
            (new nil)
            (extent
             (next-single-property-change
              start 'personality object end)))
        (cond
         ((null orig)                    ;simple case
          (put-text-property start extent 'personality v object)
          (when (< extent end)
            (emacspeak-personality-append extent end v object)))
         (t                        ;accumulate the new personality
          (unless (or (equal  v orig)
                      (listp orig)
                      (and (listp orig)(memq v orig)))
            (setq new
                  (cl-delete-duplicates
                   (nconc
                    (if (listp orig) orig (list orig))
                    (if (listp v) v (list v)))))
            (put-text-property start extent
                               'personality new object))
          (when (< extent end)
            (emacspeak-personality-append extent end v object))))))))

;;;###autoload
(defun emacspeak-personality-prepend  (start end personality &optional object)
  "Prepend specified personality to text bounded by start and end.
Existing personality properties on the text range are preserved."
  (when
      (and personality
           (integer-or-marker-p start)
           (integer-or-marker-p end)
           (not (= start end)))
    (with-silent-modifications
      (let ((v (if (listp personality)
                   (cl-delete-duplicates personality :test #'eq)
                 personality))
            (orig (get-text-property start 'personality object))
            (new nil)
            (extent
             (next-single-property-change
              start 'personality object end)))
        (cond
         ((null orig)                   ;simple case
          (put-text-property start extent 'personality v object)
          (when (< extent end)
            (emacspeak-personality-prepend extent end v object)))
         (t                            ;accumulate the new personality
          (unless (or (equal v orig)
                      (listp orig)
                      (and (listp orig) (memq v orig)))
            (setq new
                  (cl-delete-duplicates
                   (nconc
                    (if (listp v) v (list v))
                    (if (listp orig) orig (list orig)))))
            (put-text-property start extent
                               'personality new object))
          (when (< extent end)
            (emacspeak-personality-prepend extent end v object)))))
      (unless buffer-read-only (restore-buffer-modified-p nil)))))

(defun emacspeak-personality-remove  (start end personality &optional object)
  "Remove specified personality from text bounded by start and end.
Preserve other existing personality properties on the text range."
  (when
      (and personality
           (integer-or-marker-p start)
           (integer-or-marker-p end)
           (not (= start end)))
    (with-silent-modifications
      (let ((orig (get-text-property start 'personality object))
            (new nil)
            (extent
             (next-single-property-change
              start 'personality (current-buffer) end)))
        (cond
         ((null orig)                    ;simple case
          (when (< extent end)
            (emacspeak-personality-remove extent end personality)))
         (t                            ;remove the new personality
          (setq new
                (cond
                 ((equal orig personality) nil)
                 ((listp orig)
                  (remove personality orig))
                 (t nil)))
          (if new
              (put-text-property start extent
                                 'personality new object)
            (remove-text-properties start extent
                                    (list 'personality)
                                    object))
          (when (< extent end)
            (emacspeak-personality-remove extent end personality))))))))

;;}}}
;;{{{ advice overlays

(defcustom emacspeak-personality-voiceify-overlays
  'emacspeak-personality-prepend
  "Determines how and if we voiceify overlays.

None means that overlay faces are not mapped to voices.
Prepend means that the corresponding personality is prepended to the
existing personalities on the text under overlay.

Append means place corresponding personality at the end."
  :type '(choice :tag "Overlay Voiceification"
                 (const :tag "None" nil)
                 (const :tag "Simple" emacspeak-personality-put)
                 (const :tag "Prepend" emacspeak-personality-prepend)
                 (const :tag "Append" emacspeak-personality-append))
  :group 'emacspeak-personality)

(defadvice overlay-put (after emacspeak-personality  pre act)
  "Used by emacspeak to augment font lock."
  (when emacspeak-personality-voiceify-overlays
    (let ((overlay (ad-get-arg 0))
          (prop (ad-get-arg 1))
          (value (ad-get-arg 2))
          (voice nil))
      (when
          (and
           (or (eq prop 'face)
               (eq prop 'font-lock-face)
               (and (eq prop 'category) (get value 'face)))
           (integer-or-marker-p (overlay-start overlay))
           (integer-or-marker-p (overlay-end overlay)))
        (and (eq prop 'category) (setq value (get value 'face)))
        (setq voice (dtk-get-voice-for-face value))
        (when voice
          (with-current-buffer (overlay-buffer overlay)
            (with-silent-modifications
              (funcall emacspeak-personality-voiceify-overlays
                       (overlay-start overlay) (overlay-end overlay)
                       voice (overlay-buffer overlay)))))))))

(defadvice delete-overlay (before emacspeak-personality  pre act)
  "Used by emacspeak to augment font lock."
  (with-silent-modifications
  (when emacspeak-personality-voiceify-overlays
    (let* ((o (ad-get-arg 0))
           (buffer (overlay-buffer o))
           (start (overlay-start o))
           (end (overlay-end o)))
      (when
          (and  buffer
                (emacspeak-personality-plist-face-p (overlay-properties o)))
        (with-current-buffer (overlay-buffer overlay)
          (condition-case nil 
          (put-text-property start end 'personality nil)
          (error nil))))))))

(defvar emacspeak-personality-advice-move-overlay t
  "Set to nil to avoid recursive advice during redisplay.")

(defadvice move-overlay (before emacspeak-personality  pre act)
  "Used by emacspeak to augment font lock."
  (when emacspeak-personality-advice-move-overlay
    (let ((overlay (ad-get-arg 0))
          (emacspeak-personality-advice-move-overlay nil)
          (beg (ad-get-arg 1))
          (end (ad-get-arg 2))
          (object (ad-get-arg 3))
          (voice nil))
      (setq voice (dtk-get-voice-for-face (overlay-get  overlay 'face)))
      (when
          (and voice
               emacspeak-personality-voiceify-overlays
               (integer-or-marker-p (overlay-start overlay))
               (integer-or-marker-p (overlay-end overlay)))
        (with-silent-modifications
          (emacspeak-personality-remove
           (overlay-start overlay)
           (overlay-end overlay)
           voice (overlay-buffer overlay))
          (funcall emacspeak-personality-voiceify-overlays
                   beg end voice object))))))

;;}}}
(provide 'emacspeak-personality)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
