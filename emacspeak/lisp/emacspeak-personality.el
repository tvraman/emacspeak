;;; emacspeak-personality.el ---Emacspeak's new personality interface
;;; $Id$
;;; $Author$
;;; Description:  Voice lock implementation
;;; Keywords: Emacspeak,  Spoken Output, audio formatting
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

;;; Finally, I am adding  better support for overlays --again this was a
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
;;{{{ attach voice lock to global font lock
(defadvice font-lock-mode (after  emacspeak pre act comp)
  "Attach voice-lock-mode to font-lock-mode."
  (voice-lock-mode   font-lock-mode))
(defadvice global-font-lock-mode (after emacspeak pre act comp)
  "Attach voice lock to font lock."
  (when global-font-lock-mode
    (setq-default voice-lock-mode t)))

;;}}}
;;{{{ cumulative personalities 

;;;###autoload
(defsubst emacspeak-personality-put (start end personality object)
  "Apply personality to specified region, over-writing any current
personality settings."
  (when (and (integer-or-marker-p start)
             (integer-or-marker-p end ))
    (let ((v (if (listp personality)
                 (remove-duplicates personality :test #'eq)
               personality)))
      (ems-modify-buffer-safely
       (put-text-property start end 'personality v object)))))

;;;###autoload
(defun emacspeak-personality-append  (start end personality &optional object )
  "Append specified personality to text bounded by start and end.
Existing personality properties on the text range are preserved."
  (when (and (integer-or-marker-p start)
             (integer-or-marker-p end ))
    (ems-modify-buffer-safely
     (let ((v (if (listp personality)
                  (remove-duplicates personality :test #'eq)
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
        (t                             ;accumulate the new personality
         (unless (or (equal  v orig)
                     (listp orig)
                     (and (listp orig)(memq v orig)))
           (setq new
                 (remove-duplicates
                  (append
                   (if (listp orig) orig (list orig))
                   (if (listp v) v (list v)))))
           (put-text-property start extent
                              'personality new object))
         (when (< extent end)
           (emacspeak-personality-append extent end v object))))))))

;;;###autoload
(defun emacspeak-personality-prepend  (start end
                                             personality &optional object)
  "Prepend specified personality to text bounded by start and end.
Existing personality properties on the text range are preserved."
  (when (and (integer-or-marker-p start)
             (integer-or-marker-p end ))
    (ems-modify-buffer-safely
     (let ((v (if (listp personality)
                  (remove-duplicates personality :test #'eq)
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
           (emacspeak-personality-prepend extent end v object)))
        (t                             ;accumulate the new personality
         (unless (or (equal v orig)
                     (listp orig)
                     (and (listp orig) (memq v orig)))
           (setq new
                 (remove-duplicates
                  (append
                   (if (listp v) v (list v))
                   (if (listp orig) orig (list orig)))))
           (put-text-property start extent
                              'personality new object))
         (when (< extent end)
           (emacspeak-personality-prepend extent end v object))))))))

(defun emacspeak-personality-remove  (start end
                                            personality
                                            &optional object)
  "Remove specified personality from text bounded by start and end.
Other existing personality properties on the text range are
preserved."
  (when (and (integer-or-marker-p start)
             (integer-or-marker-p end ))
    (ems-modify-buffer-safely
     (let ((orig (get-text-property start 'personality object))
           (new nil)
           (extent
            (next-single-property-change
             start 'personality (current-buffer) end)))
       (cond
        ((null orig)                    ;simple case
         (when (< extent end)
           (emacspeak-personality-remove extent end personality)))
        (t                              ;remove the new personality
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
                                   (list 'personality )
                                   object))
         (when (< extent end)
           (emacspeak-personality-remove extent end
                                         personality))))))))

;;}}}
;;{{{ helper: face-p

(defsubst emacspeak-personality-plist-face-p (plist)
  "Check if plist contains a face setting."
  (or (memq 'face plist)
      (memq 'font-lock-face plist)))
(defsubst ems-plain-cons-p (value)
  "Help identify (a . b)."
  (and (consp value)    
       (equal value (last value))
       (cdr value)))

;;}}}
;;{{{ advice put-text-personality

(defcustom emacspeak-personality-voiceify-faces
  'emacspeak-personality-put
  "Determines how and if we voiceify faces.

None means that  faces are not mapped to voices.
Prepend means that the corresponding personality is prepended to the
existing personalities on the text.

Append means place corresponding personality at the end.
Simple means that voiceification is not cumulative --this is the default."
  :type '(choice :tag "Face Voiceification"
                 (const :tag "None" nil)
                 (const :tag "Simple" emacspeak-personality-put)
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
                (or (eq prop 'face) (eq prop 'font-lock-face)))
      (condition-case nil
          (progn
            (cond
             ((symbolp value)
              (setq voice (voice-setup-get-voice-for-face value)))
             ((ems-plain-cons-p value)) ;;pass on plain cons
             ( (listp value)
               (setq voice
                     (delq nil 
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
               (t (puthash  value t emacspeak-personality-unmapped-faces)))))
        (error nil)))))

(defadvice add-text-properties (after emacspeak-personality  pre act) 
  "Used by emacspeak to augment font lock."
  (let ((start (ad-get-arg 0))
        (end (ad-get-arg 1 ))
        (properties (ad-get-arg 2))
        (object (ad-get-arg 3))
        (facep nil)
        (voice nil)
        (value nil))
    (setq facep (emacspeak-personality-plist-face-p properties))
    (when (and  emacspeak-personality-voiceify-faces
                facep)
      (setq value (second facep))
      (condition-case nil
          (progn
            (cond
             ((symbolp value)
              (setq voice (voice-setup-get-voice-for-face   value)))
             ((ems-plain-cons-p value)) ;;pass on plain cons
             ( (listp value)
               (setq voice
                     (delq nil 
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
               (t (puthash  value t emacspeak-personality-unmapped-faces)))))
        (error nil)))))

(defadvice set-text-properties (after emacspeak-personality  pre act) 
  "Used by emacspeak to augment font lock."
  (let ((start (ad-get-arg 0))
        (end (ad-get-arg 1 ))
        (properties (ad-get-arg 2))
        (object (ad-get-arg 3))
        (facep nil)
        (voice nil)
        (value nil))
    (setq facep (emacspeak-personality-plist-face-p properties))
    (when (and  emacspeak-personality-voiceify-faces
                facep)
      (setq value (second facep))
      (condition-case nil
          (progn
            (cond
             ((symbolp value)
              (setq voice (voice-setup-get-voice-for-face   value)))
             ((ems-plain-cons-p value)) ;;pass on plain cons
             ( (listp value)
               (setq voice
                     (delq nil 
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
               (t (puthash  value t emacspeak-personality-unmapped-faces))))
            )
        (error nil))
      )))

(defadvice propertize (around emacspeak-personality  pre act) 
  "Used by emacspeak to augment font lock."
  (let ((string (ad-get-arg 0))
        (properties (ad-get-args 1))
        (facep nil)
        (voice nil)
        (value nil))
    (setq facep (emacspeak-personality-plist-face-p properties))
    (cond
     ((and  emacspeak-personality-voiceify-faces
            facep)
      ad-do-it
      (setq value (second facep))
      (condition-case nil
          (progn
            (cond
             ((symbolp value)
              (setq voice (voice-setup-get-voice-for-face   value)))
             ;; ((and (consp value)     ;check for plain cons and pass
             ;;                    (equal value (last value)))
             ;;               nil)
             ( (listp value)
               (setq voice
                     (delq nil 
                           (mapcar   #'voice-setup-get-voice-for-face value))))
             (t (message "Got %s" value)))
            (when voice
              (funcall emacspeak-personality-voiceify-faces 0
                       (length ad-return-value) voice ad-return-value)
              )
            (when (and emacspeak-personality-show-unmapped-faces
                       (not voice))
              (cond
               ((listp value)
                (mapcar #'(lambda (v)
                            (puthash  v t emacspeak-personality-unmapped-faces))
                        value))
               (t (puthash  value t emacspeak-personality-unmapped-faces)))))
        (error nil)))
     (t ad-do-it))
    ad-return-value))

(defadvice remove-text-properties (before emacspeak-personality pre act comp)
  "Undo any voiceification if needed."
  (let  ((start (ad-get-arg 0))
         (end (ad-get-arg 1))
         (props (ad-get-arg 2))
         (object (ad-get-arg 3))
         (voice nil)
         (face nil))
    (when (emacspeak-personality-plist-face-p props) ;;; simple minded for now
      (put-text-property start end
                         'personality nil object))))

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
    (when (and
           (or (eq prop 'face)
               (and (eq prop 'category)
                    (get value 'face)))
           (integer-or-marker-p (overlay-start overlay))
           (integer-or-marker-p (overlay-end overlay)))
      (and (eq prop 'category)
           (setq value (get value 'face)))
      (cond
       ((symbolp value)
        (setq voice (voice-setup-get-voice-for-face   value)))
       ((listp value)
        (setq voice
              (delq nil
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
               emacspeak-personality-voiceify-overlays
               (integer-or-marker-p (overlay-start overlay))
               (integer-or-marker-p (overlay-end overlay)))
      (emacspeak-personality-remove
       (overlay-start overlay)
       (overlay-end overlay)
       voice object)
      (funcall emacspeak-personality-voiceify-overlays
               beg end voice))))

;;}}}
;;{{{ silence font-lock's error messages

(defadvice font-lock-apply-syntactic-highlight (around emacspeak
                                                       pre act
                                                       comp)
  "Silence auditory feedback from redisplay errors."
  (ems-with-errors-silenced ad-do-it))

;;}}}
(provide 'emacspeak-personality )
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
