;;; emacspeak-imenu.el --- Speech enable Imenu -- produce buffer-specific table of contents
;;; $Id$
;;; $Author$ 
;;; Description: Auditory interface buffer indices
;;; Keywords: Emacspeak, Speak, Spoken Output, indices
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

;;; Copyright (c) 1995 -- 2007, T. V. Raman
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

;;{{{  Required modules

(require 'emacspeak-preamble)
;;}}}
;;{{{  Introduction

;;; Speech enable imenu and provide other useful navigation commands.

;;}}}
;;{{{  variables

(defvar emacspeak-imenu-flattened-index-alist nil
  "Cached flattened index alist for buffer navigation")
(make-variable-buffer-local 'emacspeak-imenu-flattened-index-alist)

;;}}}
;;{{{  advise imenu to cache flattened alist

(defun emacspeak-imenu-flatten-index-alist (index-alist &optional concat-names prefix)
  ;; Takes a nested INDEX-ALIST and returns a flat index alist.
  ;; If optional CONCAT-NAMES is non-nil, then a nested index has its
  ;; name and a space concatenated to the names of the children.
  ;; Third argument PREFIX is for internal use only.

  (declare (special imenu-level-separator))
  (mapcan
   (function
    (lambda (item)
      (let* ((name (car item))
             (pos (cdr item))
             (new-prefix (and concat-names
                              (if prefix
                                  (concat prefix imenu-level-separator name)
                                name))))
        (cond
         ((or (markerp pos) (numberp pos)
              (overlayp pos))
          (list (cons new-prefix pos)))
         (t
          (emacspeak-imenu-flatten-index-alist pos
                                               new-prefix))))))
   index-alist))

(defadvice imenu--make-index-alist (after emacspeak pre act comp)
  "Cache flattened index alist"
  (declare (special emacspeak-imenu-flattened-index-alist))
  (setq emacspeak-imenu-flattened-index-alist
        (emacspeak-imenu-flatten-index-alist
         imenu--index-alist t)))

;;}}}
;;{{{ advice 

(defadvice imenu (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (ems-set-personality-temporarily (point) (1+ (point))
                                     voice-animate
                                     (emacspeak-speak-line))))

(defadvice imenu-go-find-at-position (around emacspeak pre act comp)
  "Provide auditory feedback"
  (cond
   ((interactive-p)
    (push-mark)
    ad-do-it
    (emacspeak-auditory-icon 'large-movement)
    (ems-set-personality-temporarily (point) (1+ (point))
                                     voice-animate
                                     (emacspeak-speak-line)))
   (t ad-do-it))
  ad-return-value)

(defadvice imenu-go--back (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (ems-set-personality-temporarily (point) (1+ (point))
                                     voice-animate
                                     (emacspeak-speak-line))))

;;}}}
;;{{{  Navigation
(defcustom emacspeak-imenu-autospeak nil
  "Speak contents of sections automatically if set."
  :type 'boolean
  :group 'emacspeak-imenu)

(defun emacspeak-imenu-goto-next-index-position ()
  "Goto the next index position in current buffer"
  (interactive)
  (declare (special emacspeak-imenu-flattened-index-alist
                    emacspeak-imenu-autospeak
                    imenu--index-alist))
  (let ((position (point))
        (guess 0)
        (target (point-max)))
    (unless emacspeak-imenu-flattened-index-alist
      (setq emacspeak-imenu-flattened-index-alist
            (emacspeak-imenu-flatten-index-alist
             imenu--index-alist t)))
    (loop for item  in emacspeak-imenu-flattened-index-alist
          do
          (setq guess
                (cond
                 ((overlayp (cdr item))
                  (overlay-start (cdr item )))
                 ((markerp (cdr item))
                  (marker-position (cdr item )))
                 (t (cdr item))))
          (when (< position guess)
            (if (< guess target)
                (setq target guess))))
    (goto-char target)
    (when (interactive-p)
      (emacspeak-auditory-icon 'large-movement)
      (if emacspeak-imenu-autospeak
          (emacspeak-imenu-speak-this-section)
        (emacspeak-speak-line))
      (when (overlays-at (point))
        (goto-char (overlay-end (car (overlays-at (point)))))))))

(defun emacspeak-imenu-goto-previous-index-position ()
  "Goto the previous index position in current buffer"
  (interactive)
  (declare (special emacspeak-imenu-flattened-index-alist
                    emacspeak-imenu-autospeak
                    imenu--index-alist))
  (let ((position (point))
        (guess 0)
        (target (point-min)))
    (unless emacspeak-imenu-flattened-index-alist
      (setq emacspeak-imenu-flattened-index-alist
            (emacspeak-imenu-flatten-index-alist
             imenu--index-alist t)))
    (loop for item  in emacspeak-imenu-flattened-index-alist
          do
          (setq guess
                (cond
                 ((overlayp (cdr item))
                  (overlay-start (cdr item )))
                 ((markerp (cdr item))
                  (marker-position (cdr item )))
                 (t (cdr item))))
          (when (> position guess)
            (if (> guess target)
                (setq target guess))))
    (goto-char target)
    (when (interactive-p)
      (emacspeak-auditory-icon 'large-movement)
      (if emacspeak-imenu-autospeak
          (emacspeak-imenu-speak-this-section)
        (emacspeak-speak-line)))))

;;}}}
;;{{{  speaking logical sections

(defun emacspeak-imenu-speak-this-section ()
  "Speak upto start of next index entry"
  (interactive)
  (let ((start (point)))
    (save-excursion
      (emacspeak-imenu-goto-next-index-position)
      (emacspeak-speak-region start (point)))))

;;}}}
;;{{{ bind keys
(define-key emacspeak-keymap "\M-i" 'imenu)
(define-key emacspeak-keymap  "\M-p" 'emacspeak-imenu-goto-previous-index-position)
(define-key emacspeak-keymap "\M-n" 'emacspeak-imenu-goto-next-index-position)
(define-key emacspeak-keymap "\M- " 'emacspeak-imenu-speak-this-section)

;;}}}
;;{{{ customize settings

(declaim (special imenu-space-replacement
                  imenu-max-items))
(setq imenu-space-replacement "."
      imenu-max-items 200)

;;}}}
(provide 'emacspeak-imenu )
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
