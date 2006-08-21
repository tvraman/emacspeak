;;; emacspeak-ediff.el --- Speech enable Emacs interface to diff and merge
;;; $Id$
;;; $Author$ 
;;; DescriptionEmacspeak extensions for ediff
;;; Keywords:emacspeak, audio interface to emacs, Comparing files 
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
;;; Copyright (c) 1995 by .
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

;;{{{  required 

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'backquote)
(require 'custom)
(eval-when-compile (require 'dtk-speak)
(require 'emacspeak-speak)
(require 'emacspeak-keymap)
(require 'emacspeak-sounds)
(require 'voice-lock)
        (require 'ediff)
        (require 'ediff-init))
    

;;}}}
;;{{{  Introduction:

;;;Ediff provides a nice visual interface to diff.  ;;;Comparing and
;;; patching files is easy with ediff when you can see the screen.
;;; ;;;This module provides Emacspeak extensions to work fluently
;;; ;;;with ediff. Try it out, it's an excellent example of why
;;; Emacspeak is better than a traditional screenreader.  This module
;;; was originally written to interface to the old ediff.el bundled
;;; with GNU Emacs 19.28 and earlier.  It has been updated to work
;;; with the newer and much larger ediff system found in Emacs 19.29
;;; and later.

;;}}}
;;{{{  macros

(defgroup  emacspeak-ediff nil
  "Emacspeak support for EDiff."
  :link '(custom-group-link :tag "ediff"
                            ediff)
  :group 'emacspeak
  :prefix "emacspeak-ediff-")

(defmacro emacspeak-ediff-modify-buffer-safely   (&rest body )
  (`
   (let    ((save-read-only buffer-read-only)
            (buffer-read-only nil )
            (inhibit-read-only t)
            (before-change-functions nil)
            (after-change-functions nil)
            (modification-flag (buffer-modified-p)))
     (unwind-protect
         (,@ body )
       (setq buffer-read-only save-read-only )
       (set-buffer-modified-p modification-flag )))))


;;}}}
;;{{{  Mapping faces to personalities:

(defcustom emacspeak-ediff-A-personality 'paul-smooth
  "Personality used to voiceify difference chunk A"
  :type 'symbol
  :group 'emacspeak-ediff)

(defcustom emacspeak-ediff-B-personality
  'paul-monotone
  "Personality used to voiceify difference chunk B"
  :type 'symbol
:group 'emacspeak-ediff)

(defcustom emacspeak-ediff-fine-A-personality 'harry
  "Personality used to voiceify difference chunk A"
  :type 'symbol
  :group 'emacspeak-ediff)

(defcustom emacspeak-ediff-fine-B-personality
  'harry
  "Personality used to voiceify difference chunk B"
  :type 'symbol
:group 'emacspeak-ediff)


;;}}}
;;{{{ Helper functions:

(defvar emacspeak-ediff-control-buffer nil
  "Holds the control buffer for the most recent ediff")
;;;Please tell me what control buffer you're using--

(defadvice ediff-setup-control-buffer (after emacspeak pre act )
  (declare (special emacspeak-ediff-control-buffer))
  (setq emacspeak-ediff-control-buffer (ad-get-arg 0 )))


(defsubst emacspeak-ediff-control-panel ()
  (declare (special emacspeak-ediff-control-buffer ))
  emacspeak-ediff-control-buffer)

(defsubst emacspeak-ediff-difference-a-overlay (n)
  (declare (special ediff-difference-vector-A
                    ediff-number-of-differences))
  (assert (< n ediff-number-of-differences) t
          "There are only %s differences"
          ediff-number-of-differences)
  (aref (aref ediff-difference-vector-A n) 0))
  
(defsubst emacspeak-ediff-difference-b-overlay (n)
  (declare (special ediff-difference-vector-B
                    ediff-number-of-differences))
  (assert (< n ediff-number-of-differences) t
          "There are only %s differences"
          ediff-number-of-differences)
  (aref (aref ediff-difference-vector-B n) 0))

(defsubst emacspeak-ediff-difference-c-overlay (n)
  (declare (special ediff-difference-vector-B
                    ediff-number-of-differences))
  (assert (< n ediff-number-of-differences) t
          "There are only %s differences"
          ediff-number-of-differences)
  (aref (aref ediff-difference-vector-C n) 0))


(defsubst emacspeak-ediff-fine-difference-a-overlays (n)
  (declare (special ediff-difference-vector-A
                    ediff-number-of-differences))
  (assert (< n ediff-number-of-differences) t
          "There are only %s differences"
          ediff-number-of-differences)
  (aref (aref ediff-difference-vector-A n) 1))
  
(defsubst emacspeak-ediff-fine-difference-b-overlays (n)
  (declare (special ediff-difference-vector-B
                    ediff-number-of-differences))
  (assert (< n ediff-number-of-differences) t
          "There are only %s differences"
          ediff-number-of-differences)
  (aref (aref ediff-difference-vector-B n) 1))

(defsubst emacspeak-ediff-fine-difference-c-overlays (n)
  (declare (special ediff-difference-vector-B
                    ediff-number-of-differences))
  (assert (< n ediff-number-of-differences) t
          "There are only %s differences"
          ediff-number-of-differences)
  (aref (aref ediff-difference-vector-C n) 1))

(defsubst emacspeak-ediff-difference-fine-diff   (difference)
  (aref difference 2))

;;}}}
;;{{{  Voiceify variants

(defsubst  emacspeak-ediff-diff-overlay-from-difference  (diff counter )
  (aref (aref diff counter) 0))


(defsubst emacspeak-ediff-fine-overlays-from-difference  (diff counter )
  (aref (aref diff counter) 1))

(defsubst  emacspeak-ediff-voicify-extent  (overlay  personality)
  (put-text-property (overlay-start overlay)
                     (overlay-end overlay)
                     'personality personality ))

(defun emacspeak-ediff-voiceify-variant (variant diff-vector
                                                 personality fine-personality)
  "Voiceify ediff variant"
  (let ((count (length diff-vector))
        (counter 0))
    (save-excursion
      (set-buffer variant)
      (emacspeak-ediff-modify-buffer-safely
       (while (< counter count)
         (emacspeak-ediff-voicify-extent
          (emacspeak-ediff-diff-overlay-from-difference  diff-vector counter )
          personality )
         (incf counter))))))
(defun emacspeak-ediff-voiceify-fine-diff (counter)
  "Voiceify current fine difference."
  (declare (special ediff-current-difference
                    emacspeak-ediff-fine-A-personality emacspeak-ediff-fine-B-personality
                    ediff-difference-vector-A ediff-difference-vector-B
                    ediff-buffer-A ediff-buffer-B))
  (let ((control-panel (emacspeak-ediff-control-panel)))
    (when control-panel
      (save-excursion
        (set-buffer control-panel )
        (let ((a-vector ediff-difference-vector-A)
              (b-vector ediff-difference-vector-B))
          (and (<  counter 0)
               (error "ediff-current-difference is negative!"))
          (and a-vector
               (save-excursion
                 (set-buffer ediff-buffer-A)
                 (emacspeak-ediff-modify-buffer-safely
                  (mapcar
                   (function
                    (lambda  (o)
                      (emacspeak-ediff-voicify-extent  o
                                                       emacspeak-ediff-fine-A-personality)))
                   (emacspeak-ediff-fine-overlays-from-difference
                    a-vector counter)))))
          (and b-vector
               (save-excursion
                 (set-buffer ediff-buffer-B)
                 (emacspeak-ediff-modify-buffer-safely
                  (mapcar
                   (function
                    (lambda  (o)
                      (emacspeak-ediff-voicify-extent  o
                                                       emacspeak-ediff-fine-B-personality)))
                   (emacspeak-ediff-fine-overlays-from-difference
                    b-vector counter))))))))))

;;}}}
;;{{{  Function: Voicify  ediff overlays:

;;; Voiceify ediff overlay

(defsubst  emacspeak-ediff-voicify-overlay  (overlay  personality)
  (let ((buffer (overlay-buffer overlay ))
        (start (overlay-start overlay))
        (end (overlay-end overlay )))
    (save-excursion
      (set-buffer buffer )
      (emacspeak-ediff-modify-buffer-safely
       (put-text-property start end
                          'personality personality)))))

(defun emacspeak-ediff-voicify-differences  ()
  "Voicify all the difference chunks"
  (declare (special ediff-buffer-A ediff-buffer-B
                    ediff-number-of-differences
                    emacspeak-ediff-A-personality
                    emacspeak-ediff-B-personality
                    emacspeak-ediff-fine-A-personality
                    emacspeak-ediff-fine-B-personality))
  (let ((control-panel (emacspeak-ediff-control-panel)))
    (when control-panel
      (save-excursion
        (set-buffer control-panel )
        (when ediff-buffer-A
          (emacspeak-ediff-voiceify-variant ediff-buffer-A
                                            ediff-difference-vector-A
                                            emacspeak-ediff-A-personality
                                            emacspeak-ediff-fine-A-personality))
        (when ediff-buffer-B
          (emacspeak-ediff-voiceify-variant ediff-buffer-B
                                            ediff-difference-vector-B
                                            emacspeak-ediff-B-personality
                                            emacspeak-ediff-fine-B-personality)))))
  (message "Voicified differences" ))

(defun emacspeak-ediff-voicify-fine-differences  ()
  "Voicify all the fine difference chunks"
  (declare (special ediff-number-of-differences
                    ediff-buffer-A ediff-buffer-B))
  (let ((counter 0)
        (control-panel (emacspeak-ediff-control-panel)))
    (when control-panel
      (save-excursion
        (set-buffer control-panel )
        (save-excursion
          (set-buffer ediff-buffer-A)
          (while (< counter ediff-number-of-differences )
            (mapcar
             (function
              (lambda (overlay)
                (emacspeak-ediff-voicify-extent overlay
                                                emacspeak-ediff-fine-A-personality)))
             (emacspeak-ediff-fine-difference-a-overlays counter )))
          (incf counter))
        ;; do the same for variant B
        (setq counter 0)
        (save-excursion
          (set-buffer ediff-buffer-B)
          (while (< counter ediff-number-of-differences)
            (mapcar
             (function
              (lambda (overlay)
                (emacspeak-ediff-voicify-extent overlay
                                                emacspeak-ediff-fine-B-personality)))
             (emacspeak-ediff-fine-difference-b-overlays  counter )) 
            (incf counter )))
        (message "Voicified fine differences ")))))
(declaim (special ediff-auto-refine))
(setq-default ediff-auto-refine 'on)

  
  
(add-hook 'ediff-startup-hook
          (function (lambda ()
                      (declare (special ediff-mode-map
                                        voice-lock-mode))
                      (setq voice-lock-mode t)
                      (emacspeak-keymap-remove-emacspeak-edit-commands ediff-mode-map)
                      (define-key ediff-mode-map "." 'emacspeak-ediff-speak-current-difference)
                      (emacspeak-ediff-voicify-differences))))

;;}}}
;;{{{  Speak an ediff difference:

;;; To speak an ediff difference,
;;; First announce difference a and speak it.
;;; If you see keyboard activity, shut up
;;; and offer to speak difference b.


(defun emacspeak-ediff-speak-difference (n)
  "Speak a difference chunk"
  (let ((a-overlay (emacspeak-ediff-difference-a-overlay n ))
        (b-overlay (emacspeak-ediff-difference-b-overlay  n ))
        (key ""))
    (emacspeak-auditory-icon 'select-object)
    (dtk-speak
     (concat
      "Difference ai "
      (emacspeak-overlay-get-text  a-overlay)))
    (let ((dtk-stop-immediately nil ))
      (sit-for 2)
      (setq key 
            (read-key-sequence "Press any key to continue" )))
    (unless    (=  7  (string-to-char key ))
      (dtk-stop)
      (dtk-speak
       (concat
        "Difference  B  "
        (emacspeak-overlay-get-text b-overlay ))))))

(defun emacspeak-ediff-speak-current-difference ()
  "Speak the current difference"
  (interactive)
  (declare (special ediff-current-difference
                    ediff-number-of-differences))
  (emacspeak-ediff-speak-difference
   (cond
    ((minusp ediff-current-difference) 0)
    ((>= ediff-current-difference ediff-number-of-differences)
     (1- ediff-number-of-differences))
    (t ediff-current-difference))))

;;}}}
;;{{{ Advice:

(defcustom emacspeak-ediff-always-autorefine-diffs t
  "Says if emacspeak should try computing fine differences each time.
Set this to nil if things get too slow."
  :type 'boolean
  :group 'emacspeak-ediff)

(defadvice ediff-next-difference (after emacspeak pre act comp)
  "Speak the difference interactively."
  (declare (special emacspeak-ediff-always-autorefine-diffs))
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-ediff-speak-current-difference)))

(defadvice ediff-previous-difference (after emacspeak pre act comp)
  "Speak the difference interactively."
  (declare (special emacspeak-ediff-always-autorefine-diffs))
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-ediff-speak-current-difference)))

(defadvice ediff-make-fine-diffs (after emacspeak pre act comp)
  "voicify the fine differences"
  (let ((counter(or
                 (ad-get-arg 0)
                 ediff-current-difference)))
    (emacspeak-ediff-voiceify-fine-diff counter)))

(defadvice ediff-status-info (after emacspeak pre act )
  "Speak the status information"
  (when (interactive-p)
    (save-excursion
      (set-buffer " *ediff-info*")
      (emacspeak-speak-buffer ))))

(defadvice ediff-scroll-up (after emacspeak pre act )
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (message "Scrolled up buffers A and B")))

(defadvice ediff-scroll-down (after emacspeak pre act )
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (message "Scrolled down buffers A and B")))

(defadvice ediff-toggle-split (after emacspeak pre act )
  "Provide auditory feedback"
  (when (interactive-p)
    (if (eq ediff-split-window-function 'split-window-vertically)
        (message "Split ediff windows vertically")
      (message "Split ediff windows horizontally"))))

(defadvice ediff-recenter (after emacspeak pre act )
  "Provide spoken feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object )
    (message "Refreshed the ediff display")))

(defadvice ediff-jump-to-difference (after emacspeak pre act )
  "Speak the difference you jumped to"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-ediff-speak-current-difference )))

(defadvice ediff-jump-to-difference-at-point (after emacspeak pre act )
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-ediff-speak-current-difference)))

;;; advice meta panel 
(defadvice ediff-previous-meta-item (after emacspeak pre act
                                           comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object )))
(defadvice ediff-next-meta-item (after emacspeak pre act
                                       comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object )))

(defadvice ediff-registry-action (after emacspeak pre act
                                        comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'open-object)))

(defadvice ediff-show-registry (after emacspeak pre act
                                      comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Welcome to the Ediff registry")))

(defadvice ediff-toggle-filename-truncation (after emacspeak pre
                                                   act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (message "turned %s file name truncation in Ediff registry"
             ediff-meta-truncate-filenames)))



;;}}}
(provide  'emacspeak-ediff)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 


;;}}}
