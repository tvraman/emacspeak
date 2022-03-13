;;; emacspeak-ediff.el --- Speech enable Emacs interface to diff and merge  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; DescriptionEmacspeak extensions for ediff
;;; Keywords:emacspeak, audio interface to emacs, Comparing files
;;{{{ LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2008-06-21 10:50:41 -0700 (Sat, 21 Jun 2008) $ |
;;; $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{ Copyright:

;;;Copyright (C) 1995 -- 2021, T. V. Raman
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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING. If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}

;;{{{ Introduction:

;;; Commentary:

;;;Ediff provides a nice visual interface to diff. ;;;Comparing and
;;; patching files is easy with ediff when you can see the screen.
;;; ;;;This module provides Emacspeak extensions to work fluently
;;; ;;;with ediff. Try it out, it's an excellent example of why
;;; Emacspeak is better than a traditional screenreader. This module
;;; was originally written to interface to the old ediff.el bundled
;;; with GNU Emacs 19.28 and earlier. It has been updated to work
;;; with the newer and much larger ediff system found in Emacs 19.29
;;; and later.
;;;
;;; When using under modern versions of Emacs, I recommend setting
;;; (setq ediff-window-setup-function 'ediff-setup-windows-plain)
;;; so that Emacs always displays Ediff windows in a single frame.
;;; Code:

;;}}}
;;{{{ required:
(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'voice-setup)
(require 'ediff)
;;}}}
;;{{{ Map faces to voices.

(voice-setup-add-map
 '(
   (ediff-current-diff-A voice-smoothen)
   (ediff-current-diff-B voice-brighten)
   (ediff-current-diff-C voice-lighten)
   (ediff-current-diff-Ancestor voice-lighten-extra)
   (ediff-fine-diff-A voice-overlay-1)
   (ediff-fine-diff-B voice-overlay-2)
   (ediff-fine-diff-C voice-overlay-3)
   (ediff-fine-diff-Ancestor voice-overlay-4)
   (ediff-even-diff-A voice-brighten)
   (ediff-even-diff-B voice-smoothen)
   (ediff-even-diff-C voice-monotone-extra)
   (ediff-even-diff-Ancestor voice-monotone-extra)
   (ediff-odd-diff-A voice-smoothen)
   (ediff-odd-diff-B voice-brighten)
   (ediff-odd-diff-C voice-monotone-extra)
   (ediff-odd-diff-Ancestor voice-lighten)
   ))

;;}}}
;;{{{ Helper functions:

(defvar emacspeak-ediff-control-buffer nil
  "Holds the control buffer for the most recent ediff")
;;;Please tell me what control buffer you're using--

(defadvice ediff-setup-control-buffer (after emacspeak pre act comp)
  (setq emacspeak-ediff-control-buffer (ad-get-arg 0)))

(defsubst emacspeak-ediff-control-panel ()
  (cl-declare (special emacspeak-ediff-control-buffer))
  emacspeak-ediff-control-buffer)

(defun emacspeak-ediff-difference-a-overlay (n)
  (cl-declare (special ediff-difference-vector-A
                       ediff-number-of-differences))
  (cl-assert (< n ediff-number-of-differences) t
             "There are only %s differences"
             ediff-number-of-differences)
  (aref (aref ediff-difference-vector-A n) 0))

(defun emacspeak-ediff-difference-b-overlay (n)
  (cl-declare (special ediff-difference-vector-B
                       ediff-number-of-differences))
  (cl-assert (< n ediff-number-of-differences) t
             "There are only %s differences"
             ediff-number-of-differences)
  (aref (aref ediff-difference-vector-B n) 0))

(defun emacspeak-ediff-difference-c-overlay (n)
  (cl-declare (special ediff-difference-vector-B
                       ediff-difference-vector-C
                       ediff-number-of-differences))
  (cl-assert (< n ediff-number-of-differences) t
             "There are only %s differences"
             ediff-number-of-differences)
  (aref (aref ediff-difference-vector-C n) 0))

(defun emacspeak-ediff-fine-difference-a-overlays (n)
  (cl-declare (special ediff-difference-vector-A
                       ediff-number-of-differences))
  (cl-assert (< n ediff-number-of-differences) t
             "There are only %s differences"
             ediff-number-of-differences)
  (aref (aref ediff-difference-vector-A n) 1))

(defun emacspeak-ediff-fine-difference-b-overlays (n)
  (cl-declare (special ediff-difference-vector-B
                       ediff-number-of-differences))
  (cl-assert (< n ediff-number-of-differences) t
             "There are only %s differences"
             ediff-number-of-differences)
  (aref (aref ediff-difference-vector-B n) 1))

(defun emacspeak-ediff-fine-difference-c-overlays (n)
  (cl-declare (special ediff-difference-vector-B
                       ediff-difference-vector-C
                       ediff-number-of-differences))
  (cl-assert (< n ediff-number-of-differences) t
             "There are only %s differences"
             ediff-number-of-differences)
  (aref (aref ediff-difference-vector-C n) 1))

(defun emacspeak-ediff-difference-fine-diff (difference)
  (aref difference 2))

;;}}}
;;{{{ Diff Overlay Accessors:

(defun emacspeak-ediff-diff-overlay-from-difference (diff counter)
  (aref (aref diff counter) 0))

(defun emacspeak-ediff-fine-overlays-from-difference (diff counter)
  (aref (aref diff counter) 1))

;;}}}
;;{{{ Setup Ediff Hook

(add-hook
 'ediff-startup-hook
 #'(lambda ()
     (cl-declare (special ediff-mode-map voice-lock-mode))
     (setq voice-lock-mode t
           ediff-window-setup-function 'ediff-setup-windows-plain)
     (define-key ediff-mode-map "." 'emacspeak-ediff-speak-current-difference)))

;;}}}
;;{{{ Speak an ediff difference:

;;; To speak an ediff difference,
;;; First announce difference a and speak it.
;;; If you see keyboard activity, shut up
;;; and offer to speak difference b.

(defun emacspeak-ediff-speak-difference (n)
  "Speak a difference chunk"
  (let ((a-overlay (emacspeak-ediff-difference-a-overlay n))
        (b-overlay (emacspeak-ediff-difference-b-overlay n))
        (key ""))
    (emacspeak-auditory-icon 'select-object)
    (dtk-speak
     (concat
      "Difference ai "
      (emacspeak-overlay-get-text a-overlay)))
    (let ((dtk-stop-immediately nil))
      (sit-for 2)
      (setq key
            (read-key-sequence "Press any key to continue")))
    (unless (= 7 (string-to-char key))
      (dtk-stop)
      (dtk-speak
       (concat
        "Difference B "
        (emacspeak-overlay-get-text b-overlay))))))

(defun emacspeak-ediff-speak-current-difference ()
  "Speak the current difference"
  (interactive)
  (cl-declare (special ediff-current-difference
                       ediff-number-of-differences))
  (emacspeak-ediff-speak-difference
   (cond
    ((cl-minusp ediff-current-difference) 0)
    ((>= ediff-current-difference ediff-number-of-differences)
     (1- ediff-number-of-differences))
    (t ediff-current-difference))))

;;}}}
;;{{{ Advice:

(defadvice ediff-toggle-help (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'help)))

(defadvice ediff-next-difference (after emacspeak pre act comp)
  "Speak the difference interactively."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-ediff-speak-current-difference)))

(defadvice ediff-previous-difference (after emacspeak pre act comp)
  "Speak the difference interactively."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-ediff-speak-current-difference)))

(defadvice ediff-status-info (after emacspeak pre act comp)
  "Speak the status information"
  (when (ems-interactive-p)
    (save-current-buffer
      (set-buffer " *ediff-info*")
      (emacspeak-speak-buffer))))

(defadvice ediff-scroll-up (after emacspeak pre act comp)
  "speak"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (message "Scrolled up buffers A and B")))

(defadvice ediff-scroll-down (after emacspeak pre act comp)
  "speak"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (message "Scrolled down buffers A and B")))

(defadvice ediff-toggle-split (after emacspeak pre act comp)
  "speak"
  (when (ems-interactive-p)
    (if (eq ediff-split-window-function 'split-window-vertically)
        (message "Split ediff windows vertically")
      (message "Split ediff windows horizontally"))))

(defadvice ediff-recenter (after emacspeak pre act comp)
  "Speak"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (message "Refreshed the ediff display")))

(defadvice ediff-jump-to-difference (after emacspeak pre act comp)
  "Speak the difference you jumped to"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-ediff-speak-current-difference)))

(defadvice ediff-jump-to-difference-at-point (after emacspeak pre act comp)
  "speak"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-ediff-speak-current-difference)))

;;; advice meta panel
(defadvice ediff-previous-meta-item (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))
(defadvice ediff-next-meta-item (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defadvice ediff-registry-action (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'open-object)))

(defadvice ediff-show-registry (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Welcome to the Ediff registry")))

(defadvice ediff-toggle-filename-truncation (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (message "turned %s file name truncation in Ediff registry"
             ediff-meta-truncate-filenames)))

;;}}}
;;{{{Hooks:

(add-hook
 'ediff-mode-hook
 #'(lambda ()
     (emacspeak-speak-mode-line)
     (emacspeak-auditory-icon 'open-object)))

;;}}}
(provide 'emacspeak-ediff)
;;{{{ emacs local variables

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
