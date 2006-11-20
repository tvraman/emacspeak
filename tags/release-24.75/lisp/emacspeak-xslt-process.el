;;; emacspeak-xslt-process.el --- speech-enable xslt-process 
;;; $Id$
;;; $Author$
;;; Description:  Emacspeak module for speech-enabling
;;; xslt-process mode
;;; Keywords: Emacspeak, xslt-process
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

;;; Copyright (C) 1999 T. V. Raman <raman@cs.cornell.edu>
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

;;{{{ required modules

(require 'emacspeak-preamble)
;;}}}
;;{{{  Introduction:

;;; Commentary:
;;; Speech-enables xslt-process mode.
;;; sourceforge: xslt-process

;;}}}
;;{{{ interactive commands 

(defadvice xslt-process-toggle-debug-mode (
                                           after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon
     (if xslt-process-debug-mode 'on 'off))
    (message "Turned %s xslt debug mode."
             (if xslt-process-debug-mode "on" "off"))))

(defadvice xslt-process-quit-debug (after emacspeak pre act
                                          comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice xslt-process-invoke-buffer-view (after emacspeak
                                                  pre act
                                                  comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (emacspeak-speak-mode-line)))

(defadvice xslt-process-invoke-browser-view (after emacspeak
                                                   pre act
                                                   comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (emacspeak-speak-mode-line)))

(defadvice xslt-process-invoke-pdf-viewer (after emacspeak
                                                 pre act
                                                 comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (emacspeak-speak-mode-line)))

(defadvice xslt-process-set-breakpoint (after emacspeak pre
                                              act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (save-excursion
      (beginning-of-line)
      (let ((start (point))
            (end (progn (end-of-line)
                        (point))))
        (ems-modify-buffer-safely
         (put-text-property start end
                            'auditory-icon 'mark-object)
         (put-text-property start end
                            'personality voice-animate))
        (emacspeak-speak-line)
        (emacspeak-auditory-icon 'mark-object)))))
(defadvice xslt-process-remove-breakpoint (after emacspeak pre
                                                 act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (save-excursion
      (beginning-of-line)
      (let ((start (point))
            (end (progn (end-of-line)
                        (point))))
        (ems-modify-buffer-safely
         (put-text-property start end
                            'auditory-icon nil))
        (emacspeak-speak-line)
        (emacspeak-auditory-icon 'deselect-object)))))

(defadvice xslt-process-do-run (after emacspeak pre act
                                      comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (message "Debug %s" ad-return-value)))
(defadvice xslt-process-do-step (after emacspeak pre act
                                       comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice xslt-process-do-next (after emacspeak pre act
                                       comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice xslt-process-do-finish (after emacspeak pre act
                                         comp)
  "Provide auditory feedback."
  (when (interactive-p)
    

    (defadvice xslt-process-do-continue (after emacspeak pre act
                                               comp)
      "Provide auditory feedback."
      (when (interactive-p)
        (emacspeak-auditory-icon 'select-object)))

    (defadvice xslt-process-do-stop (after emacspeak pre act
                                           comp)
      "Provide auditory feedback."
      (when (interactive-p)
        (emacspeak-auditory-icon 'select-object)))

    (defadvice xslt-process-do-quit (after emacspeak pre act
                                           comp)
      "Provide auditory feedback."
      (when (interactive-p)
        (emacspeak-auditory-icon 'select-object)))

    (emacspeak-auditory-icon 'close-object)))

(defadvice xslt-process-associate-stylesheet (after
                                              emacspeak pre
                                              act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

;;}}}

(provide 'emacspeak-xslt-process)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
