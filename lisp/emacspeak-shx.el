;;; emacspeak-shx.el --- Speech-enable SHX  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable SHX An Emacs Interface to shx
;;; Keywords: Emacspeak,  Audio Desktop shx
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2007, 2011, T. V. Raman
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
;;; MERCHANTABILITY or FITNSHX FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; SHX ==  Shell Extras For emacs

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)


;;}}}
;;{{{ Forward Declaration 
(declare-function shx-insert "shx" (&rest args))

;;}}}
;;{{{ Interactive Commands:

(defadvice shx (after emacspeak pre act comp)
  "Announce switching to shell mode.
Provide an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice shx-send-input (after emacspeak pre act comp)
  "Flush any ongoing speech."
  (when (ems-interactive-p)
    (dtk-stop)))

(defadvice shx-send-input-or-copy-line (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defadvice shx--turn-on (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'on)
    (message "Turned on shx")))
(defadvice shx-send-input-or-open-thing (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (unless (eq major-mode 'shell-mode)
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'open-object))))

(defadvice shx-global-mode (after emacspeak  pre act comp)
  "speak."
  (when (ems-interactive-p)
    (message "Turned %s shx globally"
             (if shx-global-mode "on" "off"))
    (emacspeak-auditory-icon
     (if shx-global-mode 'on 'off))))

(defadvice shx-magic-insert (around emacspeak pre act comp)
  "Speak word or completion."
  (cond
   ((ems-interactive-p)
    (ems-with-messages-silenced
     (let ((orig (point))
           (count (ad-get-arg 0)))
       (setq count (or count 1))
       ad-do-it
       (cond
        ((= (point) (+ count orig))
         (save-excursion
           (forward-word -1)
           (emacspeak-speak-word)))
        (t
         (emacspeak-auditory-icon 'complete)
         (emacspeak-speak-region
          (comint-line-beginning-position) (point)))))))
   (t ad-do-it))
  ad-return-value)
;;}}}
;;{{{ Additional shx commands:

(defun shx-cmd-browse (url)
  "Browse the supplied URL."
  (shx-insert "Browsing " 'font-lock-keyword-face url "\n")
  (browse-url url))

(defun shx-cmd-grep (grep-args)
  "Run grep with `grep-args'."
  (shx-insert "grep " 'font-lock-keyword-face grep-args "\n")
  (grep (concat "grep --color -nH -e " grep-args)))

;;}}}
(provide 'emacspeak-shx)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
