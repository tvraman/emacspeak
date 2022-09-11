;;; emacspeak-syslog.el --- Speech-enable SYSLOG-MODE  -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Description:  Speech-enable SYSLOG-MODE An Emacs Interface to syslog-mode
;; Keywords: Emacspeak,  Audio Desktop syslog-mode
;;{{{  LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;; 
;;  $Revision: 4532 $ |
;; Location undetermined
;; 

;;}}}
;;{{{  Copyright:

;; Copyright (C) 1995 -- 2007, 2019, T. V. Raman
;; All Rights Reserved.
;; 
;; This file is not part of GNU Emacs, but the same permissions apply.
;; 
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNSYSLOG-MODE FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;; SYSLOG-MODE ==  Working with various log files.
;; Install package syslog-mode from melpa.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Map Faces:

(voice-setup-add-map 
 '(
   (syslog-debug voice-animate)
   (syslog-error voice-bolden)
   (syslog-file voice-smoothen-extra)
   (syslog-hide voice-annotate)
   (syslog-hour voice-monotone-extra)
   (syslog-info voice-animate)
   (syslog-ip voice-lighten)
   (syslog-su voice-bolden)
   (syslog-warn voice-bolden)))

;;}}}
;;{{{ Interactive Commands:

(defadvice syslog-whois-reverse-lookup (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-audit 'task-done)
    (message "Displayed WhoIs data in other window.")
    ))

(defadvice syslog-filter-dates (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (forward-line -2)
    (what-line)
    (emacspeak-auditory-icon 'ellipses)))

(defadvice syslog-filter-lines (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'ellipses)))

(defadvice syslog-boot-start (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(cl-loop
 for f in 
 '(
   syslog-append-files syslog-prepend-files 
   syslog-next-file syslog-previous-file
   syslog-move-next-file syslog-move-previous-file syslog-open-files)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-speak-mode-line)
       (emacspeak-auditory-icon 'open-object)))))

;;}}}
;;{{{keymap setup:
(defun emacspeak-syslog-setup ()
  "Setup keybindings."
  (cl-declare (special syslog-mode-map))
  (define-key syslog-mode-map ","  'emacspeak-speak-previous-field)
  (define-key syslog-mode-map "."  'emacspeak-speak-next-field))

(add-hook 'syslog-mode-load-hook #'emacspeak-syslog-setup)
;;}}}
(provide 'emacspeak-syslog)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}
