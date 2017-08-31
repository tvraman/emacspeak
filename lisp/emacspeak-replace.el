;;; emacspeak-replace.el --- Speech enable interactive search and replace  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $ 
;;; Description:  Emacspeak extension for replace.el
;;; Keywords: Emacspeak, Speech feedback, query replace (replace.el)
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu 
;;; A speech interface to Emacs |
;;; $Date: 2007-08-25 18:28:19 -0700 (Sat, 25 Aug 2007) $ |
;;;  $Revision: 4532 $ | 
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

;;{{{ requires
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(load-library "replace")

;;}}}
;;{{{  Introduction:
;;; Commentary:
;;; This module causes emacs' replacement functions to use voice locking
;;; Code:
;;}}}
;;{{{  define personalities

(defcustom emacspeak-replace-personality
  voice-animate
  "Personality used in search and replace to indicate word
that is being replaced."
  :group 'isearch
  :group 'emacspeak
  :type 'symbol)

;;}}}
;;{{{  Advice

(defvar emacspeak-replace-highlight-on nil
  "Flag that says if replace highlight is on.")

(defvar emacspeak-replace-saved-personality nil
  "Value saved before replace-highlight changed the personality. ")

(defvar emacspeak-replace-start nil)
(defvar emacspeak-replace-end nil)

(cl-loop
 for f in
 '(query-replace query-replace-regexp)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p) (emacspeak-auditory-icon 'task-done)))))

(defadvice perform-replace (around emacspeak pre act  comp)
  "Silence help message."
  (ems-with-messages-silenced
   ad-do-it))

(defadvice replace-highlight (before  emacspeak pre act)
  "Voicify and speak the line containing the replacement. "
  (save-match-data
    (let ((from (ad-get-arg 0))
          (to (ad-get-arg 1)))
      (condition-case nil
          (progn 
            (and emacspeak-replace-highlight-on
                 emacspeak-replace-start 
                 emacspeak-replace-end
                 (put-text-property 
                  (max emacspeak-replace-start  (point-min))
                  (min emacspeak-replace-end   (point-max))
                  'personality   emacspeak-replace-saved-personality))
            (setq emacspeak-replace-highlight-on t
                  emacspeak-replace-start from
                  emacspeak-replace-end  to 
                  emacspeak-replace-saved-personality
                  (dtk-get-style from))
            (and from to 
                 (put-text-property from to 'personality
                                    emacspeak-replace-personality))
            (dtk-stop)
            (emacspeak-speak-line))
        (error nil)))))

(defadvice replace-dehighlight (after emacspeak pre act)
  "Turn off the replacement highlight. "
  (cl-declare (special emacspeak-replace-highlight-on
                    emacspeak-replace-saved-personality
                    emacspeak-replace-start emacspeak-replace-end))
  (save-match-data
    (condition-case nil
        (progn
          (and emacspeak-replace-highlight-on
               emacspeak-replace-start
               emacspeak-replace-end
               (put-text-property 
                (max emacspeak-replace-start  (point-min))
                (min emacspeak-replace-end (point-max))
                'personality   emacspeak-replace-saved-personality)
               (setq emacspeak-replace-start nil
                     emacspeak-replace-end nil
                     emacspeak-replace-highlight-on nil)))
      (error  nil))))

;;}}}
(provide 'emacspeak-replace)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
