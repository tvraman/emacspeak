;;; emacspeak-gh-explorer.el --- Speech-enable GH-EXPLORER  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable GH-EXPLORER An Emacs Interface to gh-explorer
;;; Keywords: Emacspeak,  Audio Desktop gh-explorer
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
;;; MERCHANTABILITY or FITNGH-EXPLORER FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; GH-EXPLORER ==  GitHub Explorer 
;;; This module speech-enables Github Explorer.
;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Map Faces:

(voice-setup-add-map 
 '((github-explorer-directory-face voice-bolden-medium)))

;;}}}
;;{{{ Interactive Commands:

(cl-loop
 for f in 
 '(github-explorer github-explorer-at-point)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'open-object)))))

(defun ems--gh-explorer-nav (direction)
  "Move forward/back based on `direction' and speak current entry."
  (emacspeak-auditory-icon 'select-object)
  (forward-line direction)
  (save-excursion
    (goto-char (line-beginning-position))
    (let ((path (cdr (assoc 'path (get-text-property (point) 'invisible))))
          (type (cdr (assoc 'type (get-text-property (point) 'invisible)))))
      (cond
       ((null path) (emacspeak-speak-line))
       (t
        (dtk-speak
         (propertize path 'personality
                     (when (string= type "tree") voice-bolden-medium))))))))

(defun emacspeak-gh-explorer-next ()
  "Move forward and speak current entry."
  (interactive)
  (ems--gh-explorer-nav 1))

(defun emacspeak-gh-explorer-previous ()
     "Moveback and speak current entry."
     (interactive)
     
     (ems--gh-explorer-nav -1))

(eval-after-load
    "github-explorer"
  `(progn
     (cl-declare (special github-explorer-mode-map))
     (define-key github-explorer-mode-map "p" 'emacspeak-gh-explorer-previous)
     (define-key github-explorer-mode-map "n" 'emacspeak-gh-explorer-next))
  )

;;}}}
(provide 'emacspeak-gh-explorer)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
