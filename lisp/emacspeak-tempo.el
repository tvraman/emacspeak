;;; emacspeak-tempo.el --- Speech enable tempo -- template library used for Java and HTML authoring  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $ 
;;; Description:  Emacspeak extensions for tempo.el (used by html-helper-mode)
;;; Keywords: Emacspeak, Spoken Feedback, Template filling, html editing
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com 
;;; A speech interface to Emacs |
;;; $Date: 2007-08-25 18:28:19 -0700 (Sat, 25 Aug 2007) $ |
;;;  $Revision: 4532 $ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2021, T. V. Raman 
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
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}

;;{{{  Introduction:
;;; Commentary:
;;; tempo.el provides the
;;; infrastructure  for building up templates.
;;; This is used by html-helper-mode to allow for easy writing of HTML
;;; This module extends Emacspeak to provide fluent spoken feedback
;;; Code:
;;}}}
;;{{{ requires
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{  First setup tempo variables:

;;; Prompting in the minibuffer is useful:

(cl-declaim  (special tempo-interactive))
(setq tempo-interactive t)
(add-hook
 'tempo-insert-string-hook
 #'(lambda (string)
     (dtk-speak string)
     string))

;;}}}
;;{{{  Advice: 

(defadvice tempo-forward-mark (after emacspeak pre act comp)
  "Speak the line."
  (when (ems-interactive-p)
    (emacspeak-speak-line)))

(defadvice tempo-backward-mark (after emacspeak pre act comp)
  "Speak the line."
  (when (ems-interactive-p)
    (emacspeak-speak-line)))

(defadvice html-helper-smart-insert-item  (after emacspeak pre act comp)
  "Speak the line."
  (when (ems-interactive-p)
    (emacspeak-speak-line)))

;;}}}
(emacspeak-pronounce-add-super 'sgml-mode 'html-helper-mode)

(provide 'emacspeak-tempo)

;;{{{ end of file 

;; local variables:
;; folded-file: t
;; end: 

;;}}}

;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}
