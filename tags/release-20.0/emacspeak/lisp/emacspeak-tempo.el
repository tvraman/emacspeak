;;; emacspeak-tempo.el --- Speech enable tempo -- template library used for Java and HTML authoring
;;; $Id$
;;; $Author$ 
;;; Description:  Emacspeak extensions for tempo.el (used by html-helper-mode)
;;; Keywords: Emacspeak, Spoken Feedback, Template filling, html editing
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
;;;Copyright (C) 1995 -- 2003, T. V. Raman 
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

;;{{{  Introduction:

;;; tempo.el provides the
;;; infrastructure  for building up templates.
;;; This is used by html-helper-mode to allow for easy writing of HTML
;;; This module extends Emacspeak to provide fluent spoken feedback

;;}}}
;;{{{ requires
(require 'emacspeak-preamble)

;;}}}
;;{{{  First setup tempo variables:

;;; Prompting in the minibuffer is useful:

(declaim  (special tempo-interactive ))
(setq tempo-interactive t)
(add-hook 'tempo-insert-string-hook
          (function (lambda (string)
                      (dtk-speak string)
                      string )))

;;}}}
;;{{{  Advice: 

(defadvice tempo-forward-mark (after emacspeak pre act)
  "Speak the line."
  (when (interactive-p)
    (emacspeak-speak-line)))

(defadvice tempo-backward-mark (after emacspeak pre act)
  "Speak the line."
  (when (interactive-p)
    (emacspeak-speak-line)))

(defadvice html-helper-smart-insert-item  (after emacspeak pre act)
  "Speak the line."
  (when (interactive-p)
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
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
