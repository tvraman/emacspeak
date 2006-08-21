;;; emacspeak-texinfo.el --- Speech enable texinfo mode
;;; $Id$
;;; $Author$ 
;;; Description:  Emacspeak extension to speech enable
;;; texinfo mode
;;; Keywords: Emacspeak, texinfo
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

;;{{{  Required modules 

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'advice)
(eval-when-compile (require 'dtk-speak)
                   (require 'emacspeak-speak)
                   (require 'emacspeak-sounds)
                   (require 'voice-lock))

;;}}};;{{{  Introduction:

;;; Commentary:

;;; This module speech enables net-texinfo mode

;;; Code:

;;}}}
;;{{{ voice locking
(defvar texinfo-voice-lock-keywords
  '(
    ("^\\(@c\\|@comment\\)\\>.*" . voice-lock-comment-personality) ;comments
    ("@\\([a-zA-Z]+\\|[^ \t\n]\\)" 1 voice-lock-keyword-personality) ;commands
    ("^\\*\\(.*\\)[\t ]*$" 1 voice-lock-function-name-personality t) ;menu items
    ("@\\(emph\\|strong\\|b\\|i\\){\\([^}]+\\)" 2 voice-lock-comment-personality)
    ("@\\(file\\|kbd\\|key\\|url\\|email\\){\\([^}]+\\)" 2 voice-lock-string-personality)
    ("@\\(samp\\|code\\|var\\|math\\){\\([^}]+\\)"
     2 voice-lock-variable-name-personality)
    ("@\\(cite\\|xref\\|pxref\\){\\([^}]+\\)" 2 voice-lock-constant-personality)
    ("@\\(end\\|itemx?\\) +\\(.+\\)" 2 voice-lock-function-name-personality keep)
    )
  "Additional expressions to highlight in TeXinfo mode.")

(defun emacspeak-texinfo-mode-hook ()
  "Setup Emacspeak extensions"
  (declare (special texinfo-voice-lock-keywords
                    dtk-split-caps
                    voice-lock-defaults))
  (make-local-variable 'voice-lock-defaults)
  (setq voice-lock-defaults '(texinfo-voice-lock-keywords t))
  (voice-lock-mode 1)
  (dtk-set-punctuations "all")
  (or dtk-split-caps
      (dtk-toggle-split-caps))
  (or emacspeak-audio-indentation
      (emacspeak-toggle-audio-indentation))
  (emacspeak-dtk-sync))

(add-hook 'texinfo-mode-hook 'emacspeak-texinfo-mode-hook)

;;}}}
;;{{{ advice

(defadvice texinfo-insert-@end (after emacspeak pre act
                                      comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-line)))

(defadvice TeXinfo-insert-environment (after emacspeak pre act
                                      comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice texinfo-insert-@item (after emacspeak pre act
                                      comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'item)
    (emacspeak-speak-line)))

(defadvice texinfo-insert-@node (after emacspeak pre act
                                      comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

;;}}}
(provide 'emacspeak-texinfo)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
