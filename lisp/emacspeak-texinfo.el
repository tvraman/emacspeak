;;; emacspeak-texinfo.el --- Speech enable texinfo mode  -*- lexical-binding: t; -*-
;;
;; $Author: tv.raman.tv $ 
;; Description:  Emacspeak extension to speech enable
;; texinfo mode
;; Keywords: Emacspeak, texinfo
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
;; Copyright (C) 1995 -- 2021, T. V. Raman 
;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  Required modules 
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
;;}}}
;;{{{  Introduction:

;;; Commentary:

;; This module speech enables net-texinfo mode

;;; Code:

;;}}}
;;{{{ voice locking

(defun emacspeak-texinfo-mode-hook ()
  "Setup Emacspeak extensions"
  (cl-declare (special dtk-split-caps))
  (dtk-set-punctuations 'all)
  (or dtk-split-caps
      (dtk-toggle-split-caps))
  (or emacspeak-audio-indentation
      (emacspeak-toggle-audio-indentation)))

(add-hook 'texinfo-mode-hook 'emacspeak-texinfo-mode-hook)

;;}}}
;;{{{ advice

(defadvice texinfo-insert-@end (after emacspeak pre act
                                      comp)
  "speak"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-line)))

(defadvice TeXinfo-insert-environment (after emacspeak pre act
                                             comp)
  "speak"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice texinfo-insert-@item (after emacspeak pre act
                                       comp)
  "speak"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'item)
    (emacspeak-speak-line)))

(defadvice texinfo-insert-@node (after emacspeak pre act
                                       comp)
  "speak"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

;;}}}
(provide 'emacspeak-texinfo)
;;{{{ end of file 

;; local variables:
;; folded-file: t
;; end: 

;;}}}
