;;; emacspeak-prompts.el --- Pre-Defined Speech  PROMPTS  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable PROMPTS An Emacs Interface to prompts
;;; Keywords: Emacspeak,  Audio Desktop prompts
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
;;; MERCHANTABILITY or FITNPROMPTS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; This module provides a set of pre-defined speech prompts,
;;; And a light-weight player for generating these prompts.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Configure:

(defvar emacspeak-prompts-directory 
  (expand-file-name "prompts" emacspeak-sounds-directory)
  "Where pre-defined prompt files are located.")

(defvar emacspeak-prompts-player
      (executable-find "mplayer")
  "Player used to play pre-defined prompts.")
;;;###autoload 
(defun emacspeak-prompt (name)
  "Play  prompt for specified name."
  (cl-declare (special emacspeak-prompts-directory emacspeak-prompts-player))
  (let  ((file (expand-file-name (format "%s.mp3" name)
                                 emacspeak-prompts-directory)))
    (cl-assert (file-exists-p file) t  "File does not exist")
    (call-process emacspeak-prompts-player nil  0 nil  file)))

;;}}}

(provide 'emacspeak-prompts)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
