;;; emacspeak-ansi-color.el --- Speech-enable ansi-color terminal  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak module for ansi-color
;;; Keywords: Emacspeak, ansi-color
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2007-12-09 17:51:46 -0800 (Sun, 09 Dec 2007) $ |
;;;  $Revision: 4502 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1995 -- 2018, T. V. Raman
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

;;{{{  Introduction:

;;; Commentary:

;;; Module ansi-color (bundled with Emacs 21)
;;; handles ansi escape sequences and turns them into
;;; appropriate faces.
;;;This is useful in things like shell buffers.
;;; This module maps ansi codes to the appropriate voices.
;;; Code:

;;}}}
;;{{{ required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ advice interactive commands

(defadvice ansi-color-for-comint-mode-on (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'on)
    (message "Ansi escape sequences will be processed.")))

(defadvice ansi-color-for-comint-mode-off (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'off)
    (message "Ansi escape sequences will not be processed.")))

;;}}}
(provide 'emacspeak-ansi-color)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
