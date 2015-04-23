;;; $Id: sox-gen.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  collection of SoX  sound generators
;;; Keywords: Emacspeak,  Audio Desktop sox
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2015, T. V. Raman
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
;;; MERCHANTABILITY or FITNSOX FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; Contains a collection of functions that generate sound using SoX.
;;; These functions are primarily for use from other Emacs/Emacspeak modules.
;;; This module can be used independent of Emacspeak. 

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'sox)

;;}}}
;;{{{ Sound Generators:
(defconst sox-chime-cmd
  "play -n synth -j 3 sin %3 sin %-2 sin %-5 sin %-9 \
                   sin %-14 sin %-21 fade h .01 2 1.5 delay \
                   1.3 1 .76 .54 .27 remix - fade h 0 2.7 2.5 norm -1"
"Command-line that produces a simple chime.")

(defun sox-chime (&optional tempo speed)
  "Play chime --- optional args tempo and speed default to 1."
  (declare (special sox-chime-cmd))
  (shell-command
   (concat
    sox-chime-cmd
    (when tempo (format " tempo %s" tempo))
    (when speed (format " speed %s" speed))
    " 2>&1 > /dev/null &")))

;;}}}
(provide 'sox-gen)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
