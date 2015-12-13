;;; soundscape.el --- Soundscapes for The Emacspeak Desktop
;;; Description:  Soundscapes Using Boodler
;;; Keywords: Emacspeak,  Audio Desktop Soundscapes
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
;;; http://boodler.org is a Python-based SoundScape generator.
;;; This module defines Emacspeak conveniences for running installed Soundscapes.

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(declaim  (optimize  (safety 0) (speed 3)))

;;}}}
;;{{{ Configuration:

(defconst soundscape-player (executable-find "boodler")
  "Soundscape player. Looks for installed boodler.")

(defconst soundscape-mgr (executable-find "boodle-mgr")
  "Soundscape manager. Looks for installed boodler.")

(defconst soundscape-list (expand-file-name "soundscapes"  "~/.boodler")
  "Soundscape player. Looks for installed boodler.")

(defvar soundscape-catalog nil
  "Catalog of installed soundscapes keyed by agent name.")

;;}}}
;;{{{ Catalog:

(defun soundscape-catalog (&optional refresh)
  "Return catalog of installed Soundscapes, initialize if necessary."
  (declare (special soundscape-catalog soundscape-list))
  (cond
   ((or soundscape-catalog (null refresh)) soundscape-catalog)
   ((null (file-exists-p soundscape-list))
    (error "Soundscape catalog not initialized."))
   (t
    (let ((name nil)
          (path nil))
    (with-temp-buffer
      (insert-file-contents soundscape-list)
      (goto-char (point-min))
      (while (not (eobp))
        (setq path (buffer-substring (line-beginning-position) (line-end-position)))
        (setq name (second (split-string path "/")))
        (when (and name path)
          (push (cons name path) soundscape-catalog))
        (forward-line 1))))
    soundscape-catalog)))
(defun soundscape-read ()
  "Read name of Soundscape with completion."
  (let ((completion-ignore-case t))
  (cdr
   (assoc
    (completing-read "Soundscape: "
                     (soundscape-catalog))
    (soundscape-catalog)))))

;;}}}
(provide 'soundscape)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
