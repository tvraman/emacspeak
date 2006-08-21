;;; cd-tool.el --- Play  CDs from Emacs
;;;$Id$
;;;Emacs front-end to CDTool
;;{{{  Copyright:

;;; Copyright (C) 1995 -- 2003, T. V. Raman<raman@cs.cornell.edu>
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
;;{{{ introduction

;;; Commentary:
;;; Provide an emacs front-end to cdtool.
;;; cdtool can be obtained as an rpm
;;; check using rpmfind
;;; or from its home site at 
;;;   sunsite.unc.edu /pub/Linux/apps/sound/cdrom/cli
;;; This module also provides the ability to play or save 
;;; clips from a CD if you have cdda2wav installed.
;;; cdda2wav is a cd to wav convertor.
;;;

;;}}}
;;{{{ required packages

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))

;;}}}
;;{{{ top level 

;;; Code:

(defvar cd-tool-message
  " +Next  - Previous  p play s stop = shuffle i info e eject t track"
  "Short message to display if user hits invalid key.")              
;;;###autoload
(defun cd-tool ()
  "Front-end to CDTool.
Bind this function to a convenient key-
Emacspeak users automatically have 
this bound to <DEL> in the emacspeak keymap.

Key     Action
---     ------

+       Next Track
-       Previous Track
SPC     Pause or Resume
e       Eject
=       Shuffle
i       CD Info
p       Play
s       Stop
t       track
c       clip
cap C   Save clip to disk
"
  (interactive)
  (let ((command nil))
    (while (null command)
      (setq command
            (case (read-char "CD Action? ")
              (?+ "cdstart +")
              (?> "cdstart +")
              (?. "cdstart +")
              (?- "cdstart -")
              (?< "cdstart -")
              (?, "cdstart -")
              (?t (format "cdstart %s"
                          (read-from-minibuffer "Enter track
number: ")))
              (?p "cdstart")
              (?s "cdstop")
              (?= "cdshuffle")
              (?\ "cdpause")
              (?r "cdstart")
              (?i "cdir ")
              (?e "cdeject")
              (?c (cd-tool-get-clip-command))
              (?C (cd-tool-get-clip-command 'save))
              (otherwise (message cd-tool-message)
			 (sit-for 5)
			 nil))))
    (shell-command
     (format "%s &"
             command ))))

(defvar cd-tool-clipper "cdda2wav"
  "Program that can clip CD audio.")
(defvar cd-tool-clip-track-history nil
  "Used to record trac used in clipping.")

(defvar cd-tool-clip-skip-history nil
  "Used to record history of sectors skipped.")

(defvar cd-tool-clip-duration-history nil
  "Used to record history of previous clip duration.")

(defvar cd-tool-clipper-default-args
  "-D /dev/cdrom "
  "Default command line arguments to cdda2wav.")

(defun cd-tool-get-clip-command (&optional save)
  "Query for and return an appropriate CD clip command"
  (declare (special cd-tool-clipper
                    cd-tool-clip-track-history
                    cd-tool-clip-skip-history
                    cd-tool-clip-duration-history))
  (let ((filename (when save
                    (read-file-name
                     "File name to save clip to: ")))
        (track (read-from-minibuffer(format 
                                     "Track to clip%s: "
                                     (if save  "to file" ""))
                                    (car cd-tool-clip-track-history) ;INITIAL-CONTENTS
                                    nil ;KEYMAP
                                    nil ; READ
                                    cd-tool-clip-track-history))
        (skip (read-from-minibuffer"Skip sectors: "
                                   (car cd-tool-clip-skip-history ) ;INITIAL-CONTENTS
                                   nil  ;KEYMAP
                                   nil  ; READ
                                   cd-tool-clip-skip-history))
        (duration  (read-from-minibuffer"Duration: "
                                        (car cd-tool-clip-duration-history ) ;INITIAL-CONTENTS
                                        nil ;KEYMAP
                                        nil ; READ
                                        cd-tool-clip-duration-history)))
    (pushnew track cd-tool-clip-track-history)
    (pushnew  skip cd-tool-clip-skip-history)
    (pushnew duration cd-tool-clip-duration-history)
    (format "%s %s -t %s -o %s -d %s %s"
	    cd-tool-clipper
	    cd-tool-clipper-default-args 
	    track skip duration
	    (if save filename "-e"))))

;;}}}

(provide 'cd-tool)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
