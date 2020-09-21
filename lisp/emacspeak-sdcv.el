;;; emacspeak-sdcv.el --- Speech-enable SDCV  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable SDCV An Emacs Interface to sdcv
;;; Keywords: Emacspeak,  Audio Desktop sdcv
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
;;;Copyright (C) 1995 -- 2007, 2019, T. V. Raman
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
;;; MERCHANTABILITY or FITNSDCV FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; SDCV ==  Stardict  Dictionary Interface
;;; This module sets up Emacspeak for use with sdcv.
;;; You need to have  command-line sdcv installed.
;;; You can install additional stardict dictionaries, see
;;;  https://wiki.archlinux.org/index.php/sdcv
;;; This module sets up Emacs module sdcv to use all the installed
;;; dictionaries found on the system.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(require 'let-alist)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Interactive Commands:

(cl-loop
 for f in 
 '(sdcv-
   search-input sdcv-search-input+ sdcv-search-pointer sdcv-search-pointer+)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'task-done)))))



(cl-loop
 for f in
 '(sdcv-previous-dictionary sdcv-next-dictionary)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'large-movement)))))

(cl-loop
 for f in
 '(sdcv-next-line sdcv-prev-line)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'select-object)))))




(defun emacspeak-sdcv-update-dictionary-list ()
  "Update sdcv dictionary lists if necessary by examining
/usr/share/sdcv/dict"
  (cl-declare (special sdcv-dictionary-simple-list))
  (let ((installed
         (json-parse-string
          (shell-command-to-string "sdcv -jnl ")
          :object-type 'alist)))
    (setq sdcv-dictionary-simple-list
          (cl-loop
           for d across installed collect 
           (let-alist d  .name)))))

(defun emacspeak-sdcv-setup ()
  "Setup Emacspeak for SDCV."
  (cl-declare (special sdcv-mode-map))
  (emacspeak-sdcv-update-dictionary-list)
  (cl-loop
   for binding in
   '(
     ("n" sdcv-next-dictionary)
     ("p" sdcv-previous-dictionary))
   do
   (emacspeak-keymap-update sdcv-mode-map binding)))

(defadvice sdcv-quit (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))
(when (bound-and-true-p sdcv-mode-map)
  (emacspeak-sdcv-setup))

;;}}}
(provide 'emacspeak-sdcv)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
