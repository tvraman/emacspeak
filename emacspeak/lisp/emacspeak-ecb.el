;;; emacspeak-ecb.el --- speech-enable Emacs Class Browser
;;; $Id$
;;; $Author$
;;; Description:  Emacspeak module for speech-enabling Emacs
;;; Class Browser
;;; Keywords: Emacspeak, ecb
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

;;; Copyright (C) 1999 T. V. Raman <raman@cs.cornell.edu>
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

;;{{{ required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'advice)
(require 'thingatpt)
(require 'emacspeak-speak)
(require 'voice-lock)
(require 'emacspeak-sounds)

;;}}}
;;{{{  Introduction:

;;; The ECB is an Emacs Class Browser.
;;; This module speech-enables ECB

;;}}}
;;{{{  advice interactive commands.

(loop for f in 
      '( ecb-goto-window-directories 
         ecb-goto-window-sources 
         ecb-goto-window-methods 
         ecb-goto-window-history 
         ecb-goto-window-edit1 
         ecb-goto-window-edit2 )
      do
      (eval 
       (`
        (defadvice (, f) (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (interactive-p)
            (emacspeak-speak-mode-line)
            (emacspeak-auditory-icon 'select-object))))))
(emacspeak-fix-interactive-command-if-necessary 'ecb-add-source-path)

;;}}}
;;{{{  inform tree browser about emacspeak

(defadvice tree-buffer-create (after emacspeak pre act comp)
  "Fixes up keybindings so incremental tree search is
available."
  (let ((incr-search (ad-get-arg 10)))
    (when incr-search
      (substitute-key-definition 'emacspeak-self-insert-command
                                 'tree-buffer-incremental-node-search
                                 tree-buffer-key-map
                                 global-map))))

(defadvice tree-buffer-incremental-node-search 
  (around emacspeak pre act comp)
  "Track search and provide appropriate auditory feedback."
  (declare (special tree-buffer-incr-searchpattern))
  (cond
   ((interactive-p)
    (let ((start (point))
          (beg nil))
      ad-do-it
      (cond
       ((not (=  start (point)))
        (let ((emacspeak-speak-messages nil))
          (save-excursion
            (beginning-of-line)
            (setq beg (point))
            (search-forward tree-buffer-incr-searchpattern)
            (ems-modify-buffer-safely
             (put-text-property beg (point)
                                'personality 'harry)))
          (emacspeak-speak-line)
          (emacspeak-auditory-icon 'search-hit)))
       (t (emacspeak-auditory-icon 'search-miss)))))
   (t ad-do-it))
  ad-return-value)


(defadvice tree-buffer-select (after emacspeak pre act comp)
  "Provide auditory feedback."
  (emacspeak-auditory-icon 'select-object)
  (emacspeak-speak-line))

;;}}}
(provide 'emacspeak-ecb)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
