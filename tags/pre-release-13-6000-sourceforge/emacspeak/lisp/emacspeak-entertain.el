;;; emacspeak-entertain.el --- Speech enable misc games
;;; $Id$
;;; $Author$ 
;;; Description: Auditory interface to diversions
;;; Keywords: Emacspeak, Speak, Spoken Output, games
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

;;; Copyright (c) 1995 -- 2000, T. V. Raman
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

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
(eval-when (compile)
  (condition-case nil
      (require 'tetris)
    (error nil)))

;;}}}
;;{{{  Introduction 

;;; Commentary:

;;; Auditory interface to misc games

;;}}}
            
;;{{{ doctar

(defadvice doctor-txtype (after emacspeak pre act )
  (dtk-speak
   (mapconcat
    (function (lambda (s)
                (format "%s" s)))
    (ad-get-arg 0 )
    " ")))

;;}}}
;;{{{ mpuz

(declaim (special mpuz-silent ))
(setq mpuz-silent t )
(defadvice mpuz-correct-guess (after emacspeak pre act )
               "Provide an auditory icon"
               (emacspeak-auditory-icon 'search-hit ))

(defadvice mpuz-congratulate (after emacspeak pre act )
               "Produce auditory icon"
               (emacspeak-auditory-icon 'alarm ))

;;}}}
;;{{{ dunnet 

(defadvice dun-parse (around emacspeak pre act comp)
  "Provide auditory feedback"
  (cond
   ((interactive-p)
    (let ((orig (point)))
      ad-do-it
      (emacspeak-auditory-icon 'mark-object)
      (emacspeak-speak-region orig (point))))
   (t ad-do-it))
  ad-return-value)

;;}}}
(provide 'emacspeak-entertain)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
