;;; emacspeak-net-utils.el --- Speech enable net-utils
;;; $Id$
;;; $Author$ 
;;; Description:  Emacspeak extension to speech enable net-utils
;;; Keywords: Emacspeak, network utilities 
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
;;;Copyright (C) 1995 -- 2007, T. V. Raman 
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

;;{{{  Introduction:

;;; Commentary:

;;; This module speech enables net-utils

;;}}}
;;{{{ requires
(require 'emacspeak-preamble)

;;}}}
;;{{{ advice
(defvar emacspeak-net-utils-commands
  '(arp
    route
    traceroute
    ifconfig
    ping
    netstat
    nslookup-host)
  "Commands to speech enable")

(loop for f in emacspeak-net-utils-commands
      do
      (eval
       (`
        (defadvice (, f)  (after emacspeak pre act comp)
          "Speak output"
          (when (interactive-p)
            (emacspeak-auditory-icon 'open-object)
            (message "Displayed results of %s in other window"
                     (quote (, f))))))))

;;}}}
(provide 'emacspeak-net-utils)

;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
