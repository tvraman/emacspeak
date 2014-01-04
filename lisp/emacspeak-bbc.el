;;; emacspeak-bbc.el --- Speech-enabled  BBC client
;;; $Id: emacspeak-bbc.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable BBC An Emacs Interface to bbc
;;; Keywords: Emacspeak,  Audio Desktop bbc
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

;;;Copyright (C) 1995 -- 2011, T. V. Raman
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
;;; MERCHANTABILITY or FITNBBC FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary: BBC: http://www.bbc.co.uk This module uses
;;; publicly available REST APIs to implement a native Emacs
;;; client for browsing and listening to BBC programs.

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'emacspeak-webutils)
(require 'g-utils)

;;}}}
;;{{{ Customizations

(defgroup emacspeak-bbc nil
  "Bbc Access on the Complete Audio Desktop."
  :group 'emacspeak)

;;}}}
;;{{{ Variables:

;;}}}
;;{{{ Helpers:
(defvar emacspeak-bbc-json-schedules-template 
"http://www.bbc.co.uk/%s/programmes/schedules/%s%s.json"
"URL template for pulling schedules as json.")

(defun emacspeak-bbc-get-schedules-url (station outlet date)
  "Return schedule for specified station, outlet, date.
Date defaults to today."
  (interactive
   (list
    (read-from-minibuffer "Station:")
    (read-from-minibuffer "Outlet:")
    (emacspeak-url-template-date-year/month/date)))
  (declare (special emacspeak-bbc-json-schedules-template))
  (format emacspeak-bbc-json-schedules-template
          station
          (if (= (length outlet) 0) "" (format "%s/" outlet))
          date))
                  
;;}}}

(provide 'emacspeak-bbc)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
