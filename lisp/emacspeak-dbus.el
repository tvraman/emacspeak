;;; emacspeak-dbus.el --- DBus Tools For Emacspeak Desktop
;;; $Id: emacspeak-dbus.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  DBus Tools For The Emacspeak Desktop
;;; Keywords: Emacspeak,  Audio Desktop dbus
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
;;; MERCHANTABILITY or FITNDBUS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; Set up Emacspeak to respond to DBus notifications

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'dbus)

;;}}}
;;{{{ Customize:

(defgroup emacspeak-dbus nil
  "DBus  bindings and hooks for Emacspeak desktop."
  :group 'emacspeak)

;;}}}
;;{{{ NM Handlers

(defun emacspeak-dbus-nm-connected ()
  "Announce  network manager connection."
  (declare (special emacspeak-speak-network-interfaces-list))
  (setq emacspeak-speak-network-interfaces-list (mapcar #'car (network-interface-list)))
  (emacspeak-auditory-icon 'network-up)
  (message
   (mapconcat #'identity emacspeak-speak-network-interfaces-list "")))

(defun emacspeak-dbus-nm-disconnected ()
  "Announce  network manager disconnection."
  (declare (special emacspeak-speak-network-interfaces-list))
  (setq emacspeak-speak-network-interfaces-list (mapcar #'car (network-interface-list)))
  (emacspeak-auditory-icon 'network-down)
  (message (mapconcat #'identity emacspeak-speak-network-interfaces-list "")))

(add-hook 'nm-connected-hook 'emacspeak-dbus-nm-connected)
(add-hook 'nm-disconnected-hook 'emacspeak-dbus-nm-disconnected)

;;}}}

(provide 'emacspeak-dbus)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
