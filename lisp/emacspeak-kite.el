;;; emacspeak-kite.el --- Speech-enable KITE
;;; $Id: emacspeak-kite.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable KITE An Emacs Interface to kite
;;; Keywords: Emacspeak,  Audio Desktop kite
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
;;;Copyright (C) 1995 -- 2007, 2011, T. V. Raman
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
;;; MERCHANTABILITY or FITNKITE FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ Introduction:

;;; Commentary:
;;; kite connects Emacs to browsers that support the webkit debugging protocol.
;;; You can use this to connect to a running Chrome
;;; Make sure to start chrome with the correct command-line flag: e.g., on Linux:
;;; google-chrome --remote-debugging-port=9222
;;; You can get  Emacs package kite from here:
;;; git clone git://github.com/jscheid/kite
;;; Make sure to first install the websocket package from elpa.
;;; This package speech-enables kite for Emacspeak users.
;;; This is what I use at present when developing/debugging ChromeVox.
;;; ChromeVox == http://google-axs-chrome.googlecode.com
;;; http://chromevox.com

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Customizations:

(defgroup emacspeak-kite nil
  "Speech-enable KITE interaction."
  :group 'emacspeak)

;;}}}
;;{{{ Map Faces:
(voice-setup-add-map
 '(
   ;; kite-console.el:
   (kite-log-warning  voice-animate)
   (kite-log-error voice-bolden-and-animate)
   (kite-log-debug voice-monotone)
   (kite-log-log voice-monotone)
   (kite-object voice-bolden)
   (kite-number voice-smoothen)
   (kite-boolean voice-monotone)
   (kite-null voice-smoothen)
   (kite-undefined voice-animate)
   (kite-string voice-lighten)
   (kite-quote voice-lighten)
   (kite-loading voice-animate-extra)
   (kite-property-name voice-smoothen-medium)
   (kite-proto-property-name voice-smoothen-extra)
   (kite-console-prompt-face voice-brighten)
   (kite-stack-error-type voice-animate)
   (kite-stack-error-message voice-bolden)
   (kite-stack-function-name voice-bolden-medium)
   (kite-stack-pseudo-file-name voice-lighten)
   (kite-stack-file-name voice-lighten-medium)
   (kite-stack-line-number voice-smoothen)
   (kite-stack-column-number voice-smoothen)

   ;; kite-dom-css.el:
   ;; kite-dom.el:
   ;; kite.el:
   ;; kite-net.el:
   ;; kite-repl.el:
   ))
;;}}}
(provide 'emacspeak-kite)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
