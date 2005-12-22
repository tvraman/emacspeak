;;; emacspeak-amphetadesk.el --- Emacspeak News Portal Interface
;;; $Id$
;;; $Author$
;;; Description:  RSS Wizard for the emacspeak desktop
;;; Keywords: Emacspeak,  Audio Desktop RSS
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
;;;Copyright (C) 1995 -- 2004, T. V. Raman 
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

;;{{{  introduction

;;; AmphetaDesk --http://amphetadesk.sf.net is an RSS news aggregator.
;;; This module provides a simple emacs front-end to amphetadesk
;;; Emacspeak users should try this in conjunction with the
;;; AmphetaDesk lite theme available in the Emacspeak CVS repository 
;;; under tvr/amphetadesk

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'custom)
(require 'browse-url)
(require 'emacspeak-preamble)
(eval-when-compile
  (condition-case nil
      (require 'emacspeak-w3)
    (error nil)))
;;}}}
;;{{{ amphetadesk

(defgroup emacspeak-amphetadesk nil
  "AmphetaDesk"
  :group 'aplications)

(defcustom emacspeak-amphetadesk-program
  "/usr/local/share/amphetadesk-src-v0.93.1/AmphetaDesk.pl "
  "Script that launches amphetadesk."
  :type 'file
  :group 'emacspeak-amphetadesk)

(defcustom emacspeak-amphetadesk-port 8888
  "Port where AmphetaDesk listens."
  :type 'integer
  :group 'emacspeak-amphetadesk)
(defcustom emacspeak-amphetadesk-uri
  "http://127.0.0.1:8888/"
  "URI for Amphetadesk home."
  :type 'string
  :group 'emacspeak-amphetadesk)

(defsubst emacspeak-amphetadesk-ensure-live ()
  "Ensure AmphetaDesk is alive, and start it if necessary."
  (declare (special emacspeak-amphetadesk-program
                    emacspeak-amphetadesk-port))
  (let ((emacspeak-speak-messages nil))
    (if (=  1
	    (shell-command
	     (format "netstat -nat | grep %s"
		     emacspeak-amphetadesk-port)))
	(shell-command
	 (format "nohup %s &"
		 emacspeak-amphetadesk-program)
	 "*AmphetaDesk*"))))

;;;###autoload
(defun emacspeak-amphetadesk (&optional use-opml)
  "Open amphetadesk.
Interactive prefix-arg use-opml opens the myChannels.opml file."
  (interactive "P")
  (declare (special browse-url-browser-function
                    emacspeak-w3-post-process-hook))
  (cond
   (use-opml
    (emacspeak-opml-display
     (format "file:///%sdata/myChannels.opml"
             (file-name-directory emacspeak-amphetadesk-program))))
   (t (emacspeak-amphetadesk-ensure-live)
      (cond
       ((and (featurep 'w3)
             (or (eq browse-url-browser-function 'w3-fetch)
                 (eq browse-url-browser-function 'browse-url-w3)))
        (add-hook  'emacspeak-w3-post-process-hook
                   #'(lambda ()
                       (imenu--make-index-alist)
                       (goto-char (point-min))
                       (emacspeak-speak-mode-line)))
        (emacspeak-w3-without-xsl
         (w3-fetch "http://127.0.0.1:8888/")))
       (t (browse-url emacspeak-amphetadesk-uri))))))

;;;###autoload

(defun emacspeak-amphetadesk-quick-add (url)
  "Quick add URL to Amphetadesk by prompting for URL."
  (interactive
   (list
    (cond
     ((and (eq major-mode 'w3-mode)
           (w3-view-this-url 'no-show))
      (w3-view-this-url 'no-show))
     (t
      (read-from-minibuffer "URL:")))))
  (declare (special emacspeak-amphetadesk-uri))
  (browse-url 
   (concat emacspeak-amphetadesk-uri
           "my_channels.html?add_url="
           (webjump-url-encode
            url))))
(declaim (special w3-mode-map))
(when (boundp 'w3-mode-map)
  (define-key w3-mode-map "aa" 'emacspeak-amphetadesk-quick-add))

;;}}}
(provide 'emacspeak-amphetadesk)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
