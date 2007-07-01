;;; g-skeleton.el --- Skeleton Google  Client
;;;$Id: gcal.el,v 1.30 2006/09/28 17:47:44 raman Exp $
;;; $Author: raman $
;;; Description:  Skeleton that all clients start from.
;;; Keywords: Google   Atom API
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; gcal| T. V. Raman |raman@cs.cornell.edu
;;; An emacs interface to Reader|
;;; $Date: 2006/09/28 17:47:44 $ |
;;;  $Revision: 1.30 $ |
;;; Location undetermined
;;; License: GPL
;;;

;;}}}
;;{{{ Copyright:

;;; Copyright (c) 2006 and later, Google Inc.
;;; All rights reserved.

;;; Redistribution and use in source and binary forms, with or without modification,
;;; are permitted provided that the following conditions are met:

;;;     * Redistributions of source code must retain the above copyright notice,
;;;       this list of conditions and the following disclaimer.
;;;     * Redistributions in binary form must reproduce the above copyright notice,
;;;       this list of conditions and the following disclaimer in the documentation
;;;       and/or other materials provided with the distribution.
;;;     * The name of the author may not be used to endorse or promote products
;;;       derived from this software without specific prior written permission.

;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
;;; WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;{{{  introduction

;;; <insert description here>
;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'calendar)
(require 'g-utils)
(require 'g-auth)
(require 'browse-url)

;;}}}
;;{{{ Customizations

(defgroup gskeleton nil
  "Google skeleton"
  :group 'g)

(defcustom gskeleton-user-email nil
  "Mail address that identifies calendar user."
  :type '(choice
          (const :tag "none" nil)
          (string :tag "username@gmail.com" ""))
  :group 'gskeleton)

(defcustom gskeleton-user-password nil
  "Password for authenticating to calendar account."
  :type '(radio (const :tag "Prompt for password" nil)
                (string :tag "Save password in .emacs"))
  :group 'gskeleton)

;;}}}
;;{{{ Constants

(defconst gskeleton-service-name "skel"
  "Service name for accessing Google skeleton.")

(defsubst gskeleton-p (service)
  "Check if this is Calendar."
  (declare (special gskeleton-service-name))
  (string-equal service gskeleton-service-name))

;;}}}
;;{{{ skeleton Authenticate

(defsubst make-gskeleton-auth ()
  "Make a new gskeleton auth handle."
  (declare (special gskeleton-service-name
                    gskeleton-user-email gskeleton-user-password))
  (make-g-auth :service gskeleton-service-name
               :email gskeleton-user-email
               :password gskeleton-user-password))

(defvar gskeleton-auth-handle (make-gskeleton-auth)
  "G auth handle used for signing into calendar.")

(defun gskeleton-authenticate ()
  "Authenticate into Google Calendar."
  (declare (special gskeleton-auth-handle))
  (g-authenticate gskeleton-auth-handle))

;;}}}
;;{{{ Feed of feeds:

(defvar gskeleton-feeds-template-url
  "'https://www.google.com/calendar/feeds/%s'"
  "URL template for feed of feeds from calendar.")
(defsubst gskeleton-feeds-url (userid)
  "Return url for feed of feeds."
  (declare (special gskeleton-feeds-template-url))
  (format gskeleton-feeds-template-url userid))

(defun gskeleton-skels ()
  "Retrieve and display feed of feeds after authenticating."
  (interactive)
  (declare (special gskeleton-auth-handle
                    g-atom-view-xsl
                    g-curl-program g-curl-common-options
                    g-cookie-options))
  (g-auth-ensure-token gskeleton-auth-handle)
  (g-display-result
   (format
    "%s %s %s %s '%s' 2>/dev/null"
    g-curl-program g-curl-common-options
    g-cookie-options
    (g-authorization gskeleton-auth-handle)
    (gskeleton-feeds-url
     (g-url-encode (g-auth-email gskeleton-auth-handle))))
   g-atom-view-xsl))

;;}}}
;;{{{ Sign out:
;;;###autoload
(defun gskeleton-sign-out()
  "Resets client so you can start with a different userid."
  (interactive)
  (declare (special gskeleton-auth-handle
                    gskeleton-user-email gskeleton-user-password))
  (message "Signing out %s from Calendar"
           (g-auth-email gskeleton-auth-handle))
  (setq gskeleton-user-email nil
        gskeleton-user-password nil)
  (setq gskeleton-auth-handle (make-gskeleton-auth)))

;;;###autoload
(defun gskeleton-sign-in()
  "Resets client so you can start with a different userid."
  (interactive)
  (declare (special gskeleton-auth-handle gskeleton-user-email ))
  (setq gskeleton-user-email
        (read-from-minibuffer "User Email:"))
  (setq gskeleton-auth-handle (make-gskeleton-auth))
  (g-authenticate gskeleton-auth-handle))

;;}}}
(provide 'gskeleton)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
