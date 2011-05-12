;;; g-health.el --- Health Google  Client
;;;$Id: ghealth.el 5798 2008-08-22 17:35:01Z tv.raman.tv $
;;; $Author: raman $
;;; Description:  Health that all clients start from.
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

;;; Google Health Via Atom 

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

(defgroup ghealth nil
  "Google health"
  :group 'g)

(defcustom ghealth-user-email nil
  "Mail address that identifies calendar user."
  :type '(choice
          (const :tag "none" nil)
          (string :tag "username@gmail.com" ""))
  :group 'ghealth)

(defcustom ghealth-user-password nil
  "Password for authenticating to calendar account."
  :type '(radio (const :tag "Prompt for password" nil)
                (string :tag "Save password in .emacs"))
  :group 'ghealth)

;;}}}
;;{{{ Constants

(defconst ghealth-service-name "health"
  "Service name for accessing Google health.")

(defsubst ghealth-p (service)
  "Check if this is Health."
  (declare (special ghealth-service-name))
  (string-equal service ghealth-service-name))

;;}}}
;;{{{ health Authenticate

(defsubst make-ghealth-auth ()
  "Make a new ghealth auth handle."
  (declare (special ghealth-service-name
                    ghealth-user-email ghealth-user-password))
  (make-g-auth :service ghealth-service-name
               :email ghealth-user-email
               :password ghealth-user-password))

(defvar ghealth-auth-handle (make-ghealth-auth)
  "G auth handle used for signing into health.")

(defun ghealth-authenticate ()
  "Authenticate into Google Health."
  (declare (special ghealth-auth-handle))
  (g-authenticate ghealth-auth-handle))

;;}}}
;;{{{ Feed of feeds:

(defvar ghealth-feeds-template-url
  "'https://www.google.com/health/feeds/%s'"
  "URL template for feed of feeds from health.")

(defsubst ghealth-profile-list-url ()
  "Return url for profile list."
  (declare (special ghealth-feeds-template-url))
  (format ghealth-feeds-template-url "profile/list"))

(defun ghealth-profile-list ()
  "Retrieve and display profile list  after authenticating."
  (interactive)
  (declare (special ghealth-auth-handle
                    g-atom-view-xsl
                    g-curl-program g-curl-common-options
                    g-cookie-options))
  (g-auth-ensure-token ghealth-auth-handle)
  (g-display-result
   (format
    "%s %s %s %s '%s' 2>/dev/null"
    g-curl-program g-curl-common-options
    g-cookie-options
    (g-authorization ghealth-auth-handle)
    (ghealth-profile-list-url))
   g-atom-view-xsl))

;;}}}
;;{{{ Sign out:
;;;###autoload
(defun ghealth-sign-out()
  "Resets client so you can start with a different userid."
  (interactive)
  (declare (special ghealth-auth-handle
                    ghealth-user-email ghealth-user-password))
  (message "Signing out %s from Health"
           (g-auth-email ghealth-auth-handle))
  (setq ghealth-user-email nil
        ghealth-user-password nil)
  (setq ghealth-auth-handle (make-ghealth-auth)))

;;;###autoload
(defun ghealth-sign-in()
  "Resets client so you can start with a different userid."
  (interactive)
  (declare (special ghealth-auth-handle ghealth-user-email ))
  (setq ghealth-user-email
        (read-from-minibuffer "User Email:"))
  (setq ghealth-auth-handle (make-ghealth-auth))
  (g-authenticate ghealth-auth-handle))

;;}}}
(provide 'ghealth)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
