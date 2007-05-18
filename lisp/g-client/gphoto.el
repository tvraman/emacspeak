;;; g-photo.el ---  Google  Picasa Client
;;;$Id: gcal.el,v 1.30 2006/09/28 17:47:44 raman Exp $
;;; $Author: raman $
;;; Description:   Client  For Accessing Picasa (Photo Albums)
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

;;; Emacs client for accessing Picasa --- Google Photo Albums
;;; See  http://code.google.com/apis/picasaweb/overview.html

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

(defgroup gphoto nil
  "Google photo"
  :group 'g)

(defcustom gphoto-user-email nil
  "Mail address that identifies Picasa user."
  :type '(choice
          (const :tag "none" nil)
          (string :tag "username@gmail.com" ""))
  :group 'gphoto)

(defcustom gphoto-user-password nil
  "Password for authenticating to calendar account."
  :type '(radio (const :tag "Prompt for password" nil)
                (string :tag "Save password in .emacs"))
  :group 'gphoto)

;;}}}
;;{{{ Constants

(defconst gphoto-service-name "lh2"
  "Service name for accessing Google photo.")

(defsubst gphoto-p (service)
  "Check if this is Calendar."
  (declare (special gphoto-service-name))
  (string-equal service gphoto-service-name))

;;}}}
;;{{{ photo Authenticate

(defsubst make-gphoto-auth ()
  "Make a new gphoto auth handle."
  (declare (special gphoto-service-name
                    gphoto-user-email gphoto-user-password))
  (make-g-auth :service gphoto-service-name
               :email gphoto-user-email
               :password gphoto-user-password))

(defvar gphoto-auth-handle (make-gphoto-auth)
  "G auth handle used for signing into calendar.")

(defun gphoto-authenticate ()
  "Authenticate into Google Photo."
  (declare (special gphoto-auth-handle))
  (g-authenticate gphoto-auth-handle))

;;}}}
;;{{{ Feed of feeds:

(defvar gphoto-feeds-template-url
  "'http://picasaweb.google.com/data/feed/api/%s/liz?kind=album'"
  "URL template for feed of albums from Picasa.")
(defsubst gphoto-feeds-url (userid)
  "Return url for feed of albums."
  (declare (special gphoto-feeds-template-url))
  (format gphoto-feeds-template-url userid))

(defun gphoto-albums ()
  "Retrieve and display feed of albums after authenticating."
  (interactive)
  (declare (special gphoto-auth-handle
                    g-atom-view-xsl
                    g-curl-program g-curl-common-options
                    g-cookie-options))
  (g-auth-ensure-token gphoto-auth-handle)
  (g-display-result
   (format
    "%s %s %s %s '%s' 2>/dev/null"
    g-curl-program g-curl-common-options
    g-cookie-options
    (g-authorization gphoto-auth-handle)
    (gphoto-feeds-url
     (g-url-encode (g-auth-email gphoto-auth-handle))))
   g-atom-view-xsl))

;;}}}
;;{{{ Sign out:

(defun gphoto-sign-out()
  "Resets client so you can start with a different userid."
  (interactive)
  (declare (special gphoto-auth-handle
                    gphoto-user-email gphoto-user-password))
  (message "Signing out %s from Calendar"
           (g-auth-email gphoto-auth-handle))
  (setq gphoto-user-email nil
        gphoto-user-password nil)
  (setq gphoto-auth-handle (make-gphoto-auth)))

;;}}}
(provide 'gphoto)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
