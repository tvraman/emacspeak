;;; g-finance.el --- Finance Google  Client
;;;$Id$
;;; $Author: raman $
;;; Description:  Google Finance from emacs
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

(defgroup gfinance nil
  "Google finance"
  :group 'g)

(defcustom gfinance-user-email nil
  "Mail address that identifies calendar user."
  :type '(choice
          (const :tag "none" nil)
          (string :tag "username@gmail.com" ""))
  :group 'gfinance)

(defcustom gfinance-user-password nil
  "Password for authenticating to calendar account."
  :type '(radio (const :tag "Prompt for password" nil)
                (string :tag "Save password in .emacs"))
  :group 'gfinance)

;;}}}
;;{{{ Constants

(defconst gfinance-service-name "finance"
  "Service name for accessing Google finance.")

(defsubst gfinance-p (service)
  "Check if this is Calendar."
  (declare (special gfinance-service-name))
  (string-equal service gfinance-service-name))

;;}}}
;;{{{ finance Authenticate

(defsubst make-gfinance-auth ()
  "Make a new gfinance auth handle."
  (declare (special gfinance-service-name
                    gfinance-user-email gfinance-user-password))
  (make-g-auth :service gfinance-service-name
               :email gfinance-user-email
               :password gfinance-user-password))

(defvar gfinance-auth-handle (make-gfinance-auth)
  "G auth handle used for signing into calendar.")

(defun gfinance-authenticate ()
  "Authenticate into Google Calendar."
  (declare (special gfinance-auth-handle))
  (g-authenticate gfinance-auth-handle))

;;}}}
;;{{{ Feed of feeds:

(defvar gfinance-feeds-template-url
  "'http://finance.google.com/finance/feeds/%s/portfolios'"
  "URL template for feed of portfolios from Finance.")

(defsubst gfinance-feeds-url (userid)
  "Return url for feed of feeds."
  (declare (special gfinance-feeds-template-url))
  (format gfinance-feeds-template-url userid))
;;;###autoload
(defun gfinance-portfolios ()
  "Retrieve and display feed of feeds after authenticating."
  (interactive)
  (declare (special gfinance-auth-handle
                    g-atom-view-xsl
                    g-curl-program g-curl-common-options
                    g-cookie-options))
  (g-auth-ensure-token gfinance-auth-handle)
  (g-display-result
   (format
    "%s %s %s %s '%s' 2>/dev/null"
    g-curl-program g-curl-common-options
    g-cookie-options
    (g-authorization gfinance-auth-handle)
    (gfinance-feeds-url
     (g-url-encode (g-auth-email gfinance-auth-handle))))
   g-atom-view-xsl))
;;;###autoload
(defun gfinance-display-feed (feed-url)
  "Retrieve and display feedat feed-url  after authenticating."
  (interactive "sURL:")
  (declare (special gfinance-auth-handle
                    g-atom-view-xsl
                    g-curl-program g-curl-common-options
                    g-cookie-options))
  (g-auth-ensure-token gfinance-auth-handle)
  (g-display-result
   (format
    "%s %s %s %s '%s' 2>/dev/null"
    g-curl-program g-curl-common-options
    g-cookie-options
    (g-authorization gfinance-auth-handle)
    feed-url)
   g-atom-view-xsl))

;;}}}
;;{{{ Sign out:
;;;###autoload
(defun gfinance-sign-out()
  "Resets client so you can start with a different userid."
  (interactive)
  (declare (special gfinance-auth-handle
                    gfinance-user-email gfinance-user-password))
  (message "Signing out %s from Calendar"
           (g-auth-email gfinance-auth-handle))
  (setq gfinance-user-email nil
        gfinance-user-password nil)
  (setq gfinance-auth-handle (make-gfinance-auth)))

;;;###autoload
(defun gfinance-sign-in()
  "Resets client so you can start with a different userid."
  (interactive)
  (declare (special gfinance-auth-handle gfinance-user-email ))
  (setq gfinance-user-email
        (read-from-minibuffer "User Email:"))
  (setq gfinance-auth-handle (make-gfinance-auth))
  (g-authenticate gfinance-auth-handle))

;;}}}
(provide 'gfinance)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
