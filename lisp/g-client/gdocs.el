;;; g-docs.el --- Docs Google  Client
;;;$Id: gdocs.el 5798 2008-08-22 17:35:01Z tv.raman.tv $
;;; $Author: raman $
;;; Description:  Implement Google Docs in Emacs
;;; Keywords: Google Docs,Google   Atom API
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
;;; See http://code.google.com/apis/documents/overview.html
;;; <insert description here>
;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'g-utils)
(require 'g-auth)
(require 'browse-url)

;;}}}
;;{{{ Customizations

(defgroup gdocs nil
  "Google docs"
  :group 'g)

(defcustom gdocs-user-email nil
  "Mail address that identifies Docs user."
  :type '(choice
          (const :tag "none" nil)
          (string :tag "username@gmail.com"))
  :group 'gdocs)

(defcustom gdocs-user-password nil
  "Password for authenticating to Docs account."
  :type '(radio (const :tag "Prompt for password" nil)
                (string :tag "Save password in .emacs"))
  :group 'gdocs)

;;}}}
;;{{{ Constants

(defconst gdocs-service-name "writely"
  "Service name for accessing Google docs.")

(defsubst gdocs-p (service)
  "Check if this is Docs."
  (declare (special gdocs-service-name))
  (string-equal service gdocs-service-name))

;;}}}
;;{{{ docs Authenticate

(defsubst make-gdocs-auth ()
  "Make a new gdocs auth handle."
  (declare (special gdocs-service-name
                    gdocs-user-email gdocs-user-password))
  (make-g-auth :service gdocs-service-name
               :email gdocs-user-email
               :password gdocs-user-password))

(defvar gdocs-auth-handle (make-gdocs-auth)
  "G auth handle used for signing into Docs.")

(defun gdocs-authenticate ()
  "Authenticate into Google Docs."
  (declare (special gdocs-auth-handle))
  (g-authenticate gdocs-auth-handle))

;;}}}
;;{{{ Feed of feeds:

(defvar gdocs-feeds-template-url
  "http://docs.google.com/feeds/documents/private/full"
  "URLtemplate for DocList feed.")

(defsubst gdocs-feeds-url (userid)
  "Return url for feed of feeds."
  (declare (special gdocs-feeds-template-url))
  gdocs-feeds-template-url)

(defun gdocs-doclist ()
  "Retrieve and display feed of feeds after authenticating."
  (interactive)
  (declare (special gdocs-auth-handle
                    g-atom-view-xsl
                    g-curl-program g-curl-common-options
                    g-cookie-options))
  (g-auth-ensure-token gdocs-auth-handle)
  (g-display-result
   (format
    "%s %s %s %s '%s' 2>/dev/null"
    g-curl-program g-curl-common-options
    g-cookie-options
    (g-authorization gdocs-auth-handle)
    (gdocs-feeds-url
     (g-url-encode (g-auth-email gdocs-auth-handle))))
   g-atom-view-xsl))

;;}}}
;;{{{ Sign out:
;;;###autoload
(defun gdocs-sign-out()
  "Resets client so you can start with a different userid."
  (interactive)
  (declare (special gdocs-auth-handle
                    gdocs-user-email gdocs-user-password))
  (message "Signing out %s from Docs"
           (g-auth-email gdocs-auth-handle))
  (setq gdocs-user-email nil
        gdocs-user-password nil)
  (setq gdocs-auth-handle (make-gdocs-auth)))

;;;###autoload
(defun gdocs-sign-in()
  "Resets client so you can start with a different userid."
  (interactive)
  (declare (special gdocs-auth-handle gdocs-user-email ))
  (setq gdocs-user-email
        (read-from-minibuffer "User Email:"))
  (setq gdocs-auth-handle (make-gdocs-auth))
  (g-authenticate gdocs-auth-handle))

;;}}}
(provide 'gdocs)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
