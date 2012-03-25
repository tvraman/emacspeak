;;; g-books.el --- Books Google  Client
;;;$Id: gbooks.el 6262 2009-09-25 21:54:09Z tv.raman.tv $
;;; $Author: raman $
;;; Description:  Books that all clients start from.
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
;;; API is here: http://code.google.com/apis/books/docs/v1/getting_started.html
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

(defgroup gbooks nil
  "Google books"
  :group 'g)

(defcustom gbooks-user-email nil
  "Mail address that identifies books user."
  :type '(choice
          (const :tag "none" nil)
          (string :tag "username@gmail.com" ""))
  :group 'gbooks)

(defcustom gbooks-user-password nil
  "Password for authenticating to books account."
  :type '(radio (const :tag "Prompt for password" nil)
                (string :tag "Save password in .emacs"))
  :group 'gbooks)

;;}}}
;;{{{ Constants

(defconst gbooks-service-name "skel"
  "Service name for accessing Google books.")

(defsubst gbooks-p (service)
  "Check if this is Books."
  (declare (special gbooks-service-name))
  (string-equal service gbooks-service-name))

;;}}}
;;{{{ books Authenticate

(defsubst make-gbooks-auth ()
  "Make a new gbooks auth handle."
  (declare (special gbooks-service-name
                    gbooks-user-email gbooks-user-password))
  (make-g-auth :service gbooks-service-name
               :email gbooks-user-email
               :password gbooks-user-password))

(defvar gbooks-auth-handle (make-gbooks-auth)
  "G auth handle used for signing into books.")

(defun gbooks-authenticate ()
  "Authenticate into Google Books."
  (declare (special gbooks-auth-handle))
  (g-authenticate gbooks-auth-handle))

;;}}}
;;{{{ Feed of feeds:

(defvar gbooks-feeds-template-url
  "'https://www.google.com/books/feeds/%s'"
  "URL template for feed of feeds from books.")
(defsubst gbooks-feeds-url (userid)
  "Return url for feed of feeds."
  (declare (special gbooks-feeds-template-url))
  (format gbooks-feeds-template-url userid))

(defun gbooks-skels ()
  "Retrieve and display feed of feeds after authenticating."
  (interactive)
  (declare (special gbooks-auth-handle
                    g-atom-view-xsl
                    g-curl-program g-curl-common-options
                    g-cookie-options))
  (g-auth-ensure-token gbooks-auth-handle)
  (g-display-result
   (format
    "%s %s %s %s '%s' 2>/dev/null"
    g-curl-program g-curl-common-options
    g-cookie-options
    (g-authorization gbooks-auth-handle)
    (gbooks-feeds-url
     (g-url-encode (g-auth-email gbooks-auth-handle))))
   g-atom-view-xsl))

;;}}}
;;{{{ Sign out:
;;;###autoload
(defun gbooks-sign-out()
  "Resets client so you can start with a different userid."
  (interactive)
  (declare (special gbooks-auth-handle
                    gbooks-user-email gbooks-user-password))
  (message "Signing out %s from Books"
           (g-auth-email gbooks-auth-handle))
  (setq gbooks-user-email nil
        gbooks-user-password nil)
  (setq gbooks-auth-handle (make-gbooks-auth)))

;;;###autoload
(defun gbooks-sign-in()
  "Resets client so you can start with a different userid."
  (interactive)
  (declare (special gbooks-auth-handle gbooks-user-email ))
  (setq gbooks-user-email
        (read-from-minibuffer "User Email:"))
  (setq gbooks-auth-handle (make-gbooks-auth))
  (g-authenticate gbooks-auth-handle))

;;}}}
(provide 'gbooks)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
