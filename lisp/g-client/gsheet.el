;;; gsheet.el --- Google Spreadsheets
;;;$Id: gsheet.el,v 1.30 2006/09/28 17:47:44 raman Exp $
;;; $Author: raman $
;;; Description:  Google Spreadsheet
;;; Keywords: Spreadsheets, GData, Google   Atom API
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; gsheet| T. V. Raman |raman@cs.cornell.edu
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

;;; Google Spreadsheets
;;; Create, Browse, Find, edit  ...  integrate Emacs  with
;;; Google Spreadsheet.
;;; http://code.google.com/apis/spreadsheets/overview.html

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'g-utils)
(require 'g-auth)
(require 'browse-url)

;;}}}
;;{{{ Customizations

(defgroup gsheet nil
  "Google Spreadsheets"
  :group 'g)

(defcustom gsheet-user-email nil
  "Mail address that identifies spreadsheets user."
  :type '(choice
          (const :tag "none" nil)
          (string :tag "username@gmail.com" ""))
  :group 'gsheet)

(defcustom gsheet-user-password nil
  "Password for authenticating to Spreadsheets account."
  :type '(radio (const :tag "Prompt for password" nil)
                (string :tag "Save password in .emacs"))
  :group 'gsheet)

;;}}}
;;{{{ Constants

(defconst gsheet-service-name "wise"
  "Service name for accessing Google spreadsheets.")

(defsubst gsheet-p (service)
  "Check if this is Spreadsheet."
  (declare (special gsheet-service-name))
  (string-equal service gsheet-service-name))

;;}}}
;;{{{ GSheet Authenticate

(defsubst make-gsheet-auth ()
  "Make a new gsheet auth handle."
  (declare (special gsheet-service-name
                    gsheet-user-email gsheet-user-password))
  (make-g-auth :service gsheet-service-name
               :email gsheet-user-email
               :password gsheet-user-password))

(defvar gsheet-auth-handle
  (make-gsheet-auth)
  "G auth handle used for signing into Spreadsheets.")

(defun gsheet-authenticate ()
  "Authenticate into Google Spreadsheets."
  (declare (special gsheet-auth-handle))
  (g-authenticate gsheet-auth-handle))



;;}}}
;;{{{ gsheet-fetch

(defun gsheet-fetch (sheet-url)
  "Fetch specified sheet."
  (interactive "sSheet URL:")  (declare (special gsheet-auth-handle
                                                  g-atom-view-xsl
                                                  g-curl-program g-curl-common-options
                                                  g-cookie-options))
  (g-auth-ensure-token gsheet-auth-handle)
  (g-display-result
   (format
    "%s %s %s %s '%s' %s"
    g-curl-program g-curl-common-options
    g-cookie-options
    (g-authorization gsheet-auth-handle)
    sheet-url
    (g-curl-debug))
   g-atom-view-xsl))

;;}}}
;;{{{ Feed of feeds:

(defvar gsheet-feeds-template-url
  "http://spreadsheets.google.com/feeds/spreadsheets/private/full"
  "URL template for feed of feeds from spreadsheet.")

(defsubst gsheet-feeds-url (userid)
  "Return url for feed of feeds."
  (declare (special gsheet-feeds-template-url))
   gsheet-feeds-template-url )

(defun gsheet-sheets ()
  "Retrieve and display feed of feeds after authenticating."
  (interactive)
  (declare (special gsheet-auth-handle g-atom-view-xsl
                    g-curl-program g-curl-common-options
                    g-cookie-options))
  (g-auth-ensure-token gsheet-auth-handle)
  (g-display-result
   (format
    "%s %s %s %s '%s' %s"
    g-curl-program g-curl-common-options
    g-cookie-options
    (g-authorization gsheet-auth-handle)
    (gsheet-feeds-url (g-url-encode (g-auth-email gsheet-auth-handle)))
    (g-curl-debug))
   g-atom-view-xsl))

;;}}}
;;{{{ sign out:

(defun gsheet-sign-out()
  "Resets client so you can start with a different userid."
  (interactive)
  (declare (special gsheet-auth-handle
                    gsheet-user-email gsheet-user-password))
  (message "Signing out %s from spreadsheets"
           (g-auth-email gsheet-auth-handle))
  (setq gsheet-user-email nil
        gsheet-user-password nil)
  (setq gsheet-auth-handle (make-gsheet-auth)))

;;}}}

(provide 'gsheet)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
