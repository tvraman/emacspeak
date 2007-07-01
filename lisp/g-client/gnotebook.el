;;; gnotebook.el --- Google Notebook
;;;$Id: gnotebook.el,v 1.30 2006/09/28 17:47:44 raman Exp $
;;; $Author: raman $
;;; Description:  Google Notebook
;;; Keywords: Google   Services
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; gnotebook| T. V. Raman |raman@cs.cornell.edu
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

;;; Google Notebook
;;; Create, Browse, Find notes.
;;; http://code.google.com/apis/notebook/overview.html

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

(defgroup gnotebook nil
  "Google Calendar"
  :group 'g)

(defcustom gnotebook-user-email nil
  "Mail address that identifies calendar user."
  :type '(choice
          (const :tag "none" nil)
          (string :tag "username@gmail.com" ""))
  :group 'gnotebook)

(defcustom gnotebook-user-password nil
  "Password for authenticating to calendar account."
  :type '(radio (const :tag "Prompt for password" nil)
                (string :tag "Save password in .emacs"))
  :group 'gnotebook)

;;}}}
;;{{{ Constants

(defconst gnotebook-service-name "notebook"
  "Service name for accessing Google Notebook.")

(defsubst gnotebook-p (service)
  "Check if this is notebook."
  (declare (special gnotebook-service-name))
  (string-equal service gnotebook-service-name))
(defconst gnotebook-base-url
  "http://www.google.com/notebook/"
  "URL to Google Notebook.")

(defconst gnotebook-token-url
  (concat gnotebook-base-url
          "token")
  "URL for retrieving Google Reader token.")

;;}}}
;;{{{ Notebook Authenticate

(defsubst make-gnotebook-auth ()
  "Make a new gnotebook auth handle."
  (declare (special gnotebook-service-name
                    gnotebook-user-email gnotebook-user-password))
  (make-g-auth :service gnotebook-service-name
               :email gnotebook-user-email
               :password gnotebook-user-password
               :post-auth-action 'gnotebook-post-authenticate-function))

(defvar gnotebook-auth-handle
  (make-gnotebook-auth)
  "G auth handle used for signing into Notebook.")

(defun gnotebook-post-authenticate-function (auth-handle)
  "Run Googlre Notebook post-auth steps."
  (declare (special g-curl-program g-curl-common-options
                    gnotebook-token-url))
  (unless (gnotebook-p (g-auth-service auth-handle))
    (error "This auth handle is not for Google Notebook."))
  (setf (g-auth-token auth-handle)
        (substring
         (g-get-result
          (format
           "%s %s  --cookie SID='%s' %s 2>/dev/null"
           g-curl-program g-curl-common-options
           (g-cookie "SID" auth-handle) gnotebook-token-url))
         2 -2)))

(defun gnotebook-authenticate ()
  "Authenticate into Google Calendar."
  (declare (special gnotebook-auth-handle))
  (g-authenticate gnotebook-auth-handle))

;;}}}
;;{{{ list Notebooks

(defvar gnotebook-list-url-template
  (concat gnotebook-base-url
          "read?tok=%s&cmd=u")
  "URL template for listing Notebooks.")

(defsubst gnotebook-list-url ()
  "Return URL for retrieving list of notebooks."
  (declare (special gnotebook-auth-handle
                    gnotebook-list-url-template))
  (format  gnotebook-list-url-template
           (g-auth-token gnotebook-auth-handle)))

(defun gnotebook-list ()
  "List available notebooks."
  (interactive)
  (declare (special gnotebook-auth-handle))
  (g-auth-ensure-token gnotebook-auth-handle)
  (g-display-result
   (format
    "%s %s %s %s %s 2>/dev/null"
    g-curl-program g-curl-common-options
    g-cookie-options
    (g-authorization gnotebook-auth-handle)
    (gnotebook-list-url))
   nil))

;;}}}
(provide 'gnotebook)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
