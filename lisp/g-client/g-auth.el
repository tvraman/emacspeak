;;; g-auth.el --- Google Authentication Module
;;;$Id: g-auth.el,v 1.6 2006/10/23 16:51:50 raman Exp $
;;; $Author: raman $
;;; Description:  Google Authentication Module
;;; Keywords: Google   Auth
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; g-client| T. V. Raman |raman@cs.cornell.edu
;;; An emacs interface to Google services|
;;; $Date: 2006/10/23 16:51:50 $ |
;;;  $Revision: 1.6 $ |
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

;;; Google Auth using Curl
;;; Implements an authentication component for use in Google
;;; Clients -- see http://code.google.com/apis/accounts/AuthForInstalledApps.html

;;; Steps:
;;; 0) Connect via https to
;;; https://www.google.com/accounts/ClientLogin?service=%s
;;; where %s is the service name e.g. reader, blogger etc.
;;; and pass in the email/passwd as POST data.
;;; 1) Obtain the response, and cache the auth token
;;; 2) Cache this token for use during its lifetime.

;;; defstruct g-auth encapsulates this functionality.
;;; See module greader.el for sample usage

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'g-utils)

;;}}}
;;{{{ Customizations:

(defgroup g-auth nil
  "Google Authentication"
  :group 'g)
(defcustom g-user-email nil
  "Google account  address."
  :type '(choice
          (const :tag "none" nil)
          (string :tag "username@gmail.com" ""))
  :group 'g)

(defcustom g-auth-lifetime '(0 1800 0)
  "Lifetime of authentication token as a list suitable for
`current-time'."
  :type 'sexp
  :group 'g-auth)

;;}}}
;;{{{ Generators :

(defvar g-auth-scratch-buffer" *g auth*"
  "Scratch buffer we do authentication work.")

(defvar g-auth-url-pattern
  "https://www.google.com/accounts/ClientLogin?service=%s"
  "URL to login to Google services.")

(defsubst g-auth-url (service)
  "Return auth URL  for specified service."
  (declare (special g-auth-url-pattern))
  (format g-auth-url-pattern
          (g-url-encode service)))

;;}}}
;;{{{ G Auth Data Structures:

;;; email and password are supplied by user.
;;; Auth, sid and lsid are obtained from the server.

(defstruct g-auth
  (username  (user-login-name))
  email
  password
  token
  session-id ;gsession-id
  cookie-alist
  service
  (lifetime  g-auth-lifetime)
  timestamp
  post-auth-action)

(defsubst g-cookie (name auth-handle)
  "Return cookie for `name' from auth-handle if present."
  (let ((pair (assoc name
                     (g-auth-cookie-alist auth-handle))))
    (when pair (cdr pair))))

(defconst g-authorization-header-format
  "--header 'Authorization: GoogleLogin auth=%s'  \
--header 'Content-Type: application/atom+xml' "
  "HTTP headers to send.")

(defsubst g-authorization (auth-handle)
  "Return authorization header."
  (declare (special g-authorization-header-format))
  (format g-authorization-header-format
              (g-cookie "Auth" auth-handle)))

(defsubst g-auth-expired-p (auth-handle)
  "Check if  token for specified service has expired."
  (cond
   ((and (null (g-auth-token auth-handle))
         (null (g-auth-cookie-alist auth-handle)))t)
   ((time-less-p (g-auth-lifetime auth-handle)
                 (time-since (g-auth-timestamp auth-handle)))
    t)
   (t nil)))

;;}}}
;;{{{ G Authenticate

(defun g-authenticate (auth-handle)
  "Authenticate    using credentials in auth-handle.
Populate auth-handle with the returned cookies and token."
  (declare (special g-auth-scratch-buffer g-curl-program
                    g-user-email))
  (let* ((post-auth-action (g-auth-post-auth-action auth-handle))
        (email (or (g-auth-email auth-handle)
                   g-user-email
                   (read-from-minibuffer "User Address: ")))
        (password
         (or (g-auth-password auth-handle)
             (read-passwd
              (format "Password for %s:"
                      email))))
        (buff (get-buffer-create g-auth-scratch-buffer))
        (fields nil))
    (setf (g-auth-email auth-handle) email
          (g-auth-password auth-handle) password
          (g-auth-cookie-alist auth-handle) nil)
    (save-excursion
      (set-buffer buff)
      (erase-buffer)
      (insert
       (format "Email=%s&Passwd=%s&source=g-emacs&accountType=hosted_or_google"
               (g-url-encode email)
               (g-url-encode password)))
      (shell-command-on-region
       (point-min) (point-max)
       (format "%s %s -X POST --data-binary @- %s 2>/dev/null"
               g-curl-program g-cookie-options
               (g-auth-url (g-auth-service auth-handle)))
       (current-buffer)
       'replace)
      (goto-char (point-min))
      (while (not (eobp))
        (skip-syntax-forward " ")
        (setq fields (split-string
                      (buffer-substring (line-beginning-position)
                                        (line-end-position))
                      "="))
        (push (cons (first fields) (second fields))
              (g-auth-cookie-alist auth-handle))
        (forward-line 1))
      (when (and post-auth-action
                 (fboundp post-auth-action))
        (funcall post-auth-action auth-handle))
      (setf (g-auth-timestamp auth-handle) (current-time))
      auth-handle)))

(defun g-auth-ensure-token (auth-handle)
  "Ensure token is  still valid, re-authenticating if necessary."
  (when (g-auth-expired-p auth-handle)
    (g-authenticate auth-handle))
  auth-handle)

;;}}}
(provide 'g-auth)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
