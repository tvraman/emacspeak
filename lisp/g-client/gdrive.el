;;; gdrive.el --- Google Client  For GDrive
;;; $Author: raman $
;;; Description:  Google Client For GDrive
;;; Keywords: Google   Developer API, GDrive
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; g-client| T. V. Raman |raman@cs.cornell.edu
;;; An emacs interface to Google Services|
;;; $Date: 2006/10/13 01:38:19 $ |
;;;  $Revision: 1.14 $ |
;;; Location undetermined
;;; License: GPL
;;;

;;}}}
;;{{{ Copyright:

;;; Copyright (c) 2006 and later, Google Inc.
;;; All rights reserved.

;;; Redistribution and use in source and binary forms,
;;; with or without modification,
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
;;{{{  introduction

;;; Commentary:
;;; Implement Google OAuth flow:
;;; Create your  app credentials via the Google Developer Console,
;;; https://console.developers.google.com/
;;; Then enable the APIs you plan to access via this set of credentials.
;;; Download the credentials as JSON, and GPG encrypt the file.
;;; Defaults settings in this module assume above file to be
;;; $HOME/.emacs.d/emacs-google.json.gpg

;;; This module uses simple-httpd.el to run an Emacs HTTPD daemon.
;;; A simple HTTPD servlet in that daemon listens for the OAuth2 redirect URL
;;; to receive the OAuth auth code --- saves cut/paste from Chrome.
;;; You still need to click "Allow" from Chrome.
;;; The Elisp HTTPD servlet receives the code via  the servlet
;;; Caches it, and places it in the kill-ring.
;;; Module oauth2.el as written expects the user to type the code in,
;;; with this implementation, hit C-y at that point.

;;; Implement GDrive V3 API Using OAuth2

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(require 'browse-url)
(when (locate-library "package")
  (unless (locate-library "oauth2") (package-install 'oauth2)))
(require 'oauth2 "oauth2" 'no-error)
(when (locate-library "package")
  (unless (locate-library "simple-httpd") (package-install 'simple-httpd)))
(require 'simple-httpd nil 'no-error)
(require 'g-utils)
(cl-declaim  (optimize  (safety 0) (speed 3)))

;;}}}
;;{{{ Customizations:

(defgroup g-oauth nil
  "Google Drive"
  :group 'g)

(defcustom g-oath-creds-store
  (expand-file-name "emacs-google.json.gpg" user-emacs-directory)
  "Location of  encrypted JSON containing Google API client-id/client-secret.
Download this from the Google API Console after creating your
client-id for your project , then encrypt it with GPG. e.g. gpg -c
<filename.json>. Emacs will prompt for the encryption password on
first use. These credentials can be used with Google APIs after those APIs have been enabled for this project from the Google API Console."
  :type 'file)

;;}}}
;;{{{ g-oauth:

(cl-defstruct g-oauth
  auth-uri token-uri
  secret id
  code ; received after auth
  localhost-uri scope)

(defvar gdrive--oauth nil
  "Handle to oauth credentials for use with GDrive.")

(defun gdrive-get-oauth-from-json (&optional refresh)
  "Return a  g-oauth structure containing client-id and client-secret."
  (declare (special  g-oauth-creds-store gdrive--oauth))
  ;;; Start up HTTPD daemon if needed here (for lack of a better place)
  (unless (process-status "httpd") (httpd-start))
  (when (or refresh (null gdrive--oauth))
    (with-temp-buffer
      (insert-file-contents g-oauth-creds-store)
      (goto-char (point-min))
      (let-alist  (g-json-get 'installed (json-read))
        (setq
         gdrive--oauth
         (make-g-oauth
          :localhost-uri "http://localhost:8080/gdrive-oauth2"
          :scope "https://www.googleapis.com/auth/drive"
          :auth-uri .auth_uri
          :token-uri .token_uri
          :secret .client_secret
          :id .client_id)))))
  gdrive--oauth)

(defconst gdrive-resource-api-base
  "https://www.googleapis.com/drive/v3"
  "GDrive API Base Resource URL.")

(defun gdrive-api-method-uri (method)
  "Return REST end-point for specified method."
  (concat gdrive-resource-api-base "/" method))

(defconst gdrive-home-url
  "https://www.google.com/drive"
  "Home URL for Google Drive")

(defun gdrive-oauth-auth ()
  "Request GDrive auth for required scope."
  (let ((g (gdrive-get-oauth-from-json))
        (browse-url-browser-function  #'browse-url-chrome))
    (oauth2-auth
     (g-oauth-auth-uri g) (g-oauth-token-uri g)
     (g-oauth-id g) (g-oauth-secret g)
     (g-oauth-scope g)
     nil  ;state
     (g-oauth-localhost-uri g))))

(defun gdrive-oauth-auth-and-store ()
  "Request access to  Google Drive  and store it using `auth-source'."
  (let ((g (gdrive-get-oauth-from-json))
        (browse-url-browser-function  #'browse-url-chrome))
    (oauth2-auth-and-store
     (g-oauth-auth-uri g) (g-oauth-token-uri g)
     (g-oauth-scope g)
     (g-oauth-id g) (g-oauth-secret g)
     (g-oauth-localhost-uri g))))

(defun gdrive-url-retrieve (url)
  "Retrieve GDrive URL using OAuth2."
  (let ((buf
         (oauth2-url-retrieve-synchronously
          (gdrive-oauth-auth-and-store)
          url)))
    buf))

;;}}}
;;{{{ httpd for local redirect:
(defun httpd/gdrive-oauth2  (proc path params request)
  "Servlet to receive and propagate token."
  (declare (special gdrive--oauth))
  (kill-new
   (setf (g-oauth-code  gdrive--oauth)
         (cadr (assoc "code" params))))
  (with-httpd-buffer proc "text/plain"
                     (insert
                      (format "%s: %s"
                              (if (g-oauth-code gdrive--oauth)
                                  "Success" "Failure")
                              (file-name-nondirectory path)))))

;;}}}
(provide 'gdrive)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
