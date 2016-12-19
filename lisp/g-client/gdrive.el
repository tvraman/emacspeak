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
;;{{{  introduction

;;; Commentary:

;;; Implement GDrive V3 API Using OAuth2

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(require 'backquote)
(require 'g-utils)
(require 'oauth2 "oauth2" 'no-error)

(declaim  (optimize  (safety 0) (speed 3)))

;;}}}
;;{{{ Customizations:

(defgroup gdrive nil
  "Google Drive"
  :group 'g)

(defcustom gdrive-oauth2-json
  (expand-file-name "gdrive.json.gpg" user-emacs-directory)
  "Location where we store encrypted JSON containing Google API client-id/client-secret.
Download this from the Google API Console after creating  your client-id, then encrypt it with GPG.
e.g. gpg -c <filename.json>.
Emacs will prompt for the encryption password on first use."
  :type 'file)

;;}}}
;;{{{ g-oauth2:

(defstruct g-oauth-client
  auth-uri token-uri
  secret id
  localhost-uri scope)

(defun gdrive-get-oauth-from-json ()
  "Return a populated g-oauth structure containing client-id and client-secret."
  (declare (special  gdrive-oauth2-json))
  (with-temp-buffer
    (insert-file-contents gdrive-oauth2-json)
    (goto-char (point-min))
    (let-alist  (g-json-get 'installed (json-read))
      (make-g-oauth-client
       :localhost-uri "http://localhost:8080/gdrive-oauth2"
       :scope "https://www.googleapis.com/auth/drive"
       :auth-uri .auth_uri
       :token-uri .token_uri
       :secret .client_secret
       :id .client_id))))

(defconst gdrive-resource-api-base
  "https://www.googleapis.com/drive/v3"
  "GDrive API Base Resource URL.")


(defsubst gdrive-api-method-uri (method)
  "Return REST end-point for specified method."
  (concat gdrive-resource-api-base "/" method))

(defconst gdrive-home-url
  "https://www.google.com/drive"
  "Home URL for Google Drive")

(defun gdrive-oauth-auth (resource-url )
  "Request access to a Drive resource."
  (let ((g (gdrive-get-oauth-from-json)))
    (oauth2-auth
     (g-oauth-client-auth-uri g) (g-oauth-client-token-uri g)
     (g-oauth-client-id g) (g-oauth-client-secret g)
     (g-oauth-client-scope g) ; scope?
     nil  ;state
     (g-oauth-client-localhost-uri g))))

(defun gdrive-oauth-auth-and-store (resource-url  )
  "Request access to a Google Drive resource and store it using `auth-source'."
  (let ((g (gdrive-get-oauth-from-json)))
    (oauth2-auth-and-store
     (g-oauth-client-auth-uri g) (g-oauth-client-token-uri g)
     resource-url
     (g-oauth-client-id g) (g-oauth-client-secret g)
     (g-oauth-client-localhost-uri g))))

(defun gdrive-url-retrieve (url)
  "Retrieve GDrive URL using OAuth2."
  (let ((buf
         (oauth2-url-retrieve-synchronously
          (gdrive-oauth-auth-and-store url)
          url)))
    buf))

;;}}}
(provide 'gdrive)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
