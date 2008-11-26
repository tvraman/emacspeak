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
;;;Basic Design:
;;; Use gdocs-doclist to get a list of documents,
;;; Follow the download link to read the document as HTML,
;;; And use the edit-media URL to edit the content.
;;; Editting will be done using org-mode,
;;; And we will use org-export to turn org-authored content into HTML before posting.
;;; Function org-infile-export-plist will be used to get metadata from the org-mode source buffer
;;; ToDo: To figure out how to round-trip back from Docs-generated HTMLinto org.

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'g-utils)
(require 'g-auth)
(require 'g-app)
(require 'browse-url)
(condition-case nil
    (require 'org-exp)
  (error "You need a recent version of org."))

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
  "URL template for DocList feed.")

(defsubst gdocs-feeds-url ()
  "Return url for feed of feeds."
  (declare (special gdocs-feeds-template-url))
  gdocs-feeds-template-url)
;;;###autoload
(defun gdocs-doclist (&optional query)
  "Retrieve and display feed of feeds after authenticating.
Interactive prefix arg prompts for a query string."
  (interactive "P")
  (declare (special gdocs-auth-handle
                    g-atom-view-xsl
                    g-curl-program g-curl-common-options
                    g-cookie-options))
  (let ((location (if query
                      (concat
                       (gdocs-feeds-url)
                       (format "?q=%s"
                               (g-url-encode (read-from-minibuffer "Documents Matching: "))))
                    (gdocs-feeds-url))))
    (g-auth-ensure-token gdocs-auth-handle)
    (g-display-result
     (format
      "%s %s %s %s '%s' 2>/dev/null"
      g-curl-program g-curl-common-options
      g-cookie-options
      (g-authorization gdocs-auth-handle) location)
     g-atom-view-xsl)))

;;}}}
;;{{{ Publishing via org:

(defvar gdocs-upload-options
  "--data-binary @- -H 'Content-Type: text/html'"
  "Options template for uploading a document without metadata.")

;;;###autooad
(defun gdocs-publish-from-org ()
  "Export from Org  to Google Docs as HTML."
  (interactive)
  (declare (special  gdocs-auth-handle g-curl-program
                     gdocs-upload-options g-atom-view-xsl))
  (unless (eq major-mode 'org-mode)
    (error "Not in an org-mode buffer."))
  (g-auth-ensure-token gdocs-auth-handle)
  (let ((org-buffer (current-buffer)))
    (g-using-scratch
     (save-excursion
       (set-buffer org-buffer)
       (org-export-region-as-html (point-min) (point-max)
                                  nil g-scratch-buffer))
     (set-buffer-multibyte nil)
     (let ((cl (format "-H 'Content-Length: %s'" (g-buffer-bytes))))
       (shell-command-on-region
        (point-min) (point-max)
        (format
         "%s -s -S -i %s %s %s %s"
         g-curl-program 
         gdocs-upload-options cl 
         (g-authorization gdocs-auth-handle)
         (gdocs-feeds-url))
        nil 'replace
        "*Messages*"))
     (let ((headers (g-http-headers (point-min) (point-max)))
           (body (g-http-body (point-min) (point-max))))
       (cond
        ((string-equal "201" (g-http-header "Status" headers))
         (g-display-xml-string body g-atom-view-xsl))
        (t (error "Received %s"
                  (g-http-header "Status" headers))))))))
    

;;}}}
;;{{{ ACL:
(defvar  gdocs-acl-colaborator-template
  "<entry xmlns='http://www.w3.org/2005/Atom' 
       xmlns:gAcl='http://schemas.google.com/acl/2007'>
<category scheme='http://schemas.google.com/g/2005#kind'     
    term='http://schemas.google.com/acl/2007#accessRule'/>
<gAcl:role value='writer'/>
<gAcl:scope type='user' value='%s'/>
</entry> "
  "Atom Entry template for adding a collaborator.")
;;;###autoload
(defun gdocs-add-collaborator (email acl-url)
  "Add collaborator to ACL at acl-url.
You can find the acl-url through the DocList."
  (interactive "sEMail\nsACL-URL:")
  (declare (special gdocs-auth-handle
                    g-atom-view-xsl gdocs-acl-colaborator-template))
  (g-using-scratch 
   (set-buffer-multibyte nil)
   (let ((g-app-this-url acl-url)
         (g-app-auth-handle gdocs-auth-handle)
         (response nil))
     (insert 
      (format gdocs-acl-colaborator-template
              email))
     (setq major-mode 'g-app-mode)
     (setq response (g-app-post-entry))
     (cond
      ((string-equal "201" (g-http-header "Status" (car response)))
       (g-display-xml-string (second response)
                             g-atom-view-xsl))
      (t (error "Status %s"
                (g-http-header "Status" (car response))))))))

;;}}}
;;{{{ deleting an item:

;;;###autoload
(defun gdocs-delete-item (url)
  "Delete specified item."
  (interactive
   (list
    (read-from-minibuffer "Entry URL:")))
  (declare (special gdocs-auth-handle))
  (g-app-delete-entry gdocs-auth-handle url))

;;}}}
;;{{{ Viewing an item:
 
;;;###autoload
(defun gdocs-view-item (url)
  "View specified item."
  (interactive
   (list
    (read-from-minibuffer "Entry URL:")))
  (declare (special gdocs-auth-handle))
  (g-app-view gdocs-auth-handle url))

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
