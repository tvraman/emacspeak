;;; g-tube.el --- YouTube Google  Client
;;;$Id:$
;;; $Author: raman $
;;; Description:  YouTube API Client
;;; Keywords: Google    API
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

;;; http://www.youtube.com/dev_intro
;;; http://www.youtube.com/dev_docs

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'g-utils)
(require 'g-auth)
(require 'browse-url)

;;}}}
;;{{{ Customizations

(defgroup gtube nil
  "Google Youtube"
  :group 'g)
(defcustom gtube-user-name nil
  "YouTube user name."
  :type '(choice
          (const :tag "none" nil)
          (string   ""))
  :group 'gtube)

(defcustom gtube-user-email nil
  "YouTube user email address."
  :type '(choice
          (const :tag "none" nil)
          (string :tag "username@gmail.com" ""))
  :group 'gtube)

(defcustom gtube-user-password nil
  "Password for authenticating to YouTube account."
  :type '(radio (const :tag "Prompt for password" nil)
                (string :tag "Save password in .emacs"))
  :group 'gtube)

(defcustom gtube-developer-id nil
  "YouTube API  Developer Id"
  :type '(radio (const :tag "Prompt for Id" nil)
                (string :tag "Save Id in .emacs"))
  :group 'gtube)

;;}}}
;;{{{ Constants

;;; not yet used:

(defconst gtube-service-name "skel"
  "Service name for accessing Google tube.")

(defsubst gtube-p (service)
  "Check if this is Calendar."
  (declare (special gtube-service-name))
  (string-equal service gtube-service-name))

(defconst gtube-rest-end-point
  "http://www.youtube.com/api2_rest"
  "REST end-point for YouTube APIs.")

;;}}}
;;{{{ Construct REST end-point:

(defsubst gtube-rest-resource (method-name &optional arguments)
  "Return a GTube REST URI.
Calls method `method-name' with specified arguments.
Arguments is a list of name/value pairs."
  (declare (special gtube-rest-end-point
                    gtube-developer-id))
  (let ((base (format "%s?method=%s&dev_id=%s"
                 gtube-rest-end-point
                 method-name gtube-developer-id )))
    (if arguments
        (concat base
                "&"
         (mapconcat
          #'(lambda (pair)
              (format "%s=%s"
                      (car pair)
                      (g-url-encode (cadr pair))))
          arguments
          "&"))
      base)))

;;; helper: get XML
(defvar gtube-xml-buffer "*GTube Results*"
  "Buffer used to hold XML responses.")

(defsubst gtube-get-xml (resource)
  "Retrieve XMl and place it in an appropriate buffer."
  (declare (special gtube-xml-buffer
                    g-curl-program))
  (let ((buffer (get-buffer-create gtube-xml-buffer))
        (nxml-auto-insert-xml-declaration-flag nil))
  (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (setq buffer-undo-list t)
      (insert
          (g-get-result
       (format "%s --silent '%s' %s"
               g-curl-program resource
               (g-curl-debug))))
      (xml-mode)
      (indent-region (point-min) (point-max)))
  buffer))

;;}}}
;;{{{ get info:
;;;###autoload
(defun gtube-user-profile (&optional user)
  "Retrieve user profile."
  (interactive
   (list
    (read-from-minibuffer "username: "
                          gtube-user-name)))
  (declare (special gtube-user-name))
  (switch-to-buffer
        (gtube-get-xml
         (gtube-rest-resource "youtube.users.get_profile"
                              `(("user" ,user))))))

;;;###autoload
(defun gtube-video-details (video-id)
  "Display details of specified video."
  (interactive "sVideo:")
  (switch-to-buffer
        (gtube-get-xml
         (gtube-rest-resource  "youtube.videos.get_details"
                               `(("video_id" ,video-id))))))

(defun gtube-video-list-by-tag (tag &optional page count)
  "Retrieve content having specified tag.
optional args page and count specify position in result-set and
  number of results to retrieve."
  (interactive "sTag:\nsPage:\nsCount:")
  (or page (setq page "1"))
  (or count (setq count "10"))
  (switch-to-buffer
        (gtube-get-xml
         (gtube-rest-resource  "youtube.videos.list_by_tag"
                               `(("tag" ,tag)
                                 ("page" ,page)
                                 ("per_page" ,count))))))

(defun gtube-video-list-by-user (user &optional page count)
  "Retrieve content from specified user.
optional args page and count specify position in result-set and
  number of results to retrieve."
  (interactive "sUser:\nsPage:\nsCount:")
  (or page (setq page "1"))
  (or count (setq count "10"))
  (switch-to-buffer
   (gtube-get-xml
    (gtube-rest-resource  "youtube.videos.list_by_user"
                          `(("user" ,user)
                            ("page" ,page)
                            ("per_page" ,count))))))

(defun gtube-video-list-featured ( )
  "Retrieved featured video list."
  (interactive)
  (switch-to-buffer
        (gtube-get-xml
         (gtube-rest-resource  "youtube.videos.list_featured"))))

;;}}}
;;{{{ tube Authenticate

;;; not yet used:

(defsubst make-gtube-auth ()
  "Make a new gtube auth handle."
  (declare (special gtube-service-name
                    gtube-user-email gtube-user-password))
  (make-g-auth :service gtube-service-name
               :email gtube-user-email
               :password gtube-user-password))

(defvar gtube-auth-handle (make-gtube-auth)
  "G auth handle used for signing into calendar.")

(defun gtube-authenticate ()
  "Authenticate into Google Calendar."
  (declare (special gtube-auth-handle))
  (g-authenticate gtube-auth-handle))

;;}}}


(provide 'gtube)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
