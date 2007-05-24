;;; g-photo.el ---  Google  Picasa Client
;;;$Id: gphoto.el,v 1.30 2006/09/28 17:47:44 raman Exp $
;;; $Author: raman $
;;; Description:   Client  For Accessing Picasa (Photo Albums)
;;; Keywords: Google   Atom API
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; gphoto| T. V. Raman |raman@cs.cornell.edu
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

;;; Emacs client for accessing Picasa --- Google Photo Albums
;;; See  http://code.google.com/apis/picasaweb/overview.html

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'g-utils)
(require 'g-auth)
(require 'browse-url)
(require 'mml)

;;}}}
;;{{{ Customizations

(defgroup gphoto nil
  "Google photo"
  :group 'g)

(defcustom gphoto-user-email nil
  "Mail address that identifies Picasa user."
  :type '(choice
          (const :tag "none" nil)
          (string :tag "username@gmail.com" ""))
  :group 'gphoto)

(defcustom gphoto-user-password nil
  "Password for authenticating to Picasa account."
  :type '(radio (const :tag "Prompt for password" nil)
                (string :tag "Save password in .emacs"))
  :group 'gphoto)

(defcustom gphoto-album-default-access "public"
  "Default access mode for newly created albums."
  :type 'string
  :group 'gphoto)

(defcustom gphoto-album-default-commenting-enabled"true"
  "Default commenting mode for newly created albums."
  :type '(choice
          (string   "true")
          (string  "false"))
  :group 'gphoto)

;;}}}
;;{{{ Constants

(defconst gphoto-service-name "lh2"
  "Service name for accessing Google photo.")

(defsubst gphoto-p (service)
  "Check if this is Picasa."
  (declare (special gphoto-service-name))
  (string-equal service gphoto-service-name))

;;}}}
;;{{{ photo Authenticate

(defsubst make-gphoto-auth ()
  "Make a new gphoto auth handle."
  (declare (special gphoto-service-name
                    gphoto-user-email gphoto-user-password))
  (make-g-auth :service gphoto-service-name
               :email gphoto-user-email
               :password gphoto-user-password))

(defvar gphoto-auth-handle (make-gphoto-auth)
  "G auth handle used for signing into Picasa.")

(defun gphoto-authenticate ()
  "Authenticate into Google Photo."
  (declare (special gphoto-auth-handle))
  (g-authenticate gphoto-auth-handle))

;;}}}
;;{{{ Feed of feeds:

(defconst gphoto-base-url
  "http://picasaweb.google.com/data/feed/api/user"
  "Base URI for Picasa services.")

(defconst gphoto-album-or-tag
  '(("album" . "album")
    ("tag" . "tag"))
  "Choices for albums or tags.")

(defsubst gphoto-read-feed-kind ( prompt choices)
  "Prompt with prompt to collect choice from choices."
  (completing-read prompt choices))

(defvar gphoto-album-or-tag-template-url
  (format "%s/%%s?kind=%%s" gphoto-base-url)
  "URL template for feed of albums or tags from Picasa.")

(defsubst gphoto-album-or-tag-url (userid kind)
  "Return url for feed of albums or tags."
  (declare (special gphoto-album-or-tag-template-url))
  (format gphoto-album-or-tag-template-url userid kind))

(defun gphoto-album-or-tag (kind)
  "Retrieve and display feed of albums or tags after authenticating."
  (interactive
   (list
    (gphoto-read-feed-kind "Album or Tag: " gphoto-album-or-tag)))
  (declare (special gphoto-auth-handle
                    g-atom-view-xsl
                    g-curl-program g-curl-common-options
                    g-cookie-options))
  (g-auth-ensure-token gphoto-auth-handle)
  (g-display-result
   (format
    "%s %s %s %s '%s' %s"
    g-curl-program g-curl-common-options
    g-cookie-options
    (g-authorization gphoto-auth-handle)
    (gphoto-album-or-tag-url
     (g-url-encode (g-auth-email gphoto-auth-handle))
     kind)
    (g-curl-debug))
   g-atom-view-xsl))

;;}}}
;;{{{ Adding an album:

(defstruct gphoto-album
  title
  summary
  location
  (access gphoto-album-default-access)
  (commenting-enabled gphoto-album-default-commenting-enabled)
                                        ;timestamp
  keywords)

(defvar gphoto-album-template
  "<entry xmlns='http://www.w3.org/2005/Atom'
    xmlns:media='http://search.yahoo.com/mrss/'
    xmlns:gphoto='http://schemas.google.com/photos/2007'>
  <title type='text'>%s</title>
  <summary type='text'>%s</summary>
  <gphoto:location>%s</gphoto:location>
  <gphoto:access>%s</gphoto:access>
  <gphoto:commentingEnabled>%s</gphoto:commentingEnabled>
  <media:group>
    <media:keywords>%s</media:keywords>
  </media:group>
  <category scheme='http://schemas.google.com/g/2005#kind'
    term='http://schemas.google.com/photos/2007#album'></category>
</entry>"
  "Template for new album.")

(defun gphoto-read-album ()
  "Prompt user and return specified album structure."
  (let ((album (make-gphoto-album)))
    (loop for slot in
          '(title summary location keywords)
          do
          (eval
           `(setf (,(intern (format "gphoto-album-%s" slot))
                   album)
                  (read-from-minibuffer (format "%s: " slot)))))
    (setf (gphoto-album-access album)
          (read-from-minibuffer "access:"
                                gphoto-album-default-access
                                nil nil nil
                                gphoto-album-default-access))
    (setf (gphoto-album-commenting-enabled album)
          (read-from-minibuffer "Commenting Enabled?:"
                                gphoto-album-default-commenting-enabled
                                nil nil nil
                                gphoto-album-default-commenting-enabled))
    album))

(defun gphoto-album-as-xml (album)
  "Return Atom entry for  album structure."
  (declare (special gphoto-album-template))
  (format
   gphoto-album-template
   (gphoto-album-title album)
   (gphoto-album-summary album)
   (gphoto-album-location album)
   (gphoto-album-access album)
   (gphoto-album-commenting-enabled album)
   (gphoto-album-keywords album)))

(defsubst gphoto-album-create-url (auth-handle)
  "URL to which new albums are posted."
  (declare (special gphoto-base-url))
  (format "%s/%s"
          gphoto-base-url
          (g-url-encode
           (g-auth-email auth-handle))))

(defsubst gphoto-post-album (album location)
  "Post album to location and return HTTP response."
  (declare (special g-cookie-options gphoto-auth-handle
                    g-curl-program g-curl-common-options
                    g-curl-atom-header))
  (g-using-scratch
   (insert (gphoto-album-as-xml album))
   (let ((cl (format "-H Content-length:%s" (buffer-size)))
         (status nil))
     (shell-command-on-region
      (point-min) (point-max)
      (format
       "%s %s %s %s %s %s -i -X POST --data-binary @- %s 2>/dev/null"
       g-curl-program g-curl-common-options g-curl-atom-header cl
       (g-authorization gphoto-auth-handle)
       g-cookie-options
       location)
      (current-buffer) 'replace)
     (list (g-http-headers (point-min) (point-max))
           (g-http-body (point-min) (point-max))))))

;;;###autoload
(defun gphoto-album-create ()
  "Create a new GPhoto album."
  (interactive)
  (declare (special gphoto-auth-handle))
  (g-auth-ensure-token gphoto-auth-handle)
  (let ((album (gphoto-read-album))
        (headers nil)
        (body nil)
        (response nil))
    (setq response
          (gphoto-post-album album
                             (gphoto-album-create-url gphoto-auth-handle)))
    (setq headers (first response)
          body (second response))
    (when (or  (string-equal "201" (g-http-header "Status" headers))
               (string-equal "200" (g-http-header "Status" headers)))
      (and (> (length body)0)
           (g-display-xml-string body g-atom-view-xsl)))))

;;}}}
;;{{{ Adding a photo:

(defvar gphoto-photo-template
  "<entry xmlns='http://www.w3.org/2005/Atom'>
  <title>%s</title>
  <summary>%s</summary>
  <category scheme='http://schemas.google.com/g/2005#kind'
    term='http://schemas.google.com/photos/2007#photo'/>
</entry>")

(defstruct gphoto-photo
  title summary
  filepath)

(defun gphoto-read-photo ()
  "Prompt user and return specified photo structure."
  (let ((photo (make-gphoto-photo)))
    (setf (gphoto-photo-filepath photo)
          (read-file-name "File: "))
    (setf (gphoto-photo-title photo)
          (file-name-nondirectory (gphoto-photo-filepath photo)))
    (setf (gphoto-photo-summary photo)
          (read-from-minibuffer "Summary: "))
    photo))

(defun gphoto-photo-as-xml (photo)
  "Return Atom entry for  photo structure."
  (declare (special gphoto-photo-template))
  (format
   gphoto-photo-template
   (gphoto-photo-title photo)
   (gphoto-photo-summary photo)))

(defvar gphoto-photo-mime-template
  "<#multipart type=related>
     <#part type=image/jpeg filename=%s disposition=inline>
<#part type=application/atom+xml>%s
     <#/multipart>"
  "MML template for multipart/related posts.")

(defvar gphoto-photo-mime-separator
  "--===-=-="
  "Mime separator.")

(defvar gphoto-photo-multipart-header
  " Content-Type: multipart/related; boundary=\"%s\"

%s
Content-Type: image/jpeg
Content-Disposition: inline; filename=%s
CONTENT-LENGTH: %s
"
  "Mime preamble to insert at front of post.")

(defvar gphoto-photo-raw-header
  "Content-type: image/jpeg
Slug: %s
Content-length: %s\n"
  "Mime preamble to insert at front of raw image post.")

(defun gphoto-photo-generate-mime (photo)
  "Generate Post body"
  (declare (special gphoto-photo-raw-header))
  (insert
   (format
    gphoto-photo-raw-header
    (file-name-nondirectory(gphoto-photo-filepath photo))
    (nth 7 (file-attributes (gphoto-photo-filepath photo)))))
  (insert "\n")
  (insert-file-contents (gphoto-photo-filepath photo)))

(defsubst gphoto-post-photo (photo location)
  "Post photo to location and return HTTP response."
  (declare (special g-cookie-options gphoto-auth-handle
                    g-curl-program g-curl-common-options
                    g-curl-data-binary))
  (g-using-scratch
   (gphoto-photo-generate-mime photo)
   (let ((cl (format "-H Content-length:%s" (buffer-size)))
         (status nil))
     (shell-command-on-region
      (point-min) (point-max)
      (format
       "%s %s %s %s %s %s -i -X POST --data-binary @- %s 2>/dev/null"
       g-curl-program g-curl-common-options g-curl-data-binary cl
       (g-authorization gphoto-auth-handle)
       g-cookie-options
       location)
      (current-buffer) 'replace)
     (list (g-http-headers (point-min) (point-max))
           (g-http-body (point-min) (point-max))))))

;;;###autoload
(defun gphoto-photo-add (album-location)
  "Add a photo to an existing album."
  (interactive "sEnter Album URI: ")
  (declare (special gphoto-auth-handle))
  (g-auth-ensure-token gphoto-auth-handle)
  (let ((photo (gphoto-read-photo))
        (headers nil)
        (body nil)
        (response nil))
    (setq response
          (gphoto-post-photo photo album-location))
    (setq headers (first response)
          body (second response))
    (when (or  (string-equal "201" (g-http-header "Status" headers))
               (string-equal "200" (g-http-header "Status" headers)))
      (and (> (length body)0)
           (g-display-xml-string body g-atom-view-xsl)))))

;;}}}
;;{{{ Sign out:

(defun gphoto-sign-out()
  "Resets client so you can start with a different userid."
  (interactive)
  (declare (special gphoto-auth-handle
                    gphoto-user-email gphoto-user-password))
  (message "Signing out %s from Picasa"
           (g-auth-email gphoto-auth-handle))
  (setq gphoto-user-email nil
        gphoto-user-password nil)
  (setq gphoto-auth-handle (make-gphoto-auth)))

;;}}}
(provide 'gphoto)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
