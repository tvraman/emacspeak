;;; g-contacts.el --- Contacts API
;;;$Id: gcontacts.el 5798 2008-08-22 17:35:01Z tv.raman.tv $
;;; $Author: raman $
;;; Description:  Contacts that all clients start from.
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

;;; Provide Google Contacts Access to Emacs
;;; We use the GData Python library from:
;;;  URL: http://gdata-python-client.googlecode.com/svn/trunk
;;; Start Python in a subprocess with the Contacts API loaded.
;;; Emacs will make calls into this running process for managing contacts.
;;; One mode of pushing contacts from Emacs to Google Contacts is via BBDB

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))

;;}}}
;;{{{ Customizations

(defgroup gcontacts nil
  "Google contacts"
  :group 'g)

(defcustom gcontacts-user-email nil
  "Mail address that identifies calendar user."
  :type '(choice
          (const :tag "none" nil)
          (string :tag "username@gmail.com" ""))
  :group 'gcontacts)

(defcustom gcontacts-user-password nil
  "Password for authenticating to calendar account."
  :type '(radio (const :tag "Prompt for password" nil)
                (string :tag "Save password in .emacs"))
  :group 'gcontacts)

;;}}}
;;{{{ Constants

(defconst gcontacts-service-name "contacts"
  "Service name for accessing Google contacts.")

(defsubst gcontacts-p (service)
  "Check if this is Calendar."
  (declare (special gcontacts-service-name))
  (string-equal service gcontacts-service-name))

;;}}}
;;{{{ contacts Initialize

(defvar gcontacts-process nil
  "Process handle to Python subprocess that holds the ContactsShell.")

;;;###autoload
(defun gcontacts-initialize (username)
  "Initialize GContacts process handle."
  (interactive
   (list
    (read-from-minibuffer "User Email: "
                          nil nil nil nil
                          gcontacts-user-email)))
  (declare (special gcontacts-process gcontacts-user-email))
  (when (and gcontacts-process
             (eq (process-status gcontacts-process) 'run))
    (delete-process gcontacts-process))
  (let ((process-environment
         (list
          (substitute-env-vars (format "PYTHONPATH=%s:$PYTHONPATH"
                                       (expand-file-name "python" g-directory))))))
    (setq gcontacts-process
          (start-process
           "Contacts" "*Contacts*"
           (executable-find "python")))
    (process-send-string gcontacts-process
                         "import contacts\n")
    (process-send-string
     gcontacts-process
     (format "s=contacts.Shell('%s', '%s')\n"
             username
             (read-passwd "Password: ")))))

;;}}}
;;{{{ Add a contact:
;;;###autoload
(defun gcontacts-create (name email mobile)
  "Create a new contact as specified."
  (interactive "sName: \nsEMail: \nsPhone: ")
  (declare (special gcontacts-process))
  (when (eq 'run (process-status gcontacts-process))
    (process-send-string gcontacts-process
                         (format "s.CreateContact('%s', '%s', '%s')\n"
                                 name email mobile))))

;;}}}

(provide 'gcontacts)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
