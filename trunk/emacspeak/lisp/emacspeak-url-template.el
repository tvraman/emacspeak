;;; emacspeak-url-template.el --- Create library of URI templates
;;; $Id$
;;; $Author$
;;; Description:   Implement library of URI templates
;;; Keywords: Emacspeak, Audio Desktop
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision$ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1995 -- 2000, T. V. Raman<raman@cs.cornell.edu>
;;; All Rights Reserved.
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-speak)
(require 'emacspeak-sounds)

;;}}}
;;{{{  Introduction:

;;; Commentary:


;;; It is often useful to have ``parameterized hot list entries''
;;; i.e., hotlist entries  that are ``templates'' for the
;;; actual URL.
;;; The user provides values for the parameterized portons
;;; of the URL e.g. the date.

;;}}}
;;{{{  structures 

(defstruct (emacspeak-url-template
            (:constructor
             emacspeak-url-template-constructor))
  name ;Human-readable name
  template  ;template URL string 
generators  ; list of param generator
)

  ;;}}}
;;{{{ Helpers

(defun emacspeak-url-template-url (ut)
  "Instnatiate URL identified by URL template."
        (apply 'format
               ( emacspeak-url-template-template ut)
               (mapcar
                (function
                 (lambda (g)
                   (cond
                    ((stringp g)
                     (read-from-minibuffer g))
                    (t (funcall g)))))
                (emacspeak-url-template-generators ut))))

;;}}}
;;{{{  persistent store 

(defvar emacspeak-url-template-table (make-hash-table :test 'equal)
  "Stores URL templates. ")
(defun emacspeak-url-template-set (key ut)
  "Add  specified template to key. "
  (declare (special emacspeak-url-template-table))
  (setf (gethash key emacspeak-url-template-table ) ut))

(defun emacspeak-url-template-get (key)
  "Lookup key and return corresponding template. "
  (declare (special emacspeak-url-template-table))
  (gethash key emacspeak-url-template-table))

;;}}}
;;{{{  define resources 

(defun emacspeak-url-template-define (name template generators )
  "Define a URL template."
  (declare (special emacspeak-url-template-table))
  (emacspeak-url-template-set
   name
   (emacspeak-url-template-constructor :name name
                                       :template template
                                       :generators generators)))
                               

(defun emacspeak-url-template-load (file)
  "Load URL template resources from specified location."
(interactive
   (list
    (read-file-name "Load URL templates from file: "
                    emacspeak-resource-directory)))
  (condition-case nil
                  (progn
  (load
   (expand-file-name  file emacspeak-resource-directory)))
  (error (message "Error loading resources from %s "
                  file))))


(defun emacspeak-url-template-save (file)
  "Save out url templates."
  (interactive
   (list
    (read-file-name "Save URL templates to file: "
                    emacspeak-resource-directory)))
  (declare (special emacspeak-resource-directory))
  (let ((buffer (find-file-noselect
                 (expand-file-name file
                                   emacspeak-resource-directory))))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (loop for key being the hash-keys of
            emacspeak-url-template-table
            do
            (insert
             (format
              "\n(setf
 (gethash %s emacspeak-url-template-table)\n %s)"
              (prin1-to-string key)
               (prin1-to-string (emacspeak-url-template-get key)))))
      (basic-save-buffer)
      (kill-buffer buffer))))

;;}}}
;;{{{  template resources 
;;{{{ w3c 
(emacspeak-url-template-define "w3c Lists"
                               "http://lists.w3.org/Archives/Member/w3c-%s/%s/"
                               (list
                                'emacspeak-url-template-get-w3c-group 
                                'emacspeak-url-template-get-w3c-year/month))

(defun emacspeak-url-template-get-w3c-group ()
  "Get name of W3C group "
  (read-from-minibuffer "W3C group: "))

(defun emacspeak-url-template-get-w3c-year/month ()
  "Get year/month"
  (read-from-minibuffer "Date range: "
                        (downcase 
                         (format-time-string "%Y%h"
                                             (current-time)))))


  
  
;;}}}
;;{{{ cnn 

(emacspeak-url-template-define "CNN headlines "
                               "http://www.cnn.com/quicknews/print.html"
                               nil)

(defun emacspeak-url-template-date-YearMonthDate ()
  "Return today as yyyymmdd"
  (read-from-minibuffer "Date:"
                        (format-time-string "%Y%m%d") nil nil nil 
                        (format-time-string "%Y%m%d")))

(defun emacspeak-url-template-date-year/month/date ()
  "Return today as yyyy/mm/dd"
  (read-from-minibuffer "Date:"
                        (format-time-string "%Y/%m/%d") nil nil nil 
                        (format-time-string "%Y/%m/%d")))

(defun emacspeak-url-template-date-month/date ()
  "Return today as mm/dd"
  (read-from-minibuffer "Date:"
                        (format-time-string "%m/%d") nil nil nil 
                        (format-time-string "%m/%d")))





(emacspeak-url-template-define "CNN Tecnology "
"http://www.cnn.com/2001/TECH/%s/"
(list 'emacspeak-url-template-date-month/date))

(emacspeak-url-template-define "CNN computing "
"http://www.cnn.com/2001/TECH/computing/%s/"
(list 'emacspeak-url-template-date-month/date))

(emacspeak-url-template-define "CNN HotStocks "
                                "http://cgi.cnnfn.com/output/pfv/%s/markets/hotstox/"
 (list 
'emacspeak-url-template-date-year/month/date))


(emacspeak-url-template-define "CNN Markets New York"
                                "http://cgi.cnnfn.com/output/pfv/%s/markets/markets_newyork/"
 (list 'emacspeak-url-template-date-year/month/date))

;;}}}
;;{{{ technet cast from DDJ

(emacspeak-url-template-define "TechNetCast Stream ID" 
                               "http://technetcast.ddj.com/tnc_play.m3u?stream_id=%s"
                               (list
                                (lambda nil 
(read-from-minibuffer "Stream Id"))))

;;{{{ sourceforge

(emacspeak-url-template-define "sourceforge project" 
                               "http://sourceforge.net/projects/%s"
                               (list
                                (lambda nil 
(read-from-minibuffer "Project name"))))

(emacspeak-url-template-define "sourceforge download" 
                               "http://prdownloads.sourceforge.net/%s"
                               (list
                                (lambda nil 
(read-from-minibuffer "Project name"))))

;;}}}
;;{{{ India Today 

(emacspeak-url-template-define "India Today "
                               "http://www.india-today.com/itoday/%s/index.shtml"
                               (list  'emacspeak-url-template-date-YearMonthDate))

;;}}}

;;}}}
;;}}}
;;{{{ Interactive commands 

(defun emacspeak-url-template-open (ut)
  "Fetch resource identified by URL template."
  (browse-url  (emacspeak-url-template-url ut)))

(defun emacspeak-url-template-fetch ()
  "Prompt for  URL template and fetch specified resource."
  (interactive)
  (declare (special emacspeak-url-template-table))
  (let ((completion-ignore-case t)
        (table
         (loop for key being the hash-keys of
               emacspeak-url-template-table
               collect (list 
                        (format "%s" key)
                        (format "%s" key)))))
    (emacspeak-url-template-open
     (emacspeak-url-template-get
      (completing-read "Resource: "
                       table)))))

;;}}}
(provide 'emacspeak-url-template)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
