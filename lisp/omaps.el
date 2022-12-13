;;; omaps.el --- Open street  Maps  -*- lexical-binding: t; -*-
;; $Author: raman $
;; Description:  Open Street Maps
;; Keywords: Open Street    Maps API
;;{{{  LCD Archive entry:

;; LCD Archive Entry:
;; gcal| T. V. Raman |tv.raman.tv@gmail.com
;; An emacs interface to Reader|
;; 
;;  $Revision: 1.30 $ |
;; Location undetermined
;; License: GPL
;; 

;;}}}
;;{{{ Copyright:

;; Copyright (c) 2006 and later, Google Inc.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:

;;     * Redistributions of source code must retain the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer.  * Redistributions in binary form must reproduce
;;       the above copyright notice, this list of conditions and the
;;       following disclaimer in the documentation and/or other
;;       materials provided with the distribution.  * The name of the
;;       author may not be used to endorse or promote products derived
;;       from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;; Implements geocoding via the Open Street Maps API
;; https://nominatim.org/
;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'g-utils)

;;}}}
;;{{{ Address Structure 

(cl-defstruct omaps--location
  address
  alias                                 ; short-form entered by user
  zip
  lat-lng)

(defsubst omaps-locations-load ()
  "Load saved Omaps locations."
  (interactive)
  (cl-declare (special omaps-locations-file omaps-locations-loaded-p))
  (when (file-exists-p omaps-locations-file)
    (setq omaps-locations-loaded-p (ems--fastload omaps-locations-file))))

(defvar omaps-location-table (make-hash-table  :test  #'equal)
  "Hash table that memoizes geolocation.")

;;;###autoload
(defun omaps-address-location (address)
  "Returns omaps--location structure. "
  (cl-declare (special omaps-location-table omaps-locations-loaded-p))
  (unless omaps-locations-loaded-p (omaps-locations-load))
  (let ((found (gethash address omaps-location-table))
        (result nil))
    (cond
      (found found)
      (t ;;; Get geocode from network  and  memoize
       (setq result 
             (let-alist (aref (omaps-geocode address 'raw) 0)
                        (make-omaps--location
                         :alias address
                         :address .formatted_address
                         :zip
                         (g-json-get
                          'short_name
                          (cl-find-if ; component whose type contains postal_code
                                      #'(lambda (v)
                                          (cl-find
                                           "postal_code" (g-json-get 'types v) :test #'string=))
                                      .address_components))
                         :lat-lng .geometry.location)))
       (puthash  address result omaps-location-table)
       (puthash  (omaps--location-address result) result omaps-location-table)
       (omaps-locations-save)
       result))))

;;;###autoload
(defun omaps-address-geocode(address)
  "Return lat/long for a given address."
  (omaps--location-lat-lng (omaps-address-location address)))

(defun omaps-address-zip(address)
  "Return ZIP code  for a given address."
  (omaps--location-zip (omaps-address-location address)))

(defvar omaps-locations-loaded-p nil
  "Record if Locations cache  is loaded.")
(defvar emacspeak-user-directory)

(defvar omaps-locations-file
  (expand-file-name "omaps-locations" emacspeak-user-directory)
  "File where we save Locations.")
(declare-function emacspeak-auditory-icon "emacspeak-sounds" (icon))

(defun omaps-locations-save ()
  "Save Omaps Locations."
  (interactive)
  (cl-declare (special omaps-locations-file omaps-location-table))
  (let ((buffer (find-file-noselect omaps-locations-file))
        (print-length nil)
        (print-level nil))
    (with-current-buffer buffer
      (erase-buffer)
      (insert  ";;; Auto-generated.\n\n")
      (insert "(setq omaps-location-table\n")
      (pp omaps-location-table (current-buffer))
      (insert ") ;;; set hash table\n\n")
      (insert "(setq omaps-locations-loaded-p t)\n")
      (save-buffer))
    (when (called-interactively-p 'interactive)
      (message "Saved Omaps Locations."))
    (when (featurep 'emacspeak)
      (emacspeak-auditory-icon 'save-object))))

;;}}}
;;{{{ Maps Geo-Coding and Reverse Geo-Coding:

;; https://nominatim.org/
;; eg: '/search?q=6217+hopi+court+San+Jose+CA+95123&format=json' | jq

(defvar omaps-geocoder-base
  "https://nominatim.openstreetmap.org/"
  "Base URL  end-point for talking to the Open Street  Maps Geocoding service.")

(defun omaps-geocoder-url (address)
  "Return URL   for geocoding address."
  (cl-declare (special omaps-geocoder-base ))
  (format "%s/search?q=%s&format=json"
          omaps-geocoder-base
          (g-url-encode address)))

(defun omaps-reverse-geocoder-url (location)
  "Return URL   for reverse geocoding location."
  (cl-declare (special omaps-geocoder-base))
  (format "%s/reverse?lat=%s&lon=%s&format=json"
          omaps-geocoder-base
          (cdr (assoc 'lat location))
          (cdr (assoc 'lng location))))

;;;###autoload
(defun omaps-geocode (address &optional full)
  "Geocode given address using nominatim search.. "
  (let ((result
          (g-json-from-url (omaps-geocoder-url (g-url-encode
                                                address)))))
    (cond
      (full result)
      (t (list
          (cons 'lat (g-json-path-lookup "[0].lat" result))
          (cons 'lng (g-json-path-lookup "[0].lon" result)))))))

;;;###autoload
(defun omaps-reverse-geocode (lat-long &optional full)
  "Reverse geocode lat-long.
Optional argument `full' returns full  object."
  (let ((result
          (g-json-from-url
                  (omaps-reverse-geocoder-url lat-long))))
    (cond
     (full result)
     (t (g-json-get 'display_name result)))))

;; Example of use:
(defvar omaps-my-location
  nil
  "Geo coordinates --- automatically set by reverse geocoding omaps-my-address")

(defvar omaps-my-zip
  nil
  "Postal Code --- automatically set by reverse geocoding omaps-my-address")

(declare-function
 emacspeak-calendar-setup-sunrise-sunset  "emacspeak-calendar" nil)
;;;###autoload
(defcustom omaps-my-address
  nil
  "Location address. Setting this updates omaps-my-location
coordinates via geocoding."
  :type '(choice
          (const :tag "None" nil)
          (string  :tag "Address"))
  :set
  #'(lambda (sym val)
      (cl-declare (special omaps-my-location))
      (when val
        (setq omaps-my-location (omaps-address-location val))
        (setq omaps-my-zip (omaps--location-zip omaps-my-location))
        (set-default sym (omaps--location-address omaps-my-location))
        val))
  :group 'gweb)

;;}}}

(provide 'omaps)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}
