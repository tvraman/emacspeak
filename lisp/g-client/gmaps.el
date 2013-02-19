;;; gmaps.el --- Google Maps
;;;$Id: gmaps.el 8157 2013-02-19 01:31:05Z tv.raman.tv $
;;; $Author: raman $
;;; Description:  Google Maps -> Lisp
;;; Keywords: Google   Maps API
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

;;; Implements the Google Maps API

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'g-utils)

;;}}}
;;{{{ Customizations

(defgroup gmaps nil
  "Google Maps"
  :group 'g)

;;}}}
;;{{{ Maps Geo-Coding and Reverse Geo-Coding:

;;; See http://feedproxy.google.com/~r/GoogleGeoDevelopersBlog/~3/0aP4dsogPJ4/introducing-new-google-geocoding-web.html

(defvar gmaps-geocoder-base
  "http://maps.google.com/maps/api/geocode/json?"
  "Base URL  end-point for talking to the Google Maps Geocoding service.")

(defsubst gmaps-geocoder-url (address)
  "Return URL   for geocoding address."
  (declare (special gmaps-geocoder-base))
  (format "%saddress=%s&sensor=false"
          gmaps-geocoder-base address))

(defsubst gmaps-reverse-geocoder-url (address)
  "Return URL   for reverse geocoding location."
  (declare (special gmaps-geocoder-base))
  (format "%slatlng=%s&sensor=false"
          gmaps-geocoder-base address))

;;;###autoload
(defun gmaps-geocode (address &optional raw-p)
  "Geocode given address.
Optional argument `raw-p' returns complete JSON  object."
  (let ((result 
         (g-json-get-result
          (format "%s --max-time 2 --connect-timeout 1 %s '%s'"
                  g-curl-program g-curl-common-options
                  (gmaps-geocoder-url
                   (g-url-encode address))))))
    
    (unless
        (string= "OK" (g-json-get 'status result))
      (error "Error geo-coding location."))
    (cond
     (raw-p (g-json-get 'results result))
     (t
      (g-json-get 'location 
                  (g-json-get 'geometry
                              (aref (g-json-get 'results result) 0)))))))

;;;###autoload
(defun gmaps-reverse-geocode (lat-long &optional raw-p)
  "Reverse geocode lat-long.
Optional argument `raw-p' returns raw JSON  object."
  (let ((result 
         (g-json-get-result
          (format "%s --max-time 2 --connect-time 1%s '%s'"
                  g-curl-program g-curl-common-options
                  (gmaps-reverse-geocoder-url
                   (format "%s,%s"
                           (g-json-get 'lat lat-long)
                           (g-json-get 'lng   lat-long)))))))
    (unless (string= "OK" (g-json-get 'status result))
      (error "Error reverse geo-coding."))
    (cond
     (raw-p (g-json-get 'results result))
     (t
     (g-json-get 'formatted_address
                 (aref (g-json-get 'results result) 0))))))

;;; Example of use:
;;;###autoload
(defvar gweb-my-location
  nil
  "Geo coordinates --- automatically set by reverse geocoding gweb-my-address")

;;;###autoload
(defcustom gweb-my-address
  nil
  "Location address. Setting this updates gweb-my-location coordinates  via geocoding."
  :type '(choice
          (const :tag "None" nil)
                 (string  :tag "Address"))
  :set  #'(lambda (sym val)
            (declare (special gweb-my-location))
            (when val 
              (setq gweb-my-location (gmaps-geocode val))
              (when (featurep 'emacspeak)
                (emacspeak-calendar-setup-sunrise-sunset)))
            (set-default sym val))
  :group 'gweb)

;;}}}
;;{{{ Maps Directions 

;;; See  https://developers.google.com/maps/documentation/directions/
(defvar gmaps-directions-base
  "http://maps.googleapis.com/maps/api/directions/json?sensor=false&origin=%s&destination=%s&mode=%s&departure_time=%d"
  "Base URL  end-point for talking to the Google Maps directions service.")

(defsubst gmaps-directions-url (origin destination mode)
  "Return URL   for getting directions from origin to destination.
Parameters 'origin' and 'destination' are  be url-encoded."
  (declare (special gmaps-directions-base))
  (format gmaps-directions-base  origin destination
          mode (float-time)))


;;; Places:
;; 
(defvar gmaps-places-base
  "https://maps.googleapis.com/maps/api/place/%s/json?sensor=false&key=%s"
  "Base URL  end-point for talking to the Google Maps Places service.")

(defsubst gmaps-places-url (type key)
  "Return URL  for Places services.
Parameter `type' is one of nearbysearch or textsearch.
Parameter `key' is the API  key."
  (declare (special gmaps-places-base))
  (format gmaps-places-base  type key))
          

;;}}}
(provide 'gmaps)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
