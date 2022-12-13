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

;;}}}
(provide 'omaps)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}
