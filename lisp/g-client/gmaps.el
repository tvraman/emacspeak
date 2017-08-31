;;; gmaps.el --- Google Maps  -*- lexical-binding: t; -*-
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

;;{{{  introduction
;;; Commentary:
;;; Implements the Google Maps API
;;; Code:
;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'g-utils)
(require 'pp)
;;}}}
;;{{{ Customizations

(defgroup gmaps nil
  "Google Maps"
  :group 'g)

;;}}}
;;{{{ Address Structure 

(cl-defstruct gmaps--location
  address
  alias ; short-form entered by user
  zip
  lat-lng)

(defun gmaps-locations-load ()
  "Load saved GMaps locations."
  (interactive)
  (cl-declare (special gmaps-locations-file))
  (when (file-exists-p gmaps-locations-file)
    (load-file gmaps-locations-file)))

(defvar gmaps-location-table (make-hash-table  :test  #'equal)
  "Hash table that memoizes geolocation.")
;;;###autoload
(defun gmaps-address-location (address)
  "Returns gmaps--location structure. Memoized to save network calls."
  (cl-declare (special gmaps-location-table gmaps-locations-loaded-p))
  (unless gmaps-locations-loaded-p (gmaps-locations-load))
  (let ((found (gethash address gmaps-location-table))
        (result nil))
    (cond
     (found found)
     (t ;;; Get geocode from network  and  memoize
      (setq result 
            (let-alist (aref (gmaps-geocode address 'raw) 0)
              (make-gmaps--location
               :alias address
               :address .formatted_address
               :zip (g-json-get 'short_name
                                (cl-find-if ; component whose type contains postal_code
                                 #'(lambda (v) (cl-find "postal_code" (g-json-get 'types v) :test #'string=))
                                 .address_components))
               :lat-lng .geometry.location)))
      (puthash  address result gmaps-location-table)
      (puthash  (gmaps--location-address result) result gmaps-location-table)
      (gmaps-locations-save)
      result))))
;;;###autoload
(defun gmaps-address-geocode(address)
  "Return lat/long for a given address."
  (gmaps--location-lat-lng (gmaps-address-location address)))

(defun gmaps-address-zip(address)
  "Return ZIP code  for a given address."
  (gmaps--location-zip (gmaps-address-location address)))

(defvar gmaps-locations-loaded-p nil
  "Record if Locations cache  is loaded.")
(defvar emacspeak-resource-directory)

(defvar gmaps-locations-file
  (expand-file-name "gmaps-locations" emacspeak-resource-directory)
  "File where we save Locations.")
(declare-function emacspeak-auditory-icon (icon) "emacspeak-sounds")

(defun gmaps-locations-save ()
  "Save GMaps Locations."
  (interactive)
  (cl-declare (special gmaps-locations-file gmaps-location-table))
  (let ((buffer (find-file-noselect gmaps-locations-file))
        (print-length nil)
        (print-level nil))
    (with-current-buffer buffer
      (erase-buffer)
      (insert  ";;; Auto-generated.\n\n")
      (insert "(setq gmaps-location-table\n")
      (pp gmaps-location-table (current-buffer))
      (insert ") ;;; set hash table\n\n")
      (insert "(setq gmaps-locations-loaded-p t)\n")
      (save-buffer))
    (when (called-interactively-p 'interactive)
      (message "Saved GMaps Locations."))
    (when (featurep 'emacspeak)
    (emacspeak-auditory-icon 'save-object)))                                    )

;;}}}

;;{{{ Maps Geo-Coding and Reverse Geo-Coding:

;;; See http://feedproxy.google.com/~r/GoogleGeoDevelopersBlog/~3/0aP4dsogPJ4/introducing-new-google-geocoding-web.html

(defvar gmaps-geocoder-base
  "https://maps.google.com/maps/api/geocode/json?"
  "Base URL  end-point for talking to the Google Maps Geocoding service.")

(defun gmaps-geocoder-url (address)
  "Return URL   for geocoding address."
  (cl-declare (special gmaps-geocoder-base))
  (format "%saddress=%s&sensor=false"
          gmaps-geocoder-base address))

(defun gmaps-reverse-geocoder-url (location)
  "Return URL   for reverse geocoding location."
  (cl-declare (special gmaps-geocoder-base))
  (format "%slatlng=%s&sensor=false"
          gmaps-geocoder-base location))

;;;###autoload
(defun gmaps-geocode (address &optional raw-p)
  "Geocode given address.
Optional argument `raw-p' returns complete JSON  object."
  (let ((result
         (g-json-from-url (gmaps-geocoder-url (g-url-encode address)))))
    (unless (string= "OK" (g-json-get 'status result))
      (error "Error geo-coding location."))
    (cond
     (raw-p (g-json-get 'results result))
     (t (g-json-path-lookup "results.[0].geometry.location" result)))))

;;;###autoload
(defun gmaps-reverse-geocode (lat-long &optional raw-p)
  "Reverse geocode lat-long.
Optional argument `raw-p' returns raw JSON  object."
  (let ((result
         (g-json-get-result
          (format "%s --max-time 5 --connect-timeout 3 %s '%s'"
                  g-curl-program g-curl-common-options
                  (gmaps-reverse-geocoder-url
                   (format "%s,%s"
                           (g-json-get 'lat lat-long)
                           (g-json-get 'lng   lat-long)))))))
    (unless (string= "OK" (g-json-get 'status result))
      (error "Error reverse geo-coding."))
    (cond
     (raw-p (g-json-get 'results result))
     (t (g-json-path-lookup "results.[0].formatted_address" result)))))

(defun gmaps-postal-code-from-location (location)
  "Reverse geocode location and return postal coe."
  (condition-case nil
      (g-json-get
       'short_name
       (cl-find-if  ; component whose type contains postal_code
        #'(lambda (v) (cl-find "postal_code" (g-json-get 'types v) :test #'string=))
        (g-json-get ; from address_components at finest granularity
         'address_components
         (aref (gmaps-reverse-geocode location 'raw) 0))))
    (error "")))

;;; Example of use:
(defvar gweb-my-location
  nil
  "Geo coordinates --- automatically set by reverse geocoding gweb-my-address")

(defvar gweb-my-zip
  nil
  "Postal Code --- automatically set by reverse geocoding gweb-my-address")

(declare-function  emacspeak-calendar-setup-sunrise-sunset  nil "emacspeak-calendar")
;;;###autoload
(defcustom gweb-my-address
  nil
  "Location address. Setting this updates gweb-my-location coordinates  via geocoding."
  :type '(choice
          (const :tag "None" nil)
          (string  :tag "Address"))
  :set
  #'(lambda (sym val)
      (cl-declare (special gweb-my-location))
      (when val
        (setq gweb-my-location (gmaps-address-location val))
        (setq gweb-my-zip (gmaps--location-zip gweb-my-location))
        (set-default sym (gmaps--location-address gweb-my-location))
        (when (featurep 'emacspeak) (emacspeak-calendar-setup-sunrise-sunset))
        val))
  :group 'gweb)

;;}}}
;;{{{ Maps Directions

;;; See  https://developers.google.com/maps/documentation/directions/
(defvar gmaps-directions-base
  "https://maps.googleapis.com/maps/api/directions/json?sensor=false&origin=%s&destination=%s&mode=%s&departure_time=%d"
  "Base URL  end-point for talking to the Google Maps directions service.")

(defun gmaps-directions-url (origin destination mode)
  "Return URL   for getting directions from origin to destination.
Parameters 'origin' and 'destination' are  be url-encoded."
  (cl-declare (special gmaps-directions-base))
  (format gmaps-directions-base  origin destination
          mode (float-time)))

;;; Places:
;;; https://developers.google.com/places/documentation/
;;
(defvar gmaps-places-base
  "https://maps.googleapis.com/maps/api/place/%s/json?sensor=false&key=%s"
  "Base URL  end-point for talking to the Google Maps Places service.")

(defun gmaps-places-url-base (query-type key)
  "Return URL  for Places services.
Parameter `query-type' is one of nearbysearch or textsearch.
Parameter `key' is the API  key."
  (cl-declare (special gmaps-places-base))
  (format gmaps-places-base  query-type key))

;;}}}
;;{{{ Google Maps API V3

;;; See  https://developers.google.com/maps/documentation/directions/
(defvar gmaps-modes '("driving" "walking" "bicycling" "transit")
  "Supported modes for getting directions.")

(defun gmaps-routes (origin destination mode)
  "Return routes as found by Google Maps Directions."
  (let-alist (g-json-from-url (gmaps-directions-url origin  destination mode))
    (cond
     ((string= "OK" .status) .routes)
     (t (error "Status %s from Maps" .status)))))

;;; https://developers.google.com/places/

(defcustom gmaps-places-key nil
  "Places API  key --- goto  https://code.google.com/apis/console to get one."
  :type '(choice
          (const :tag "None" nil)
          (string :value ""))
  :group 'gmaps)

;;}}}
;;{{{ Maps UI:

(make-variable-buffer-local 'gmaps-current-location)

(define-derived-mode gmaps-mode special-mode
  "Google Maps Interaction"
  "A Google Maps front-end for the Emacspeak desktop."
  (let ((start (point))
        (inhibit-read-only t))
    (setq buffer-undo-list t)
    (goto-char (point-min))
    (insert "Google Maps Interaction")
    (put-text-property start (point) 'face font-lock-doc-face)
    (insert "\n\f\n")
    (and gweb-my-address (gmaps-set-current-location gweb-my-address))
    (setq header-line-format
          '("Google Maps: "
            (:eval   (gmaps--location-address gmaps-current-location))))))

(cl-declaim (special gmaps-mode-map))

(cl-loop for k in
         '(
           ("d" gmaps-driving-directions)
           ("w" gmaps-walking-directions)
           ("t" gmaps-transit-directions)
           ("b" gmaps-bicycling-directions)
           ("n" gmaps-places-nearby)
           ("c" gmaps-set-current-location)
           ("f" gmaps-set-current-filter)
           ("r" gmaps-set-current-radius)
           ("s" gmaps-places-search)
           (" " gmaps-place-details)
           ("\M-i" backward-button)
           ("\C-i" forward-button)
           ("[" backward-page)
           ("]" forward-page)
           )
         do
         (define-key  gmaps-mode-map (cl-first k) (cl-second k)))

(defvar gmaps-interaction-buffer "*Google Maps*"
  "Google Maps interaction buffer.")

;;;###autoload
(defun gmaps ()
  "Google Maps Interaction."
  (interactive)
  (cl-declare (special gmaps-interaction-buffer))
  (let ((buffer (get-buffer gmaps-interaction-buffer)))
    (cond
     ((buffer-live-p buffer) (switch-to-buffer buffer))
     (t
      (with-current-buffer (get-buffer-create gmaps-interaction-buffer)
        (erase-buffer)
        (gmaps-mode)
        (setq buffer-read-only t))
      (switch-to-buffer gmaps-interaction-buffer)))))

;;}}}
;;{{{ Directions:

(defun gmaps-display-leg (leg)
  "Display a leg of a route."
  (let ((inhibit-read-only t)
        (start (point)))
    (insert "<ol>")
    (cl-loop
     for step across (g-json-get 'steps leg) do
     (let-alist step
       (insert
        (format "<li>:%s%s%s</li>\n"
                .html_instructions .distance.text .duration.text))))
    (shr-render-region start (point))))

(defun gmaps-display-route (route)
  "Display route in a Maps buffer."
  (let-alist route
    (let ((i 1)
          (inhibit-read-only t)
          (length (length  .legs))
          (leg nil))
      (insert (format "Summary: %s\n" .summary))
      (cond
       ((= 1 length)
        (setq leg (aref .legs 0))
        (insert (format "From %s to %s\n%s\t%s\n"
                        (g-json-get 'start_address leg)
                        (g-json-get 'end_address leg)
                        (g-json-get 'text (g-json-get 'distance leg))
                        (g-json-get 'text (g-json-get 'duration leg))))
        (gmaps-display-leg (aref .legs 0)))
       (t
        (cl-loop
         for leg across .legs
         do
         (insert (format "Leg:%d: From %s to %s\n"
                         i
                         (g-json-get 'start_address leg)
                         (g-json-get 'end_address leg)))
         (gmaps-display-leg leg)
         (cl-incf i))))
      (insert (format "Warnings: %s\n" .warnings))
      (insert (format "Copyrights: %s\n\f\n" .copyrights)))))

(defun gmaps-read-origin-destination ()
  "Read origin and destination addresses using context-based
guesses. Addresses are returned url-encoded; if available
origin/destination may be returned as a lat,long string."
  (cl-declare (special gmaps-current-location))
  (let* ((maps-data (get-text-property (point) 'maps-data))
         (place-location (and maps-data
                              (g-json-lookup
                               "geometry.location"
                               (get-text-property (point) 'maps-data))))
         (origin nil)
         (destination nil))
    (setq origin
          (cond
           (gmaps-current-location
            (url-hexify-string (gmaps--location-address gmaps-current-location)))
           (t (url-hexify-string (read-from-minibuffer "Start Address: ")))))
    (setq destination
          (cond
           (place-location
            (format "%s,%s"
                    (g-json-get 'lat place-location)
                    (g-json-get 'lng place-location)))
           (t (url-hexify-string (read-from-minibuffer "Destination  Address: ")))))
    (list origin destination)))

(defun gmaps-display-routes (routes)
  "Display routes in Maps interaction buffer."
  (let ((i 1)
        (length (length routes))
        (inhibit-read-only t))
    (cond
     ((= 1 length) (gmaps-display-route (aref routes 0)))
     (t
      (cl-loop
       for route across routes do
       (insert (format  "\nRoute %d\n" i))
       (cl-incf i)
       (gmaps-display-route route))))))

(defun gmaps-driving-directions (origin destination)
  "Driving directions from Google Maps."
  (interactive (gmaps-read-origin-destination))
  (gmaps-directions origin destination "driving"))

(defun gmaps-walking-directions (origin destination)
  "Walking directions from Google Maps."
  (interactive (gmaps-read-origin-destination))
  (gmaps-directions origin destination "walking"))

(defun gmaps-bicycling-directions (origin destination)
  "Biking directions from Google Maps."
  (interactive (gmaps-read-origin-destination))
  (gmaps-directions origin destination "bicycling"))

(defun gmaps-transit-directions (origin destination)
  "Transit directions from Google Maps."
  (interactive (gmaps-read-origin-destination))
  (gmaps-directions origin destination "transit"))

(defun gmaps-directions (origin destination mode)
  "Display  directions obtained from Google Maps."
  (interactive (gmaps-read-origin-destination))
  (unless (eq major-mode 'gmaps-mode)
    (error "Not in a Maps buffer."))
  (let ((inhibit-read-only t)
        (start (point-max))
        (routes (gmaps-routes origin destination mode)))
    (goto-char (point-max))
    (insert (format "%s Directions\n" (capitalize mode)))
    (when routes (gmaps-display-routes routes))
    (goto-char start)))

;;}}}
;;{{{ Places:

;;; Place Types: https://developers.google.com/places/documentation/supported_types

(defvar gmaps-place-types
  '(
    "accounting"
    "administrative_area_level_1"
    "administrative_area_level_2"
    "administrative_area_level_3"
    "airport"
    "amusement_park"
    "aquarium"
    "art_gallery"
    "atm"
    "bakery"
    "bank"
    "bar"
    "beauty_salon"
    "bicycle_store"
    "book_store"
    "bowling_alley"
    "bus_station"
    "cafe"
    "campground"
    "car_dealer"
    "car_rental"
    "car_repair"
    "car_wash"
    "casino"
    "cemetery"
    "church"
    "city_hall"
    "clothing_store"
    "colloquial_area"
    "convenience_store"
    "country"
    "courthouse"
    "dentist"
    "department_store"
    "doctor"
    "electrician"
    "electronics_store"
    "embassy"
    "fire_station"
    "floor"
    "florist"
    "funeral_home"
    "furniture_store"
    "gas_station"
    "geocode"
    "grocery_or_supermarket"
    "gym"
    "hair_care"
    "hardware_store"
    "hindu_temple"
    "home_goods_store"
    "hospital"
    "insurance_agency"
    "intersection"
    "jewelry_store"
    "laundry"
    "lawyer"
    "library"
    "liquor_store"
    "local_government_office"
    "locality"
    "locksmith"
    "lodging"
    "meal_delivery"
    "meal_takeaway"
    "mosque"
    "movie_rental"
    "movie_theater"
    "moving_company"
    "museum"
    "natural_feature"
    "neighborhood"
    "night_club"
    "painter"
    "park"
    "parking"
    "pet_store"
    "pharmacy"
    "physiotherapist"
    "plumber"
    "point_of_interest"
    "police"
    "political"
    "post_box"
    "post_office"
    "postal_code"
    "postal_code_prefix"
    "postal_town"
    "premise"
    "real_estate_agency"
    "restaurant"
    "roofing_contractor"
    "room"
    "route"
    "rv_park"
    "school"
    "shoe_store"
    "shopping_mall"
    "spa"
    "stadium"
    "storage"
    "store"
    "street_address"
    "street_number"
    "sublocality"
    "sublocality_level_1"
    "sublocality_level_2"
    "sublocality_level_3"
    "sublocality_level_4"
    "sublocality_level_5"
    "subpremise"
    "subway_station"
    "synagogue"
    "taxi_stand"
    "train_station"
    "transit_station"
    "travel_agency"
    "university"
    "veterinary_care"
    "zoo"
    )
  "List of supported Place Types.")

(defvar gmaps-current-location nil
  "Current maps location.")

(defun gmaps-set-current-location (address)
  " Set current location."
  (interactive  "sAddress: ")
  (cl-declare (special gmaps-current-location))
  (setq gmaps-current-location (gmaps-address-location address))
  (message "Moved to %s" address))

(cl-defstruct gmaps-places-filter
  type ; singleton as per new API
  types ; multiple types (until Feb 2017)
  keyword name)

(defvar gmaps-current-filter nil
  "Currently active filter. ")
(make-variable-buffer-local 'gmaps-current-filter)

(defun gmaps-places-filter-as-params (filter)
  "Convert filter structure into URL  params."
  (let ((keyword (gmaps-places-filter-keyword filter))
        (name (gmaps-places-filter-name filter))
        (types (gmaps-places-filter-types filter))
        (type (gmaps-places-filter-type filter)))
    (format "%s%s%s %s"
            (if keyword (format "&keyword=%s" keyword) "")
            (if name (format "&name=%s" name) "")
            (if type (format "&type=%s" type) "")
            (if types (format "&types=%s" (mapconcat #'identity types "|")) ""))))

(defun gmaps-places-filter-as-string (filter)
  "Convert filter structure into display-friendly string."
  (let ((keyword (gmaps-places-filter-keyword filter))
        (name (gmaps-places-filter-name filter))
        (type (gmaps-places-filter-type filter))
        (types (gmaps-places-filter-types filter)))
    (format "%s%s%s %s"
            (if keyword (format "Keyword: %s" keyword) "")
            (if name (format "Name: %s" name) "")
            (if type (format "Type: %s" type) "")
            (if types (format "Types: %s" (mapconcat #'identity types "|")) ""))))
(defun gmaps-place-read-types ()
  "Returns a list of types."
  (cl-declare (special gmaps-place-types))
  (let ((result nil)
        (type (completing-read "Type: Blank to quit " gmaps-place-types)))
    (while (not (= 0 (length type)))
      (cl-pushnew type result)
      (setq type (completing-read "Type: Blank to quit " gmaps-place-types)))
    result))

(defun gmaps-place-read-type ()
  "Returns a type."
  (cl-declare (special gmaps-place-types))
  (completing-read "Type: " gmaps-place-types))

(defun gmaps-set-current-filter (&optional all)
  "Set up filter in current buffer.
Optional interactive prefix arg prompts for all filter fields."
  (interactive "P")
  (cl-declare (special gmaps-current-filter gmaps-place-types))
  (cond
   (all
    (let ((name (read-string "Name: "))
          (keyword (read-string "Keyword: "))
          (type (gmaps-place-read-type)))
      (when (= (length name) 0) (setq name nil))
      (when (= (length keyword) 0) (setq keyword nil))
      (when (= (length type) 0) (setq type nil))
      (setq gmaps-current-filter
            (make-gmaps-places-filter
             :name name
             :keyword keyword
             :type type))))
   (t
    (setq gmaps-current-filter
          (make-gmaps-places-filter
           :name nil
           :keyword nil
           :type (gmaps-place-read-type)))))
  (let ((current-prefix-arg nil))       ;dont clear filter
    (call-interactively 'gmaps-places-nearby)))

(defvar gmaps-current-radius  5000
  "Radius  to use for places search.")

(make-variable-buffer-local 'gmaps-current-radius)

(defun gmaps-set-current-radius  (radius)
  "Set current radius"
  (interactive "nRadius: ")
  (cl-declare (special gmaps-current-radius))
  (setq gmaps-current-radius radius)
  (call-interactively 'gmaps-places-nearby))

(defun gmaps-places-nearby (&optional clear-filter)
  "Find places near current location.
Uses default radius. optional interactive prefix arg clears any active filters."
  (interactive "P")
  (cl-declare (special gmaps-current-location gmaps-current-filter
                    gmaps-places-key gmaps-places-radius))
  (unless gmaps-current-location (error "Set current location."))
  (and clear-filter (setq gmaps-current-filter nil))
  (goto-char (point-max))
  (let-alist
      (g-json-from-url
       (format "%s&%s&%s%s"
               (gmaps-places-url-base "nearbysearch" gmaps-places-key)
               (format "location=%s,%s"
                       (g-json-get 'lat (gmaps--location-lat-lng gmaps-current-location))
                       (g-json-get 'lng (gmaps--location-lat-lng gmaps-current-location)))
               (format "radius=%s" gmaps-current-radius)
               (if gmaps-current-filter
                   (gmaps-places-filter-as-params gmaps-current-filter)
                 "")))
    (let ((start nil)
          (inhibit-read-only t))
      (cond
       ((string= "OK" .status)
        (goto-char (point-max))
        (setq start (point))
        (insert
         (format "Places within %sm of  %s\n"
                 gmaps-current-radius
                 (gmaps--location-address gmaps-current-location)))
        (when gmaps-current-filter
          (insert (format "Filter: %s\n"
                          (gmaps-places-filter-as-string gmaps-current-filter))))
        (gmaps-display-places .results)
        (goto-char start))
       ((string= "ZERO_RESULTS"  .status)
        (insert
         (format "No places within %sm  matching %s.\n"
                 gmaps-current-radius
                 (gmaps-places-filter-as-string gmaps-current-filter))))
       (t (error "Status %s from Maps" .status))))))

(defun gmaps-places-search (query &optional clear-filter)
  "Perform a places search.
Use this only if you dont know the locality  of the place you're looking for.
Optional  prefix arg clears any active filters."
  (interactive
   (list
    (read-from-minibuffer "Search For: ")
    current-prefix-arg))
  (cl-declare (special gmaps-current-filter gmaps-places-key))
  (and clear-filter (setq gmaps-current-filter nil))
  (goto-char (point-max))
  (let-alist
      (g-json-from-url
       (format "%s&query=%s%s"
               (gmaps-places-url-base "textsearch" gmaps-places-key)
               (url-hexify-string query)
               (if gmaps-current-filter
                   (gmaps-places-filter-as-params gmaps-current-filter)
                 "")))
    (let ((start nil)
          (inhibit-read-only t))
      (cond
       ((string= "OK" .status)
        (goto-char (point-max))
        (setq start (point))
        (insert (format "Places  matching %s\n" query))
        (when gmaps-current-filter
          (insert
           (format "Filter: %s\n"
                   (gmaps-places-filter-as-string gmaps-current-filter))))
        (gmaps-display-places .results)
        (goto-char start))
       ((string= "ZERO_RESULTS"  .status)
        (insert (format "No places matching %s" query)))
       (t (error "Status %s from Maps" .status))))))

(defun gmaps-display-places (places)
  "Display places in Maps interaction buffer."
  (let ((length (length places))
        (inhibit-read-only t))
    (cond
     ((= 1 length) (gmaps-display-place (aref places 0)))
     (t
      (cl-loop
       for place across places do
       (gmaps-display-place place))))))

(defun gmaps-colonize-timestring (timestring)
  "Insert `:' to turn 0800 into 08:00. "
  (format "%s:%s"
          (substring timestring 0 2)
          (substring timestring 2)))

(defun gmaps-hours-for-day (hours &optional day)
  "Display hours.Day defaults to today."
  (or day (setq day  (read-number "Week Day: 0 for Sunday: ")))
  (let ((open nil)
        (close nil)
        (weekday
         (aref
          '["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"]
          day)))
    (setq open
          (cl-find-if
           #'(lambda (h)
               (= day (g-json-lookup "open.day" h))) hours)
          close
          (cl-find-if
           #'(lambda (h) (= day (g-json-lookup "close.day" h))) hours))
    (format "%s Open: %s, Close: %s"
            weekday
            (gmaps-colonize-timestring (g-json-lookup "open.time" open))
            (gmaps-colonize-timestring (g-json-lookup "close.time" close)))))

(defun gmaps-display-places-hours (hours)
  "Display opening/closing hours."
  (let* ((inhibit-read-only t)
         (day-hours  (gmaps-hours-for-day hours (read-number "Week Day (0 for Sunday): ")))
         (start (next-single-property-change (point) 'open-hours))
         (end (next-single-property-change start 'open-hours)))
    (delete-region start end)
    (goto-char start)
    (insert (format "%s\t" day-hours))
    (put-text-property start (point)
                       'open-hours t)))

(defun gmaps-display-place-details (details)
  "Insert place details."
  (goto-char (line-end-position))
  (insert "\n")
  (let-alist details
    (let ((start (point))
          (hours .opening_hours.periods)
          (open .opening_hours.open_now))
      (when hours
        (let ((today (gmaps-hours-for-day hours (read (format-time-string "%w"))))
              (here nil))
          (insert-text-button
           "[Hours]\t"
           'hours hours
           'action
           #'(lambda (b)
               (gmaps-display-places-hours  (button-get b 'hours))))
          (setq here (point))
          (insert (format "%s\t" today))
          (put-text-property  here (point) 'open-hours t)))
      (when .website
        (insert-text-button
         "[WebSite]\t"
         'url-link .website
         'action
         #'(lambda (b)
             (browse-url (button-get b 'url-link)))))
      (when .url
        (insert-text-button
         "[Places URL]\n"
         'url-link .url
         'action #'(lambda (b) (browse-url (button-get b 'url-link)))))
      (when (or .formatted_address .international_phone_number)
        (insert
         (format "%s\t%s\n" .formatted_address  .international_phone_number)))
      (insert
       (format "Open: %s\tRating: %s\tPrice: %s\n"
               (if open "Yes" "No")
               (or .ratings "N/A")
               (or .price_level "N/A")))
      (indent-rigidly start  (point) 4)
      (put-text-property start (point)
                         'place-details details)
      (goto-char start))))

(defun gmaps-display-place (place)
  "Display place in Maps buffer."
  (let-alist place
    (let ((inhibit-read-only t)
          (start (point)))
      (insert
       (format "%s\t%s\t%s\n"
               .name .types .vicinity))
      (put-text-property start (1- (point)) 'maps-data place))))

(defun gmaps-place-details ()
  "Display details for place at point.
Insert reviews if already displaying details."
  (interactive)
  (cl-declare (special gmaps-places-key))
  (unless (eq major-mode 'gmaps-mode) (error "Not in a Google Maps buffer."))
  (unless
      (or (get-text-property  (point) 'maps-data)
          (get-text-property (point) 'place-details))
    (error "No maps data at point."))
  (cond
   ((get-text-property (point) 'place-details)
    (gmaps-place-reviews))
   (t
    (let* ((inhibit-read-only t)
           (place-ref
            (g-json-get 'reference (get-text-property (point)'maps-data))))
      (unless place-ref (error "No place here"))
      (let-alist
          (g-json-from-url
           (format
            "%s&%s"
            (gmaps-places-url-base "details" gmaps-places-key)
            (format "reference=%s" place-ref)))
        (cond
         ((string= "OK" .status)
          (put-text-property (line-beginning-position) (line-end-position)
                             'place-details t)
          (gmaps-display-place-details .result))
         (t (error "Status %s from Maps" .result))))))))

(defun gmaps-place-reviews ()
  "Display reviews for place at point.
Place details need to have been expanded first."
  (interactive)
  (let ((inhibit-read-only t)
        (start (point))
        (details (get-text-property (point) 'place-details))
        (reviews nil))
    (unless  details (error  "No place details here."))
    (setq reviews (g-json-get 'reviews details))
    (unless reviews (error "No reviews for this place."))
    (goto-char (next-single-property-change (point)  'place-details))
    (setq start (point))
    (insert
     (with-temp-buffer
       (insert "<ol>")
       (cl-loop
        for r across reviews
        do
        (insert
         (format "<li><a href='%s'>%s %s</a> [%s] %s</li>\n"
                 (g-json-get 'author_url r)
                 (g-json-get 'author_name r)
                 (format-time-string "%m/%d/%y"
                                     (seconds-to-time (g-json-get 'time r)))
                 (g-json-get 'rating r)
                 (g-json-get 'text r))))
       (insert "</ol>")
       (g-html-string  (buffer-string))))
    (set-mark (point))
    (emacspeak-auditory-icon 'task-done)
    (goto-char start)
    (message (format "Inserted %d reviews"  (length reviews)))))

;;}}}
(provide 'gmaps)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
