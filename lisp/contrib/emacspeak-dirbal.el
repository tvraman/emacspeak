;http://dirble.com/inlogged/apikeys  -*- lexical-binding: t; -*-
(defcustom  emacspeak-dirbal-api-key 
""
"API key."
:type 'string)

(defvar emacspeak-dirbal-api-base
  "http://api.dirble.com/v1"
  "Base URL for REST  API to Dirbal radio streams.")
(defsubst emacspeak-dirbal-search-uri (query)
  "Return search url for query."
  (declare (special emacspeak-dirbal-api-base emacspeak-dirbal-api-key))
  (format "%s/search/apikey/%s/search/%s"
          emacspeak-dirbal-api-base emacspeak-dirbal-api-key
          (emacspeak-url-encode query)))

(defsubst emacspeak-dirbal-categories-uri ()
  "Return list of categories."
  (declare (special emacspeak-dirbal-api-base emacspeak-dirbal-api-key))
  (format "%s/categories/apikey/%s"
          emacspeak-dirbal-api-base emacspeak-dirbal-api-key))


(defsubst emacspeak-dirbal-primary-categories-uri ()
  "Return list of categories."
  (declare (special emacspeak-dirbal-api-base emacspeak-dirbal-api-key))
  (format "%s/primaryCategories/apikey/%s"
          emacspeak-dirbal-api-base emacspeak-dirbal-api-key))


(defsubst emacspeak-dirbal-child-categories-uri ()
  "Return list of categories."
  (declare (special emacspeak-dirbal-api-base emacspeak-dirbal-api-key))
  (format "%s/childCategories/apikey/%s"
          emacspeak-dirbal-api-base emacspeak-dirbal-api-key))
(defsubst emacspeak-dirbal-country-uri (id)
  "Return search url for query."
  (declare (special emacspeak-dirbal-api-base emacspeak-dirbal-api-key))
  (format "%s/country/apikey/%s/country/%s"
          emacspeak-dirbal-api-base emacspeak-dirbal-api-key
          id))
