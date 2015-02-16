;;; emacspeak-url-template.el --- Create library of URI templates
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:   Implement library of URI templates
;;; Keywords: Emacspeak, Audio Desktop
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2008-08-14 11:23:31 -0700 (Thu, 14 Aug 2008) $ |
;;;  $Revision: 4626 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1995 -- 2011, T. V. Raman<raman@cs.cornell.edu>
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

;;{{{  Introduction:

;;; Commentary:

;;; It is often useful to have ``parametrized hot list entries''
;;; i.e., hotlist entries  that are ``templates'' for the
;;; actual URL.
;;; The user provides values for the parametrized portions
;;; of the URL e.g. the date.
;;; See @xref{URL Templates}, for details on the URL templates
;;; that are presently defined.

;;; Code:

;;}}}
;;{{{ required modules

(require 'emacspeak-preamble)
(require 'emacspeak-webutils)
(require 'gweb)
(require 'emacspeak-we)
(require 'emacspeak-xslt)
(eval-when-compile
  (require 'calendar))
;;}}}
;;{{{  structures

(defstruct (emacspeak-url-template
            (:constructor emacspeak-url-template-constructor))
  name                                ;Human-readable name
  template                            ;template URL string
  generators                          ; list of param generator
  post-action                         ;action to perform after opening
  documentation                       ;resource  documentation
  fetcher                             ; custom fetcher
  dont-url-encode)

;;}}}
;;{{{ Helpers

(defun emacspeak-url-template-url (ut)
  "Instantiate URL identified by URL template."
  (let ((url 
         (apply 'format
                ( emacspeak-url-template-template ut)
                (mapcar
                 #'(lambda (g)
                     (let ((input nil))
                       (setq input
                             (cond
                              ((stringp g)
                               (if (emacspeak-url-template-dont-url-encode ut)
                                   (read-from-minibuffer g)
                                 (emacspeak-url-encode (read-from-minibuffer g))))
                              (t (funcall g))))
                       input))
                 (emacspeak-url-template-generators ut)))))
    url))

;;}}}
;;{{{  persistent store

(defvar emacspeak-url-template-table (make-hash-table :test 'equal)
  "Stores URL templates. ")
(defun emacspeak-url-template-set (key ut)
  "Add  specified template to key. "
  (declare (special emacspeak-url-template-table))
  (setf (gethash key emacspeak-url-template-table ) ut))
;;;###autoload
(defun emacspeak-url-template-get (key)
  "Lookup key and return corresponding template. "
  (declare (special emacspeak-url-template-table))
  (gethash key emacspeak-url-template-table))

;;}}}
;;{{{  define resources

;;;###autoload
(defun emacspeak-url-template-define (name template
                                           &optional generators post-action
                                           documentation fetcher
                                           dont-url-encode)
  "Define a URL template.

name            Name used to identify template
template        Template URI with `%s' for slots
generators      List of prompters.
                Generators are strings or functions.
                String values specify prompts.
                Function values are called to obtain values.
post-action     Function called to apply post actions.
                Possible actions include speaking the result.
fetcher         Unless specified, browse-url retrieves URL.
                If specified, fetcher is a function of one arg
                that is called with the URI to retrieve.
documentation   Documents this template resource.
dont-url-encode if true then url arguments are not url-encoded "
  (declare (special emacspeak-url-template-table))
  (emacspeak-url-template-set
   name
   (emacspeak-url-template-constructor
    :name name
    :template template
    :generators generators
    :post-action  post-action
    :documentation documentation
    :fetcher fetcher
    :dont-url-encode dont-url-encode)))

;;;###autoload
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
  (let ((print-level nil)
        (print-length nil)
        (buffer (find-file-noselect
                 (expand-file-name file
                                   emacspeak-resource-directory))))
    (save-current-buffer
      (set-buffer buffer)
      (setq buffer-undo-list t)
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
;;; template resources
;;{{{  fedex, UPS
(emacspeak-url-template-define
 "fedex packages"
 "http://www.fedex.com/cgi-bin/tracking?link=6&pv=ja&action=track&ftc_3=null&template_type=ftc&language=english&last_action=track&ascend_header=1&cntry_code=us&initial=x&mps=y&ascend_header=1&cntry_code=us&initial=x&tracknumber_list=%s"
 (list "Tracking Number: ")
 nil
 "Display package tracking information from Fedex.")

(emacspeak-url-template-define
 "UPS Packages"
 "http://wwwapps.ups.com/WebTracking/processInputRequest?HTMLVersion=5.0&sort_by=status&tracknums_displayed=5&TypeOfInquiryNumber=T&loc=en_US&InquiryNumber1=%s&InquiryNumber2=&InquiryNumber3=&InquiryNumber4=&InquiryNumber5=&track.x=0&track.y=0&AgreeToTermsAndConditions=yes"
 (list "Tracking Number: ")
 nil
 "Display package tracking information from UPS."
 #'(lambda (url)
     (emacspeak-we-extract-by-class "dataTable" url 'speak)))

;;}}}
;;{{{ amazon

(emacspeak-url-template-define
 "Amazon Product Details By ASIN"
 "http://amazon.com/o/dt/upda-1.0-i/tg/aa/upda/item/-/%s"
 (list "ASIN Or ISBN: ")
 nil
 "Retrieve product details from Amazon by either ISBN or ASIN.")

;;}}}
;;{{{  old time radio

(emacspeak-url-template-define
 "Old Time Radio"
 "http://www.oldtimeradioprograms.com"
 nil
 nil
 "This months Old Time Radio Programing"
 #'(lambda (url)
     (emacspeak-we-extract-nested-table-list
      (list 2 3 )
      url)))

;;}}}
;;{{{ BBC iPlayer 
;;; convertor is here:
;;; http://www.iplayerconverter.co.uk/convert.aspx
;;;Conversion: http://www.iplayerconverter.co.uk/convert.aspx?pid=%s
;;; This used to work, but now gives a 403:
;;; "http://www.iplayerconverter.co.uk/pid/%s/r/stream.aspx"
;;; So now we need to open a Web page and click a link (sad)

(defvar emacspeak-url-template-iplayer-convertor
  "http://www.iplayerconverter.co.uk/convert.aspx?pid=%s"
  "Template for generating persistent realplayer URL for iplayer content.")

(defun emacspeak-url-template-iplayer-player (cid)
  "Take a cid particle, and opens player page."
  (declare (special emacspeak-url-template-iplayer-convertor))
  (add-hook
   'emacspeak-web-post-process-hook
   #'(lambda nil
       (cond
        ((search-forward "mms:" nil t)
         (emacspeak-webutils-play-media-at-point)
         (bury-buffer))
        (t (message "Could not find media link."))))
   'at-end)
  (browse-url
   (format emacspeak-url-template-iplayer-convertor (substring  cid 4))))

;;; See http://www.bbc.co.uk/programmes/developers
;;; for how to obtain xml schedules 
(emacspeak-url-template-define
 "BBC  iPlayer"
 "http://www.bbc.co.uk/%s/programmes/schedules/%s%s.xml"
 (list
  "Station: "
  #'(lambda ()
      (let ((outlet (read-from-minibuffer "Outlet: ")))
        (cond
         ((= 0 (length outlet)) outlet)
         (t (concat outlet "/")))))
  'emacspeak-speak-read-date-year/month/date)
 #'(lambda ()
     (declare (special emacspeak-we-url-executor))
      (setq emacspeak-we-url-executor
           'emacspeak-url-template-iplayer-player))
 "BBC iPlayer"
 #'(lambda (url)
     (emacspeak-webutils-with-xsl-environment
      (expand-file-name "bbc-iplayer.xsl" emacspeak-xslt-directory)
      nil emacspeak-xslt-options
      (browse-url url))))

(emacspeak-url-template-define
 "BBC Genre Using IPlayer"
 "http://www.bbc.co.uk/radio/programmes/genres/%s/schedules.xml"
 (list "Genre/Genre/Genre: ")
 #'(lambda ()
     (declare (special emacspeak-we-url-executor))
     (setq emacspeak-we-url-executor 'emacspeak-url-template-iplayer-player))
 "BBC iPlayer Genre"
 #'(lambda (url)
     
     (emacspeak-webutils-with-xsl-environment
      (expand-file-name "bbc-iplayer.xsl" emacspeak-xslt-directory)
      nil emacspeak-xslt-options
      (browse-url url)))
 'dont-url-encode)

;;}}}
;;{{{ bbc

(emacspeak-url-template-define
 "BBC Program Guide"
 "http://downloads.bbc.co.uk/podcasts/ppg.xml"
 nil nil
 "Display interactive BBC Program Guide."
 #'(lambda (url)
     (emacspeak-xslt-view-xml
      (expand-file-name "bbc-ppg.xsl" emacspeak-xslt-directory) url )))
(emacspeak-url-template-define
 "BBC Podcast Directory"
 "http://www.bbc.co.uk/podcasts.opml"
 nil nil
 "BBC PodCast Directory"
 #'emacspeak-feeds-opml-display)

;;}}}
;;{{{ html5irc 

(emacspeak-url-template-define
 "html5IRC"
 "http://krijnhoetmer.nl/irc-logs/whatwg/%s"
 (list 'emacspeak-speak-date-YearMonthDate)
 nil
 "Show HTML5 IRC log.")

;;}}}
;;{{{ market summary from google finance

(emacspeak-url-template-define
 "Market summary from Google"
 "http://finance.google.com/finance"
 nil
 nil
 "Display financial market summary."
 #'(lambda (url)
     (let ((w3-auto-image-alt " "))
       (emacspeak-we-extract-by-class
        "id-summary-chart" url 'speak))))

;;}}}
;;{{{ google CSE :

(emacspeak-url-template-define
 "Official GoogleBlog Search"
 "http://www.google.com/cse?q=%s&loading=1&cref=%s"
 (list
  "GoogleBlog Search: "
  #'(lambda nil
      (emacspeak-url-template-make-cse
       "http://www.google.com/reader/public/subscriptions/user/10949413115399023739/label/officialgoogleblogs-all")))
 nil
 "Search within all official Google blogs."
 #'(lambda (url)
     (emacspeak-we-extract-by-class "g" url 'speak)))
(defsubst emacspeak-url-template-make-cse (meta-url)
  "Builds up a CSE url for specified meta-url."
  (format
   "http://www.google.com/cse/tools/makecse?url=%s"
   meta-url))

(emacspeak-url-template-define
 "On The Fly CSE"
 "http://www.google.com/cse?q=%s&loading=1&cref=%s&nojs=1"
 (list
  "Search This CSE: "
  #'(lambda nil
      (emacspeak-url-template-make-cse
       (read-from-minibuffer
        "Feed URL to build CSE for: "))))
 nil
 "Build a CSE on the fly and  use it to search."
 #'(lambda (url)
     (emacspeak-we-extract-by-class "g" url 'speak)))

;;}}}
;;{{{ utils:

(defun emacspeak-url-template-setup-content-filter ()
  "Set up content filter in displayed page."
  (declare (special emacspeak-we-xpath-filter emacspeak-we-paragraphs-xpath-filter))
  (setq emacspeak-we-xpath-filter
        emacspeak-we-paragraphs-xpath-filter))

;;}}}
;;{{{ webmaster tools
(emacspeak-url-template-define
 "Google Webmaster Page Analysis"
 "https://www.google.com/webmasters/tools/pageanalysis?siteUrl=%s"
 (list "URL To Analyze: ")
 nil
 "Page Analysis From Google Webmaster tools.")

;;}}}
;;{{{ Anonimize google search
(emacspeak-url-template-define
 "Sign in to Google"
 "https://accounts.google.com/ServiceLogin?hl=en&continue=https://www.google.com/"
 nil
 nil
 "Login to Google.")

(emacspeak-url-template-define
 "Anonymize Google Search"
 "http://www.google.com/accounts/Logout"
 nil
 nil
 "Logout from Google to do an anonymous search.")

;;}}}
;;{{{ Calendar Mobile:

(defcustom emacspeak-url-template-google-calendar-uri
  "http://www.google.com/calendar/m?output=xhtml&pli=1"
  "URI for accessing mobile version of Google Calendar.
Google Apps users should set this to
http://www.google.com/calendar/a/<my-corp>/m?output=xhtml"
  :type 'string
  :group 'emacspeak-url-template)

(emacspeak-url-template-define
 "GCalendar Mobile"
 emacspeak-url-template-google-calendar-uri
 nil
 'emacspeak-speak-buffer
 "Google Calendar XHTML version.")

;;}}}
;;{{{  google patent search:

(emacspeak-url-template-define
 "Patent Search From Google"
 "http://www.google.com/patents?ie=ISO-8859-1&q=%s"
 (list "Google For Patents: ")
 #'(lambda nil
     (search-forward " Patent Search" nil t)
     (beginning-of-line)
     (emacspeak-speak-rest-of-buffer))
 "Perform patent search via Google")

;;}}}
;;{{{ google API  search

(emacspeak-url-template-define
 "Google API Search"
 "http://www.google.com/cse?cx=001456098540849067467%%3A6whlsytkdqg&cof=FORID%%3A0&q=%s&sa=Search"
 (list "Google API Search: ")
 nil
 "Search Google APIDocs")

;;}}}
;;{{{ YouTube:
(emacspeak-url-template-define
 "YouTube Results"
 "http://gdata.youtube.com/feeds/api/videos?vq=%s"
 (list
  #'(lambda ()
      (gweb-google-autocomplete-with-corpus "youtube")))
 #'(lambda ()
     (declare (special emacspeak-we-url-executor))
     (setq emacspeak-we-url-executor
           'emacspeak-m-player-youtube-player))
 "YouTube Search Via Feeds"
 #'emacspeak-feeds-atom-display)

(emacspeak-url-template-define
 "Recent YouTube Results"
 "http://gdata.youtube.com/feeds/api/videos?vq=%s&orderby=updated"
 (list "YouTube:")
 #'(lambda ()
     (declare (special emacspeak-we-url-executor))
     (setq emacspeak-we-url-executor
           'emacspeak-m-player-youtube-player))
 "YouTube Search Via Feeds"
 #'emacspeak-feeds-atom-display)

;;}}}
;;{{{ google finance
;;{{{ seeking alpha stock search

(emacspeak-url-template-define
 "Seeking Alpha Stock Search"
 "http://seekingalpha.com/search/?cx=001514237567335583750%%3Acdhc2yeo2ko&cof=FORID%%3A11%%3BNB%%3A1&q=%s"
 (list "Company:")
 nil
 "Seeking Alpha search.")

;;}}}
;;; pull google finance search results via the transcoder

(emacspeak-url-template-define
 "Mobile Finance Google Search"
 "http://finance.google.com/finance?q=%s"
 (list "Finance Search: ")
 #'(lambda nil
     (call-interactively 'emacspeak-imenu-goto-next-index-position))
 "Display content from Google Finance."
 #'(lambda (url)
     (browse-url
      (format emacspeak-webutils-google-transcoder-url
              (emacspeak-url-encode
               url)))))

(emacspeak-url-template-define
 "Finance Google Search"
 "http://finance.google.com/finance?q=%s"
 (list "Finance Search: ")
 nil
 "Display content from Google Finance."
 )

(defun emacspeak-finance-google-up-or-down (value)
  "Return up/down by value."
  (let ((minus-p (string-match "-" value)))
    (cond
     (minus-p
      (format " down  %s"
              (substring value (1+ minus-p ))))
     (t (format " up  %s " value)))))

(defvar emacspeak-google-finance-row-filter
  '(0 (emacspeak-finance-google-up-or-down 3)" to " 2  " for  a market cap of " 4 
      "Traded today between " 8 " and " 7 " on a  volume of " 5)
  "Template used as a row formatter for Finance Portfolios.")

(emacspeak-url-template-define
 "Finance Google Portfolio"
 "http://finance.google.com/finance/portfolio?action=view&pid=1&pview=sview&output=csv"
 nil nil
 "Download and display portfolio from Google Finance."
 #'(lambda (url)
     (declare (special emacspeak-google-finance-row-filter
                       emacspeak-table-speak-row-filter))
     (let ((buffer (url-retrieve-synchronously url)))
       (save-current-buffer
         (set-buffer buffer)
         (goto-char (point-min))
         (search-forward "\n\n")
         (delete-region (point-min) (point))
         (emacspeak-table-view-csv-buffer buffer)
         (kill-buffer buffer)
         (when (get-buffer "Portfolio From Google Finance")
           (kill-buffer "Portfolio From Google Finance"))
         (rename-buffer "Portfolio From Google Finance")
         (setq tab-width 12
               emacspeak-table-speak-row-filter
               emacspeak-google-finance-row-filter)
         (emacspeak-table-next-row)))))

(emacspeak-url-template-define
 "Finance Google news"
 "http://finance.google.com/"
 nil
 nil
 "Display content from Google Finance."
 #'(lambda (url)
     (emacspeak-we-extract-by-class
      "news" url 'speak)))

;;}}}
;;{{{ google scholar

(emacspeak-url-template-define
 "Google Scholar"
 "http://scholar.google.com/scholar?ie=UTF-8&oe=UTF-8&hl=en&btnG=Search&num=25&q=%s"
 (list "Google Scholar Search: ")
 nil
 "Google Scholar Search"
 #'(lambda (url)
     (emacspeak-we-extract-by-class "gs_r" url 'speak)))

;;}}}
;;{{{ google images

(emacspeak-url-template-define
 "Google Image Search"
 "http://images.google.com/images?hl=en&source=hp&q=%s&btnG=Search+Images&gbv=1"
 (list "Google Image Search: ")
 #'(lambda ()
     (search-forward "results" nil t)
     (emacspeak-speak-line))
 "Google Image Search"
 #'(lambda (url)
     (emacspeak-webutils-without-xsl
      (browse-url url))))

;;}}}
;;{{{ google translation service

(emacspeak-url-template-define
 "Multilingual dictionary via Google."
 "http://translate.google.com/translate_dict?q=%s&sa=N&hl=en&langpair=%s"
 (list
  "Word: "
  "Translate from|To:")
 nil
 "Translate word using Google.
Source and target languages
are specified as two-letter language codes, e.g. en|de translates
from English to German")

(emacspeak-url-template-define
 "Translation Via Google"
 "http://translate.google.com/translate_c?hl=en&langpair=%s&u=%s"
 (list
  "Translate from|To:"
  "URI: ")
 nil
 "Translate a Web page using google. Source and target languages
are specified as two-letter language codes, e.g. en|de translates
from English to German.")

;;}}}
;;{{{ dictionary.com:
(emacspeak-url-template-define
 "Dictionary Lookup"
 "http://dictionary.reference.com/search?q=%s"
 (list "Dictionary Lookup: ")
 #'(lambda ()
     (search-forward "entries found for " nil t)
     (emacspeak-speak-line))
 "Dictionary Lookup"
 #'(lambda (url)
     (emacspeak-webutils-without-xsl
      (browse-url url))))

;;}}}
;;{{{ NY Times
(emacspeak-url-template-define
 "NY Times RSS Feeds"
 "http://www.nytimes.com/services/xml/rss/nyt/index.opml"
 nil
 nil
 "Display browsable list of NY Times RSS Feeds."
 #'(lambda (url)
     (let ((buffer
            (emacspeak-xslt-xml-url
             (expand-file-name "opml.xsl"
                               emacspeak-xslt-directory)
             url )))
       (save-current-buffer
         (set-buffer buffer)
         (browse-url-of-buffer)))))

(emacspeak-url-template-define
 "NY Times Mobile"
 "http://mobile.nytimes.com"
 nil
 #'(lambda ()
     (emacspeak-url-template-setup-content-filter)
     (emacspeak-speak-buffer))
 "NYTimes Mobile Site"
 #'(lambda (url)
     (emacspeak-we-extract-by-role "main" url))
 )

;;}}}
;;{{{ google OverviewOfNews

(emacspeak-url-template-define
 "Google Print"
 "http://print.google.com/print?oi=print&q=%s"
 (list "Google Print Search:")
 #'(lambda nil
     (search-forward "Print  Books" nil t)
     (emacspeak-speak-rest-of-buffer))
 "Google Print Search")

(emacspeak-url-template-define
 "Google NewsPaper"
 "http://news.google.com/news"
 nil
 nil
 "Retrieve and speak Google News Overview."
 #'(lambda (url)
     (emacspeak-we-extract-by-id-list
      '("s_WEATHER_GADGET" "s_SPORTS_GADGET"
        "s_MOST_POPULAR" "s_INTERESTING"
        "s_EDITORS_PICK" "s_BREAKING_NEWS_BOX")
      url
      'speak)))      

(emacspeak-url-template-define
 "Google News Search"
 "http://news.google.com/news?hl=en&ned=tus&q=%s&btnG=Google+Search&output=atom"
 (list "Google News: ")
 'emacspeak-url-template-setup-content-filter
 "Search Google news."
 #'emacspeak-feeds-atom-display)

(emacspeak-url-template-define
 "Google Recent News Search"
 "http://news.google.com/news?hl=en&ned=tus&q=%s&scoring=d&output=atom"
 (list "Search news for: ")
 nil
 "Search Google news."
 #'emacspeak-feeds-atom-display)

(emacspeak-url-template-define
 "Google Mobile Search"
 "http://www.google.com/xhtml?q=%s&site=search&mrestrict=xhtml&num=25"
 (list "Search For: ")
 #'(lambda ()
     (re-search-forward "^Results" nil t)
     (emacspeak-speak-rest-of-buffer))
 "Google Mobile Search")

(defvar emacspeak-url-template-google-transcoder-url
  "http://www.google.com/gwt/n?_gwt_noimg=1&output=xhtml&u=%s"
  "URL for  obtaining mobile transcoder page views.")

(emacspeak-url-template-define
 "Google Transcoder"
 emacspeak-url-template-google-transcoder-url
 (list
  #'(lambda ()
      (read-from-minibuffer "URL: "
                            (or (browse-url-url-at-point)
                                "http://"))))
 'emacspeak-speak-buffer
 "Transcode site via Google.")

(emacspeak-url-template-define
 "Google topical  News"
 "http://news.google.com/news?ned=us&topic=%s&output=atom"
 (list "Topic Code: ")
 nil
 "Display specified news feed."
 #'emacspeak-feeds-atom-display)

;;}}}
;;{{{ Google Archive Search

(emacspeak-url-template-define
 "Archive News Search"
 "http://news.google.com/archivesearch?hl=en&sa=N&q=%s"
 (list "Archive News Search: ")
 #'(lambda nil
     (search-forward "Results" nil t)
     (emacspeak-speak-line))
 "Search Google Archive News."
 #'(lambda (url)
     (emacspeak-webutils-without-xsl
      (browse-url url))))

(emacspeak-url-template-define
 "IToRSS"
 "http://feedflipper.net/convert.php?feed=%s"
 (list "ITunes URL: ")
 nil
 "Pull RSS  feed corresponding to an ITunes Podcast."
 #'emacspeak-feeds-rss-display)

;;}}}
;;{{{  cnet news

(emacspeak-url-template-define
 "Tech News From CNet"
 "http://feeds.feedburner.com/cnet/tcoc"
 nil
 'emacspeak-url-template-setup-content-filter
 "Display tech news from CNET"
 #'emacspeak-feeds-rss-display)

(emacspeak-url-template-define
 "PodCast CNet"
 "http://podcast-files.cnet.com/podcast/cnet_podcast_%s.mp3"
 (list
  #'(lambda nil
      (read-from-minibuffer
       "Date: "
       (format-time-string "%m%d%y"))))
 nil
 "Play Podcast from CNET"
 #'(lambda (url)
     (funcall emacspeak-media-player url)))

;;}}}
;;{{{ yahoo daily news
(emacspeak-url-template-define
 "Yahoo RSS Feeds"
 "http://news.yahoo.com/rss"
 nil
 #'(lambda ()
     (emacspeak-pronounce-add-buffer-local-dictionary-entry
      "http://rss.news.yahoo.com/rss/" ""))
 "List Yahoo RSS Feeds."
 #'(lambda (url)
     (emacspeak-we-xslt-filter
      "//a[not(contains(@href,\"url\"))and  contains(@href, \"rss\") ]"
      url 'speak)))

(defun emacspeak-url-template-yahoo-news-processor (url)
  "Process and speak Yahoo news."
  (declare (special emacspeak-web-post-process-hook))
  (add-hook 'emacspeak-web-post-process-hook
            #'(lambda nil
                (declare (special  emacspeak-we-url-rewrite-rule
                                   emacspeak-we-class-filter))
                (setq emacspeak-we-class-filter "article"
                      emacspeak-we-url-rewrite-rule
                      '("$" "&printer=1"))
                (emacspeak-speak-buffer)))
  (emacspeak-we-xslt-filter
   "//*[@id=\"ynmain\"]"
   url))

(emacspeak-url-template-define
 "Yahoo DailyNews"
 "http://dailynews.yahoo.com/"
 nil
 nil
 "Retrieve and speak DailyNewspage from  Yahoo Daily News."
 'emacspeak-url-template-yahoo-news-processor)

(emacspeak-url-template-define
 "Yahoo Politics"
 "http://dailynews.yahoo.com/news?tmpl=index2&cid=703"
 nil
 nil
 "Retrieve and speak Politics section from Yahoo Daily News."
 'emacspeak-url-template-yahoo-news-processor)

(emacspeak-url-template-define
 "Yahoo Entertainment"
 "http://dailynews.yahoo.com/news?tmpl=index2&cid=762"
 nil
 nil
 "Retrieve and speak Entertainment section from Yahoo Daily News."
 'emacspeak-url-template-yahoo-news-processor)

(emacspeak-url-template-define
 "Yahoo Sports"
 "http://dailynews.yahoo.com/news?tmpl=index2&cid=755"
 nil
 nil
 "Entertainment news from Yahoo."
 'emacspeak-url-template-yahoo-news-processor)

(emacspeak-url-template-define
 "Yahoo Business News"
 "http://story.news.yahoo.com/news?tmpl=index&cid=749"
 nil
 nil
 "Retrieve and speak business  section from Yahoo Daily News."
 'emacspeak-url-template-yahoo-news-processor)

(emacspeak-url-template-define
 "Yahoo Science"
 "http://dailynews.yahoo.com/news?tmpl=index2&cid=753"
 nil
 nil
 "Retrieve and speak Science section from Yahoo Daily News."
 'emacspeak-url-template-yahoo-news-processor)

(emacspeak-url-template-define
 "Yahoo SF Local"
 "http://dailynews.yahoo.com/news?tmpl=index2&cid=390"
 nil
 nil
 "Retrieve and speak Local section from Yahoo Daily News."
 'emacspeak-url-template-yahoo-news-processor)

(emacspeak-url-template-define
 "Yahoo Content By Content ID"
 "http://dailynews.yahoo.com/news?tmpl=index2&cid=%s"
 (list "Content ID: ")
 nil
 "Retrieve and speak news section from Yahoo Daily News."
 'emacspeak-url-template-yahoo-news-processor)

(emacspeak-url-template-define
 "Yahoo Top Stories"
 "http://dailynews.yahoo.com/news?tmpl=index2&cid=716"
 nil
 nil
 "Retrieve and speak Top Stories  section from Yahoo Daily News."
 'emacspeak-url-template-yahoo-news-processor)

(emacspeak-url-template-define
 "Yahoo Health"
 "http://dailynews.yahoo.com/news?tmpl=index2&cid=751"
 nil
 nil
 "Retrieve and speak Health section from Yahoo Daily News."
 'emacspeak-url-template-yahoo-news-processor)

(emacspeak-url-template-define
 "Yahoo Oddly"
 "http://dailynews.yahoo.com/news?tmpl=index2&cid=757"
 nil
 nil
 "Retrieve and speak Oddity section from Yahoo Daily News."
 'emacspeak-url-template-yahoo-news-processor)

(emacspeak-url-template-define
 "Yahoo Technology  News"
 "http://dailynews.yahoo.com/news?tmpl=index2&cid=738"
 nil
 nil
 "Yahoo Technology News."
 'emacspeak-url-template-yahoo-news-processor)

(emacspeak-url-template-define
 "Yahoo Lifestyle"
 "http://dailynews.yahoo.com/news?tmpl=index2&cid=811"
 nil
 nil
 "Yahoo Lifestyle News."
 'emacspeak-url-template-yahoo-news-processor)

(emacspeak-url-template-define
 "Yahoo World News"
 "http://dailynews.yahoo.com/news?tmpl=index2&cid=959"
 nil
 nil
 "Yahoo World News."
 'emacspeak-url-template-yahoo-news-processor)

;;}}}
;;{{{ w3c

(emacspeak-url-template-define
 "w3c IRC Logs"
 "http://www.w3.org/%s-%s-irc "
 (list
  #'(lambda nil
      (emacspeak-speak-collect-date "Date: "
                                    "%Y/%m/%d"))
  "Channel Name: ")
 #'(lambda ()
     (let ((inhibit-read-only t))
       (flush-lines "has joined #" (point-min) (point-max))
       (flush-lines "has left #" (point-min) (point-max))))
 "Use this to pull up the
archived  logs from the W3C IRC. You need to know the exact
name of the channel.")

(emacspeak-url-template-define
 "w3c Lists"
 "http://lists.w3.org/Archives/Member/%s/%s/"
 (list
  'emacspeak-url-template-get-w3c-group
  'emacspeak-url-template-get-w3c-year/month)
 nil
 "Use this to pull up the
archived  mail from the W3C list. You need to know the exact
name of the list.")

(defun emacspeak-url-template-get-w3c-group ()
  "Get name of W3C group "
  (read-from-minibuffer "W3C group: "
                        "w3c-"))

(defun emacspeak-url-template-get-w3c-year/month ()
  "Get year/month"
  (emacspeak-speak-collect-date "Date range: "
                                "%Y%h"))

;;}}}
;;{{{ cnn

(emacspeak-url-template-define
 "CNNPodCasts"
 "http://www.cnn.com/services/podcasting/"
 nil
 nil
 "List CNN Podcast media links."
 #'(lambda (url)
     (emacspeak-we-extract-by-class-list
      '("cnnPODcastleft"
        "cnnPODcastright")
      url
      'speak )))

(emacspeak-url-template-define
 "CNN Content"
 "http://www.cnn.com/"
 nil
 nil
 "Filter down to CNN  content area."
 #'(lambda (url)
     (emacspeak-we-extract-by-class "column" url 'speak)))

(emacspeak-url-template-define
 "CNN technology "
 "http://www.cnn.com/TECH/"
 nil
 #'(lambda nil
     (declare (special emacspeak-we-class-filter))
     (setq emacspeak-we-class-filter "cnnStoryContent"))
 "CNN Technology news."
 #'(lambda (url)
     (emacspeak-we-extract-by-class-list
      (list
       "cnnSectT2s"
       "cnnSectT2head"
       "cnnSectBoxHeadW"
       "cnnSectBox")
      url 'speak)))

(emacspeak-url-template-define
 "CNN Market Data "
 "http://money.cnn.com/markets/data/"
 nil
 nil
 "CNN Money"
 #'(lambda (url)
     (emacspeak-we-extract-by-role
      "main" ;"wsod_marketsOverview" 
      url 'speak)))

(emacspeak-url-template-define
 "Money Content "
 "http://money.cnn.com/"
 nil
 'emacspeak-url-template-setup-content-filter
 "CNN Content"
 #'(lambda (url)
     (emacspeak-we-extract-by-id
      "cnnMoneyBody"
      url
      'speak)))

;;}}}
;;{{{  The Linux Show

(emacspeak-url-template-define
 "Geek Linux Daily"
 "http://thelinuxdaily.com/shows/%s.m3u"
 (list
  #'(lambda ()
      (emacspeak-speak-collect-date "Date:"
                                    "%Y/%m/%d")))
 nil
 "Play specified edition of Geek  Linux DailyShow"
 #'(lambda (url)
     (funcall emacspeak-media-player url 'play-list)))

(emacspeak-url-template-define
 "Redhat Linux Show"
 "http://www.thelinuxshow.com/archives/%s.mp3"
 (list
  #'(lambda ()
      (let ((mm-dd-yy
             (emacspeak-speak-collect-date
              "Date: (Tuesday)"
              "%m-%d-%Y")))
        (format "%s/tls-%s"
                (third (split-string mm-dd-yy "-"))
                mm-dd-yy))))
 nil
 "Play specified edition of Redhat Linux Show"
 #'(lambda (url)
     (funcall emacspeak-media-player url 'play-list)))

;;}}}
;;{{{  linux today

(emacspeak-url-template-define
 "Linux Today News"
 "http://www.linuxtoday.com/"
 nil
 nil
 "Get news column from Linux Today."
 #'(lambda (url)
     (emacspeak-we-xslt-filter
      "(//table)[2]/tr/td[2]"
      url
      'speak)))

;;}}}
;;{{{ sourceforge

(emacspeak-url-template-define
 "sourceforge project"
 "http://sourceforge.net/projects/%s"
 (list "Project name")
 nil
 "Open specified project page at SourceForge.")

(emacspeak-url-template-define
 "sourceforge browse mirrors"
 "http://prdownloads.sourceforge.net/%s/?sort_by=date"
 (list "Project name")
 nil
 "Retrieve download page  at Sourceforge for specified project.")

(emacspeak-url-template-define
 "sourceforge Download"
 "http://prdownloads.sourceforge.net/%s"
 (list "File: project/filename: ")
 nil
 "Download specified file."
 'browse-url
 'dont-url-encode)

;;}}}
;;{{{  MLB scores
;;; standings:

(emacspeak-url-template-define
 "MLB Scorecard"
                                        ;"http://gd.mlb.com/components/game/mlb/%s/master_scoreboard.xml"
 "http://gd.mlb.com/components/game/mlb/%s/scoreboard.xml"
 (list
  #'(lambda nil
      (let ((date
             (emacspeak-speak-collect-date
              "Date: "
              "%Y-%m-%d"))
            (fields nil)
            (result nil))
        (setq fields (split-string date "-"))
        (setq result
              (format
               "year_%s/month_%s/day_%s"
               (first fields)
               (second fields)
               (third fields)))
        result))
  )
 'emacspeak-speak-buffer
 "Show MLB Scorecard."
 #'(lambda (url)
     (emacspeak-xslt-view-xml
      (expand-file-name "mlb-scorecard.xsl" emacspeak-xslt-directory)
      url)))

(emacspeak-url-template-define
 "Baseball standings"
 "http://www.mlb.com/NASApp/mlb/mlb/standings/index.jsp"
 nil
 nil
 "Display MLB standings."
 #'(lambda (url)
     (emacspeak-we-extract-table-by-match
      "Standings"
      url 'speak)))

(emacspeak-url-template-define
 "Baseball Game Index"
                                        ;"http://gd.mlb.com/components/game/%s"
 "http://gd.mlb.com/components/game/mlb/%s/"
 (list
  #'(lambda nil
      (let ((date
             (emacspeak-speak-collect-date "Date: "
                                           "%Y-%m-%d"))
            (fields nil)
            (result nil))
        (setq fields (split-string date "-"))
        (setq result
              (format
               "year_%s/month_%s/day_%s/"
               (first fields)
               (second fields)
               (third fields)))
        result)))
 nil
 "Display baseball Play By Play."
 )

(emacspeak-url-template-define
 "Baseball Play By Play"
 "http://gd.mlb.com/components/game/%s_%smlb_%smlb_1/playbyplay.html"
 (list
  #'(lambda nil
      (let ((date
             (emacspeak-speak-collect-date
              "Date: "
              "%Y-%m-%d"))
            (fields nil)
            (result nil))
        (setq fields (split-string date "-"))
        (setq result
              (format
               "year_%s/month_%s/day_%s/gid_%s_%s_%s"
               (first fields)
               (second fields)
               (third fields)
               (first fields)
               (second fields)
               (third fields)))
        result))
  "Visiting Team: "
  "Home Team: ")
 nil
 "Display baseball Play By Play."
 )

(emacspeak-url-template-define
 "Baseball scores"
 "http://gd.mlb.com/components/game/mlb/%s_%smlb_%smlb_1/boxscore.html"
 (list
  #'(lambda nil
      (let ((date
             (emacspeak-speak-collect-date
              "Date: "
              "%Y-%m-%d"))
            (fields nil)
            (result nil))
        (setq fields (split-string date "-"))
        (setq result
              (format
               "year_%s/month_%s/day_%s/gid_%s_%s_%s"
               (first fields)
               (second fields)
               (third fields)
               (first fields)
               (second fields)
               (third fields)))
        result))
  "Visiting Team: "
  "Home Team: ")
 nil
 "Display baseball scores."
 )

;;}}}
;;{{{ Listening to Air Traffic control

(emacspeak-url-template-define
 "Air Traffic Control"
 "http://www.liveatc.net/search?icao=%s"
 (list "Airport Code: ")
 nil
 "Find live streams for Air Traffic Control."
 #'(lambda (url)
     (emacspeak-we-extract-by-class
      "col1wrap"
      url
      'speak)))

;;}}}
;;{{{  times of india

;;; create url rewrite url to get print page
(emacspeak-url-template-define
 "Times Of India"
 "http://www.timesofindia.com"
 nil
 #'(lambda ()
     (declare (special emacspeak-we-url-rewrite-rule))
     (setq emacspeak-we-url-rewrite-rule
           (list "$" "&prtPage=1")))
 "Retrieve Times Of India.
Set up URL rewrite rule to get print page."
 )

;;}}}
;;{{{ weather underground
;;;###autoload
(defcustom emacspeak-url-template-weather-city-state
  "95123"
  "Default city/state for weather forecasts"
  :type 'string
  :group 'emacspeak-url-template)

(emacspeak-url-template-define
 "rss weather from wunderground"
 "http://www.wunderground.com/auto/rss_full/%s.xml?units=both"
 (list
  #'(lambda nil
      (declare (special emacspeak-url-template-weather-city-state))
      (read-from-minibuffer "State/City:"
                            emacspeak-url-template-weather-city-state)))
 nil
 "Pull RSS weather feed for specified state/city."
 #'emacspeak-feeds-rss-display)

(emacspeak-url-template-define
 "Weather forecast from Weather Underground"
 "http://mobile.wunderground.com/cgi-bin/findweather/getForecast?query=%s"
 (list
  #'(lambda () (read-from-minibuffer "Zip: " emacspeak-url-template-weather-city-state)))
 'emacspeak-speak-buffer
 "Weather forecast from weather underground mobile."
 )

;;}}}
;;{{{ airport conditions:
(emacspeak-url-template-define
 "Airport conditions"
 "http://www.fly.faa.gov/flyfaa/flyfaaindex.jsp?ARPT=%s&p=0"
 (list "Airport Code:")
 nil
 "Display airport conditions from the FAA."
 #'(lambda (url)
     (emacspeak-we-extract-table-by-match "Status"
                                          url 'speak)))

;;}}}
;;{{{ emacs wiki search

(emacspeak-url-template-define
 "EmacsWiki Search"
 "http://www.emacswiki.org/cgi-bin/wiki?search=%s"
 (list "Search EmacsWiki For: ")
 #'(lambda nil
     (search-forward "Result page" nil t)
     (emacspeak-speak-line))
 "EmacsWiki Search")

;;}}}
;;{{{ reuters

(emacspeak-url-template-define
 "Reuters Finance"
 "http://today.reuters.com/stocks/Overview.aspx?ticker=%s&fs=1"
 (list "Ticker: ")
 nil
 "Reuters Finance Lookup")

;;}}}
;;{{{ ask

(emacspeak-url-template-define
 "ask search mobile"
 "http://mobile.ask.com/web.jsp?fi_what=%s&fi_Search=Search&form=web"
 (list "Ask Mobile Search: ")
 #'(lambda nil
     (search-forward "results")
     (emacspeak-speak-rest-of-buffer))
 "Mobile search using Ask.com")

(emacspeak-url-template-define
 "Ask Walking Directions"
 "http://mobile.ask.com/dd.jsp?fi_st_addr=%s&fi_end_addr=%s&fi_method=Walk&form=dd"
 (list "Start Address: "
       "End Address: ")
 #'(lambda()
     (search-forward "Time:")
     (beginning-of-line)
     (emacspeak-speak-rest-of-buffer))
 "Walking directions from Ask.com")

(emacspeak-url-template-define
 "Ask Local Search"
 "http://mobile.ask.com/local.jsp?fi_what=%s&fi_where=%s&fi_Search=Search&form=local)"
 (list "Find: "
       "Near: ")
 #'(lambda nil
     (search-forward "Sort")
     (forward-line 1)
     (emacspeak-speak-rest-of-buffer))
 "Ask Local Search.")
;;}}}
;;{{{  wordnet

(emacspeak-url-template-define
 "WordNet Search"
 "http://wordnet.princeton.edu/perl/webwn?s=%s"
 (list "WordNet Define: ")
 'emacspeak-speak-buffer
 "Look up term in WordNet.")

;;}}}
;;{{{ prairie home companion 

(emacspeak-url-template-define
 "PHC Prairie Home Companion"
 "http://www.publicradio.org/tools/media/player/phc/%s_phc.ram"
 (list 'emacspeak-speak-read-date-year/month/date)
 nil
 "Play Prairie Home Companion"
 #'(lambda (url)
     (funcall emacspeak-media-player  url 'play-list))) 

;;}}}
;;{{{  earthquakes 

(emacspeak-url-template-define
 "Earthquakes"
 "http://earthquake.usgs.gov/earthquakes/recenteqsus/Quakes/quakes_all.php"
 nil
 nil
 "Show table of recent quakes."
 #'(lambda (url)
     (emacspeak-we-xslt-filter "//tr[position() < 10]"
                               url
                               'speak)))

;;}}}
;;{{{  Radio station streams 

(emacspeak-url-template-define
 "StreamWorld Radio"
 "http://provisioning.streamtheworld.com/pls/%s.pls"
 (list
  #'(lambda () (upcase (read-from-minibuffer "Station ID: "))))
 nil
 "Play radio stream.
See http://www.cbsradio.com/streaming/index.html for a list of CBS  stations that use StreamTheWorld."
 #'(lambda (url)
     (emacspeak-m-player url 'playlist)))

;;}}}
;;{{{ Bing RSS

(emacspeak-url-template-define
 "Bing Search"
 "http://www.bing.com/search?format=rss&q=%s"
 (list "Bing Search: ")
 nil
 "Bing results as RSS feed."
 #'emacspeak-feeds-rss-display)

(emacspeak-url-template-define
 "Bing News"
 "http://www.bing.com:80/news/search?q=%s&format=RSS"
 (list "Bing Search: ")
 nil
 "Bing News results as RSS feed."
 #'emacspeak-feeds-rss-display)

;;}}}
;;{{{ GitHub Search

(emacspeak-url-template-define
 "GitHub Search"
 "https://github.com/search?q=%s"
 (list "Query: ")
 #'(lambda ()
     (emacspeak-imenu-goto-next-index-position)
     (emacspeak-speak-rest-of-buffer))
 "Perform a GitHub Search.")

;;}}}
;;{{{ TuneIn: streamId->URL
; "http://stream.radiotime.com/listen.stream?streamIds=4299203"wget  -O t 
(emacspeak-url-template-define
 "TuneIn Radio"
 "http://stream.radiotime.com/listen.stream?streamIds=%s"
 (list "StreamId: ")
 nil
 "Translate StreamId to playable stream."
 #'(lambda (url)
     (kill-new url)
     (message "%s" url))
 "TuneIn Helper.")

(emacspeak-url-template-define
 "RadioTime  Browser"
 "http://opml.radiotime.com/"
 nil
 #'(lambda ()
     (declare (special emacspeak-we-url-executor))
     (setq emacspeak-we-url-executor 'emacspeak-feeds-opml-display))
 "RadioTime Entry point."
 #'emacspeak-feeds-opml-display)

(emacspeak-url-template-define
 "RadioTime  Search"
 "http://opml.radiotime.com/Search.ashx?query=%s"
 (list "Search: ")
 #'(lambda ()
     (declare (special emacspeak-we-url-executor))
     (setq emacspeak-we-url-executor 'emacspeak-feeds-opml-display))
 "RadioTime Search."
 #'emacspeak-feeds-opml-display)

;;}}}
;;{{{ Interactive commands

;;;###autoload
(defun emacspeak-url-template-open (ut)
  "Fetch resource identified by URL template."
  (declare (special  emacspeak-web-post-process-hook ))
  (let ((fetcher (or (emacspeak-url-template-fetcher ut) 'browse-url))
        (url (emacspeak-url-template-url ut))
        (action (emacspeak-url-template-post-action ut))
        (name (emacspeak-url-template-name ut)))
    (when action (add-hook 'emacspeak-web-post-process-hook action))
    (kill-new url)
    (funcall fetcher   url)))

(defsubst emacspeak-url-template-help-internal (name)
  "Display and speak help."
  (with-output-to-temp-buffer "*Help*"
    (princ name)
    (princ "\n\n")
    (princ
     (emacspeak-url-template-documentation
      (emacspeak-url-template-get name)))
    (save-current-buffer
      (set-buffer standard-output)
      (fill-region (point-min)
                   (point-max)))
    (help-print-return-message))
  (emacspeak-speak-help)
  (emacspeak-auditory-icon 'help))
(defun emacspeak-url-template-generate-name-setter (name)
  "Generate a setter that sets emacspeak-eww-url-template
to specified name for use as a callback."
  (eval
   (function
    `(lambda ()
       (declare (special emacspeak-eww-url-template))
       (setq emacspeak-eww-url-template ',name)))))

;;;###autoload
(defun emacspeak-url-template-fetch (&optional documentation)
  "Fetch a pre-defined resource.
Use Emacs completion to obtain a list of available resources.
Resources typically prompt for the relevant information
before completing the request.
Optional interactive prefix arg displays documentation for specified resource."
  (interactive "P")
  (let ((completion-ignore-case t)
        (emacspeak-speak-messages nil)
        (name  nil))
    (setq name
          (completing-read "Resource: "
                           emacspeak-url-template-table
                           nil
                           'must-match))
    (cond
     (documentation (emacspeak-url-template-help-internal name))
     (t
      (add-hook
       'emacspeak-web-post-process-hook
       (emacspeak-url-template-generate-name-setter name))
      (emacspeak-url-template-open (emacspeak-url-template-get name))))))

(defun emacspeak-url-template-help ()
  "Display documentation for  a URL template.
Use Emacs completion to obtain a list of available
resources."
  (interactive)
  (declare (special emacspeak-url-template-table))
  (let ((completion-ignore-case t)
        (name nil))
    (setq name
          (completing-read "Resource: "
                           emacspeak-url-template-table))
    (emacspeak-url-template-help-internal  name)))

;;}}}
;;{{{ Generate texinfo documentation for all defined url

(defun emacspeak-url-template-generate-texinfo-documentation (buffer)
  "Generates texinfo section documenting all defined URL templates."
  (declare (special emacspeak-url-template-table))
  (insert
   "@node URL Templates \n@section  URL Templates\n\n")
  (insert
   (format
    "This section is generated automatically from the source-level documentation.
Any errors or corrections should be made to the source-level
documentation.
This section documents a total of %d URL Templates.\n\n"
    (hash-table-count emacspeak-url-template-table)))
  (insert
   (format
    "All of these URL templates can be invoked via command
  @kbd{M-x emacspeak-url-template-fetch} normally bound to
  @kbd{%s}.
This command prompts for the name of the template, and completion
  is available via Emacs' minibuffer completion.
Each URL template carries out the following steps:
@itemize @bullet
@item Prompt for the relevant information.
@item Fetch the resulting URL using an appropriate fetcher.
@item Set up the resulting resource with appropriate
  customizations.
@end itemize

As an example, the URL templates for weather forecasts 
prompts for a location and speaks the forecast. \n\n"
    (mapconcat #'key-description
               (where-is-internal
                'emacspeak-url-template-fetch)
               " ")))
  (let ((keys
         (sort
          (loop for key being the hash-keys of emacspeak-url-template-table
                collect key)
          'string-lessp)))
    (loop for key in keys
          do
          (insert
           (format "@kbd{%s}\n\n" key))
          (insert
           (emacspeak-url-template-documentation
            (emacspeak-url-template-get key)))
          (insert "\n\n"))))

;;}}}
(provide 'emacspeak-url-template)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
