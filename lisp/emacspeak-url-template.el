;;; emacspeak-url-template.el --- Create library of URI templates
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:   Implement library of URI templates
;;; Keywords: Emacspeak, Audio Desktop
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-06-06 10:03:16 -0700 (Wed, 06 Jun 2007) $ |
;;;  $Revision: 4626 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1995 -- 2007, T. V. Raman<raman@cs.cornell.edu>
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

;;; It is often useful to have ``parameterized hot list entries''
;;; i.e., hotlist entries  that are ``templates'' for the
;;; actual URL.
;;; The user provides values for the parameterized portions
;;; of the URL e.g. the date.
;;; See @xref{URL Templates}, for details on the URL templates
;;; that are presently defined.

;;; Code:

;;}}}
;;{{{ required modules

(require 'emacspeak-preamble)
(require 'emacspeak-websearch)
(eval-when-compile (require 'emacspeak-w3))
;;}}}
;;{{{  structures

(defstruct (emacspeak-url-template
            (:constructor emacspeak-url-template-constructor))
  name                                  ;Human-readable name
  template                              ;template URL string
  generators                            ; list of param generator
  post-action                         ;action to perform after opening
  documentation                         ;resource  documentation
  fetcher                               ; custom fetcher
  dont-url-encode)

;;}}}
;;{{{ Helpers

(defun emacspeak-url-template-url (ut)
  "Instantiate URL identified by URL template."
  (declare (special emacspeak-url-template-current-ut))
  (setq emacspeak-url-template-current-ut nil)
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
                (setq emacspeak-url-template-current-ut
                      (list (emacspeak-url-template-name ut) input))
                input))
          (emacspeak-url-template-generators ut))))

(defun emacspeak-url-template-collect-date (prompt time-format-string)
  "Smart date collector.
Prompts with `prompt'.
`time-format-string' is format argument for format-time-string.
This function is sensitive to calendar mode when prompting."
  (let ((default (format-time-string time-format-string))) ; today is default
    (when (eq major-mode 'calendar-mode)
                                        ;get smart default from calendar
      (let ((date (calendar-cursor-to-nearest-date)))
        (setq default (format-time-string time-format-string
                                          (apply 'encode-time 0 0
                                                 0
                                                 (second date)
                                                 (first date)
                                                 (list (third date )))))))
    (read-from-minibuffer prompt
                          default
                          nil nil nil
                          default)))

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

(defvar emacspeak-url-template-name-alist nil
  "Alist of url template names --used by completing-read when
prompting for a template.")
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
  (declare (special emacspeak-url-template-table
                    emacspeak-url-template-name-alist))
  (unless (emacspeak-url-template-get  name)
    (push (list name name )
          emacspeak-url-template-name-alist))
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
 "Display package tracking information from UPS.")

;;}}}
;;{{{ amazon
(emacspeak-url-template-define
 "Amazon Product Details By ASIN"
 "http://amazon.com/o/dt/upda-1.0-i/tg/aa/upda/item/-/%s"
 (list "ASIN Or ISBN: ")
 nil
 "Retrieve product details from Amazon by either ISBN or ASIN.")
;;}}}
;;{{{ bookshare
(defcustom emacspeak-bookshare-user-id nil
  "Bookshare user Id."
  :type '(choice :tag "Bookshare User id"
                 (const :tag "None" nil)
                 (string :tag "Email"))
  :group 'emacspeak-url-template)

(emacspeak-url-template-define
 "BookShare"
 "https://www.bookshare.org/web/MembersLogin.html?email=%s&password=%s&operation=submit"
 (list
  #'(lambda nil
      (read-from-minibuffer
       "Bookshare UserId: "
       emacspeak-bookshare-user-id))
  #'(lambda nil
      (read-passwd  "Password: ")))
 nil
 "Bookshare Login")

(defun emacspeak-url-template-calendar-to-seconds ()
  "Convert date under cursor to seconds since epoch."
  (unless (eq 'calendar-mode  major-mode)
    (error "Not in the calendar."))
  (let ((date (calendar-cursor-to-date)))
    (format "%d"
            (float-time
             (encode-time 0 0 0
                          (second date)
                          (first date)
                          (third date)))))        )


(emacspeak-url-template-define
 "Periodicals from Bookshare"
 "http://www.bookshare.org/web/DownloadPeriodical.html?publishtitleid=%s&date=%s&format=1"
 (list
  "Periodical: "
  'emacspeak-url-template-calendar-to-seconds)
 nil
 "Fetch periodical from Bookshare.")


;;}}}
;;{{{ shoutcast
(defvar emacspeak-url-template-shoutcast-history nil
  "History to track shoutcast searchses.")

(emacspeak-url-template-define
 "Shoutcast Search"
 "http://yp.shoutcast.com/directory?s=%s&l=25"
 (list
  #'(lambda ()
      (let ((query
             (read-from-minibuffer "Shoutcast search: "
                                   (car
                                    emacspeak-url-template-shoutcast-history)
                                   nil nil
                                   'emacspeak-url-template-shoutcast-history)))
        (pushnew query emacspeak-url-template-shoutcast-history
                 :test #'string-equal)
        (emacspeak-url-encode query))))
 nil
 "Locate and display Shoutcast streams."
 #'(lambda (url)
     (emacspeak-w3-xslt-filter
      "(//table//table)[8]//td[position()=3 or position()=5]"
      url
      'speak)))

;;}}}
;;{{{  old time radio
(emacspeak-url-template-define
 "Old Time Radio"
 "http://www.oldtimeradioprograms.com"
 nil
 nil
 "This months Old Time Radio Programing"
 #'(lambda (url)
     (emacspeak-w3-extract-nested-table-list
      (list 2 3 )
      url)))
;;}}}
;;{{{ Netcraft surveys
(emacspeak-url-template-define
 "Netcraft Web Analysis"
 "http://uptime.netcraft.com/up/graph?display=uptime&site=%s"
 (list "Site to analyze: ")
 nil
 "Analyze WWW site using Netcraft."
 #'(lambda (url)
     (emacspeak-w3-extract-tables-by-match-list
      (list "running" "average")
      url 'speak)))

(emacspeak-url-template-define
 "Netcraft Site Report"
 "http://toolbar.netcraft.com/site_report?url=%s"
 (list "Site Report: ")
 'emacspeak-speak-buffer
 "Analyze WWW site using Netcraft."
 nil
 'dont-url-encode)

;;}}}
;;{{{  digg

(emacspeak-url-template-define
 "Digg"
 "http://www.digg.org/rss/index.xml"
 nil
 nil
 "Display Digg Feed."
 'emacspeak-rss-display)

;;}}}
;;{{{ bbc

(emacspeak-url-template-define
 "BBC 7 Schedule"
 "http://www.bbc.co.uk/bbc7/listings/index.shtml?%s"
 (list
  #'(lambda ()
      (read-from-minibuffer
       "BBC 7 Schedule: "
       (when (eq major-mode 'calendar-mode)
         (calendar-day-name (calendar-cursor-to-date))))))
 nil
 "Retrieve BBC7 schedule for specified day."
 #'(lambda (url)
     (emacspeak-w3-extract-table-by-match
      "Morning"
      url 'speak)))

(emacspeak-url-template-define
 "BBC Radio4 On Demand"
 "rtsp://rmv8.bbc.net.uk/radio4/%s.ra"
 (list "WeekdayTime: ")
 nil
 "Specify a week day (three letters -- lower case -- and a time spec
-- e.g. 1230 --
to play a BBC Radio4 program on demand."
 #'(lambda (url)
     (funcall emacspeak-media-player  url 'play-list)))

(emacspeak-url-template-define
 "BBC Radio7 On Demand"
 "rtsp://rmv8.bbc.net.uk/bbc7/%s_%s.ra"
 (list "hhmm" "day")
 nil
 "Specify a week day (three letters -- lower case -- and a time spec
-- e.g. 1230 --
to play a BBC Radio7 program on demand."
 #'(lambda (url)
     (funcall emacspeak-media-player  url 'play-list)))

(emacspeak-url-template-define
 "BBC Listen Again"
 "http://www.bbc.co.uk/radio4/progs/listenagain.shtml"
 nil
 #'(lambda ()
     (search-forward "ABCDEFGHIJKLMNOPQRSTUVWXYZ" nil t)
     (forward-line 2)
     (search-forward "A" nil t)
     (emacspeak-speak-line))
 "BBC Listen Again Listings"
 #'(lambda (url)
     (emacspeak-w3-browse-url-with-style
      (expand-file-name "linearize-tables.xsl"
                        emacspeak-xslt-directory)
      url)))
;;{{{ bbc channel

(defvar  emacspeak-url-template-bbc-channels-content
  "http://www.bbc.co.uk/radio/aod/shows/rpms/"
  "Location of BBC audio content.")

(defun emacspeak-url-template-bbc-channel-player (url)
  "Extract program name, construct realplayer URL and play that
content."
  (declare (special  emacspeak-url-template-bbc-channels-content))
  (let ((content (second
                  (split-string url "?")))
        (uri nil))
    (cond
     ((null content)
      (error "Cannot locate content particle in %s" url))
     (t
      (setq uri
            (concat emacspeak-url-template-bbc-channels-content
                    content
                    ".rpm"))
      (kill-new uri)
      (funcall emacspeak-media-player  uri 'play-list)
      (message "Playing content under point.")))))

(emacspeak-url-template-define
 "BBC Channel On Demand"
 "http://www.bbc.co.uk/radio/aod/networks/%s/audiolist.shtml"
 (list "BBC Channel: ")
 #'(lambda ()
     (declare (special emacspeak-w3-url-executor))
     (setq emacspeak-w3-url-executor
           'emacspeak-url-template-bbc-channel-player))
 "Display BBC Channel on demand.")

;;}}}
;;{{{ bbc Genres

(emacspeak-url-template-define
 "BBC Genres On Demand"
 "http://www.bbc.co.uk/radio/aod/genres/%s/audiolist.shtml"
 (list "BBC Genre: ")
 #'(lambda ()
     (declare (special emacspeak-w3-url-executor))
     (setq emacspeak-w3-url-executor
           'emacspeak-url-template-bbc-channel-player))
 "Display BBC Channel on demand."
 )

;;}}}

(emacspeak-url-template-define
 "BBC Programs On Demand"
 "http://www.bbc.co.uk/radio/aod/shows/rpms/%s.rpm"
 (list "BBC Program: ")
 nil
 "Play BBC programs on demand."
 #'(lambda (url)
     (funcall emacspeak-media-player url 'play-list)))

(emacspeak-url-template-define
 "BBC News"
 "http://news.bbc.co.uk/2/low.html"
 nil
 #'(lambda nil
     (search-forward
      (format-time-string
       "%A, %e %B, %Y"
       (current-time)
       'universal))
     (emacspeak-auditory-icon 'open-object)
     (beginning-of-line)
     (emacspeak-speak-rest-of-buffer))
 "BBC News text version.")

(emacspeak-url-template-define
 "BBC Sports"
 "http://news.bbc.co.uk/sport2/low/default.stm"
 nil
 #'(lambda nil
     (search-forward
      (format-time-string "%A, %d %B, %Y"
                          (current-time)))
     (emacspeak-auditory-icon 'open-object)
     (beginning-of-line)
     (emacspeak-speak-rest-of-buffer))
 "BBC News text version."
 )

;;}}}
;;{{{  answers.com
(emacspeak-url-template-define
 "Answers.com"
 "http://www.answers.com/main/ntquery?s=%s"
 (list "Search answers.com for: ")
 nil
 "Search answers.com")

;;}}}
;;{{{ google CSE and Google Reader:
(defcustom emacspeak-url-template-reading-list-opml nil
   "OPML feed location to use for our Custom Search."
  :type '(choice
          (const :tag "None" nil)
          (string :tag "URL"))
  :group 'emacspeak-url-template)

(defsubst emacspeak-url-template-make-cse (meta-url)
  "Builds up a CSE url for specified meta-url."
  (format
       "http://www.google.com/cse/tools/makecse?url=%s"
       meta-url))


(emacspeak-url-template-define
 "Reader Subscription Search"
 "http://www.google.com/cse?q=%s&loading=1&cref=%s"
 (list
  "Reader Search: "
  #'(lambda nil
      (declare
       (special emacspeak-url-template-reading-list-opml))
      (emacspeak-url-template-make-cse
       (emacspeak-url-encode emacspeak-url-template-reading-list-opml))))
 nil
 "Search within feeds subscribed to in Google Reader."
 #'(lambda (url)
     (emacspeak-w3-extract-by-class "g" url 'speak)))

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
     (emacspeak-w3-extract-by-class "g" url 'speak)))

(emacspeak-url-template-define
 "On The Fly CSE"
 "http://www.google.com/cse?q=%s&loading=1&cref=%s"
 (list
  "Search This CSE: "
  #'(lambda nil
      (emacspeak-url-template-make-cse
       (read-from-minibuffer
        "Feed URL to build CSE for: "))))
 nil
 "Build a CSE on the fly and  use it to search."
 #'(lambda (url)
     (emacspeak-w3-extract-by-class "g" url 'speak)))





;;}}}

;;{{{ Anonimize google search

(emacspeak-url-template-define
 "Anonymize Google Search"
 "http://www.google.com/accounts/Logout"
 nil
 nil
 "Logout from Google to do an anonymous search.")

;;}}}
;;{{{ gmail:


(emacspeak-url-template-define
 "GMail Mobile"
 "http://mail.google.com/mail/x/"
 nil
 'emacspeak-speak-buffer
 "GMail Mobile XHTML version --- light-weight, fast!")

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
     (search-forward " Patent Search" nil t)
     (beginning-of-line)
     (emacspeak-speak-rest-of-buffer))
 "Perform patent search via Google")

;;}}}
;;{{{ google code search
(emacspeak-url-template-define
 "Code Search From Google"
 "http://www.google.com/codesearch?q=%s"
 (list "Google For Code: ")
 #'(lambda nil
     (search-forward "results"  )
     (emacspeak-speak-line))
 "Search using Google Code Search."
 #'(lambda (url)
     (emacspeak-w3-without-xsl (browse-url url))))

;;}}}
;;{{{ google music:

(emacspeak-url-template-define
 "Music Search"
 "http://www.google.com/musicsearch?q=%s"
 (list "Google Music Search:")
 #'(lambda nil
     (search-forward "results"  )
     (emacspeak-speak-line))
 "Music search on Google."
 #'(lambda (url)
     (emacspeak-w3-without-xsl
      (browse-url url))))

;;}}}
;;{{{  Google Video:

(emacspeak-url-template-define
 "Google Soccer Results"
 "http://www.google.com/search?q=%s+world+cup+soccer&num=1"
 (list "Countries:")
 nil
 "Display World Cup Soccer Results for specified countries."
 #'(lambda (url)
     (emacspeak-w3-extract-nested-table 5 url 'speak)))

(emacspeak-url-template-define
 "Google Video"
 "http://video.google.com/videofeed?type=search&q=%s&num=20&output=rss"
 (list "Video Search:")
 nil
 "Retrieve Google Video search results as an atom feed."
 'emacspeak-rss-display)

;;}}}
;;{{{ Google Reader:

(emacspeak-url-template-define
 "Google Reader"
 "http://www.google.com/reader/public/atom/feed/%s?n=100"
 (list "Feed:")
 nil
 "Google Reader"
 'emacspeak-atom-display)

;;}}}
;;{{{ google finance
;;; pull google finance search results via the transcoder

(emacspeak-url-template-define
 "Finance Google Search"
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



;;}}}
;;{{{ google maps

(emacspeak-url-template-define
 "EmapSpeak Via Google"
 "http://maps.google.com/maps?q=%s&output=kml"
 (list "Query: ")
 'emacspeak-speak-buffer
 "EmapSpeak Via Google.

Specify the query using English and  addresses as complete as
  possible.

Here are some examples:

0) To find a location by address specify:

650 Harry Road San Jose CA 95120

1) To get directions, specify:

<source address> to <destination address>

2) To find businesses etc., near a location, specify:

<what> near <location address>
"
 #'(lambda (url)
     (let ((buffer
            (emacspeak-xslt-xml-url
             (expand-file-name "kml2html.xsl" emacspeak-xslt-directory)
             url )))
       (browse-url-of-buffer buffer))))

;;}}}
;;{{{ google scholar

(emacspeak-url-template-define
 "Google Books"
 "http://books.google.com/books?q=%s&btnG=Search+Books&hl=en&output=html_text"
 (list "Google Book Search: ")
 #'(lambda nil
     (emacspeak-imenu-goto-next-index-position)
     (emacspeak-speak-rest-of-buffer))
 "Google Books Search with text when available")

(emacspeak-url-template-define
 "Google Scholar"
 "http://scholar.google.com/scholar?ie=UTF-8&oe=UTF-8&hl=en&btnG=Search&num=25&q=%s"
 (list "Google Scholar Search: ")
 #'(lambda nil
     (search-forward "Results" nil t)
     (emacspeak-speak-line))
 "Google Scholar Search")

;;}}}
;;{{{ google images

(emacspeak-url-template-define
 "Google Image Search"
 "http://images.google.com/images?hl=en&tab=wi&ie=UTF-8&q=%s"
 (list "Google Image Search: ")
 #'(lambda ()
     (search-forward "Showing" nil t)
     (emacspeak-speak-line))
 "Google Image Search"
 #'(lambda (url)
     (emacspeak-w3-without-xsl
      (browse-url url))))

;;}}}
;;{{{ googl blogsearch
(emacspeak-url-template-define
 "BlogSearch Google"
 "http://blogsearch.google.com/blogsearch_feeds?hl=en&q=%s&scoring=d&ie=utf-8&num=10&output=atom"
 (list "Google Blog Search: ")
 nil
 "Google Blog Search"
 'emacspeak-atom-display)

;;}}}
;;{{{ google translation service

black(emacspeak-url-template-define
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
  "URI")
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
     (emacspeak-w3-without-xsl
      (browse-url url))))

;;}}}
;;{{{ yubnub
(emacspeak-url-template-define
 "YubNub Web Command Line"
 "http://yubnub.org/parser/parse?command=%s"
 (list "YubNub Command: ")
 nil
 "YubNub Command Line"
 )

;;}}}
;;{{{ Google Reader:

(emacspeak-url-template-define
 "Google Reader"
 "http://www.google.com/reader/public/atom/feed/%s"
 (list "Feed: ")
 nil
 "View feed via Google Reader."
 'emacspeak-atom-display)

;;}}}
;;{{{  google filters
(emacspeak-url-template-define
 "Google WebQuotes"
 "http://labs.google.com/cgi-bin/webquotes?num_quotes=3&q=%s&btnG=Google+WebQuotes+Search&show_titles=1&bold_links=1&snippet_threshold=3"
 (list "Query: ")
 nil
 "Google WebQuotes.")
(emacspeak-url-template-define
 "Google Glossary"
 "http://labs.google.com/glossary?q=%s"
 (list "Term: ")
 nil
 "Google Glossary lookup.")
(emacspeak-url-template-define
 "Google Hits"
 "http://www.google.com/search?q=%s&num=%s"
 (list "Google For: "
       #'(lambda nil
           (declare (special  emacspeak-websearch-google-number-of-results))
           emacspeak-websearch-google-number-of-results))
 nil
 "Only show Google hits."
 #'(lambda (url)
     (emacspeak-w3-extract-by-class "g"
                                    url 'speak)))

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
       (save-excursion
         (set-buffer buffer)
         (browse-url-of-buffer)))))

;;}}}
;;{{{ Google Local:

(emacspeak-url-template-define
 "Google Local"
 "http://local.google.com/local?output=html&q=%s"
 (list "Google Local Search:")
 #'(lambda ()
     (search-forward "Local Search within" nil t)
     (emacspeak-speak-line))
 "Google Local Search.")

;;}}}
;;{{{ google OverviewOfNews

(emacspeak-url-template-define
 "Google Print"
 "http://print.google.com/print?oi=print&q=%s"
 (list "Google Print Search:")
 #'(lambda nil
     (search-forward "Print  Books" nil t)
     (emacspeak-speak-rest-of-buffer))
 "Google Print Search")

(emacspeak-url-template-define
 "Google Text  News"
 "http://news.google.com/news?ned=tus"
 nil
 #'(lambda nil
     (emacspeak-speak-rest-of-buffer))
 "Retrieve and speak Google News Overview."
 #'(lambda (url)
     (emacspeak-w3-without-xsl
      (browse-url url))))

(emacspeak-url-template-define
 "Google News Search"
 "http://news.google.com/news?hl=en&ned=tus&q=%s&btnG=Google+Search&output=atom"
 (list "Search news for: ")
 nil
 "Search Google news."
 #'(lambda (url)
     (emacspeak-atom-display url 'speak)))

(emacspeak-url-template-define
 "Google Recent News Search"
 "http://news.google.com/news?hl=en&ned=tus&q=%s&scoring=d"
 (list "Search news for: ")
 #'(lambda nil
     (or
      (search-forward "Sorted by" (point-max) 'no-error)
      (search-forward "Top Stories" (point-max) 'no-error))
     (forward-line 4)
     (emacspeak-speak-line))
 "Search Google news."
 #'(lambda (url)
     (emacspeak-w3-without-xsl
      (browse-url url))))

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
 "Google RSS News"
 "http://news.google.com/news?hl=en&ned=tus&q=%s&scoring=d&ie=ISO-8859-1&output=rss"
 (list "Search news for: ")
 nil
 "Search Google news."
 #'(lambda (url)
     (emacspeak-rss-display url 'speak)))

(emacspeak-url-template-define
 "Google Atom News"
 "http://news.google.com/news?ned=us&topic=%s&output=atom"
 (list "Topic Code: ")
 nil
 "Display specified news feed."
 #'(lambda (url)
     (emacspeak-atom-display url 'speak)))

(emacspeak-url-template-define
 "Google Feeds"
 "http://news.google.com/intl/en_us/news_feed_terms.html"
 nil
 nil
 "List  Google news Feeds."
 #'(lambda (url)
     (emacspeak-w3-extract-table-by-match "Sports"
                                          url 'speak)))

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
     (emacspeak-w3-without-xsl
      (browse-url url))))

;;}}}
;;{{{  cnet news

(emacspeak-url-template-define
 "Tech News From CNet"
 "http://rss.com.com/2547-12-0-20.xml"
 nil
 'emacspeak-speak-buffer
 "Display tech news from CNET"
 'emacspeak-rss-display)

;;}}}
;;{{{ Infoworld RSS
(emacspeak-url-template-define
 "InfoWorld RSS Feeds"
 "http://www.infoworld.com/rss/rss_info.html"
 nil
 nil
 "Produce  a set of RSS links published by InfoWorld."
 #'(lambda (url)
     (emacspeak-w3-xslt-filter
      "//a[contains(@href, \".rdf\") and @class]"
      url  'speak)))

;;}}}

;;{{{ mapquest

(emacspeak-url-template-define
 "MapQuest Directions"
 "http://www.mapquest.com/directions/main.adp?go=1&do=nw&1y=US&2y=US&ct=NA&1a=%s&1c=%s&1c=%s&1z=%s&2a=%s&2c=%s&2s=%s&2z=%s"
 (list "Start Address:" "City:" "State:" "Zip:"
       "Destination Address:" "City:" "State:" "Zip:")
 nil
 "Retrieve and speak directions from MapQuest."
 #'(lambda (url)
     (emacspeak-w3-extract-table-by-match "Maneuvers"
                                          url 'speak)))

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
     (emacspeak-w3-xslt-filter
      "//a[not(contains(@href,\"url\"))and  contains(@href, \"rss\") ]"
      url 'speak)))

(defun emacspeak-url-template-yahoo-news-processor (url)
  "Process and speak Yahoo news."
  (declare (special emacspeak-w3-post-process-hook))
  (add-hook 'emacspeak-w3-post-process-hook
            #'(lambda nil
                (declare (special  emacspeak-w3-url-rewrite-rule
                                   emacspeak-w3-class-filter))
                (setq emacspeak-w3-class-filter "article"
                      emacspeak-w3-url-rewrite-rule
                      '("$" "&printer=1"))
                (emacspeak-speak-buffer)))
  (emacspeak-w3-xslt-filter
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
;;{{{ Adobe pdf conversion

(emacspeak-url-template-define
 "pdf2txt"
 "http://access.adobe.com/access/convert.do?acceptLanguage=en&srcPdfUrl=%s&convertTo=text&visuallyImpaired=true&preferHTMLReason=&platform=Linux&comments=&submit=Convert"
 (list "PDF URL: ")
 nil
 "Use access.adobe.com to  convert a remote PDF document to plain
 text.
The PDF document needs to be available on the public Internet.")

(emacspeak-url-template-define
 "pdf2html"
 "http://access.adobe.com/access/convert.do?acceptLanguage=en&srcPdfUrl=%s&convertTo=html&visuallyImpaired=true&preferHTMLReason=&platform=Linux&comments=&submit=Convert"
 (list "PDF URL: ")
 nil
 "Use access.adobe.com to  convert a remote PDF document to plain
 text.
The PDF document needs to be available on the public Internet.")

;;}}}
;;{{{ w3c

(emacspeak-url-template-define
 "w3c IRC Logs"
 "http://www.w3.org/%s-%s-irc "
 (list
  #'(lambda nil
      (emacspeak-url-template-collect-date "Date: "
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
  (emacspeak-url-template-collect-date "Date range: "
                                       "%Y%h"))

;;}}}
;;{{{ cnn

(emacspeak-url-template-define
 "CNN PodCasts"
 "http://www.cnn.com/services/podcasting/"
 nil
 nil
 "List CNN Podcast media links."
 #'(lambda (url)
     (emacspeak-w3-extract-media-streams url 'speak)))
;;{{{ cnnfn content
(emacspeak-url-template-define
 "CNNFn Content"
 "http://www.cnnfn.com/"
 nil
 nil
 "Extract content links from CNN FN."
 #'(lambda (url)
     (emacspeak-w3-extract-by-class-list
      (list
       "t1headline"
       "t1tease"
       "tease" "t2headline")
      url 'speak)))

;;}}}

(emacspeak-url-template-define
 "CNN headlines "
 "http://www.cnn.com"
 nil
 nil
 "Retrieve and speak headline news from CNN."
 #'(lambda (url)
     (emacspeak-w3-extract-by-class "cnnMainT1" url 'speak)))

(defun emacspeak-url-template-date-YearMonthDate ()
  "Return today as yyyymmdd"
  (emacspeak-url-template-collect-date "Date:"
                                       "%Y%m%d"))

(defun emacspeak-url-template-date-year/month/date ()
  "Return today as yyyy/mm/dd"
  (emacspeak-url-template-collect-date "Date:"
                                       "%Y/%m/%d"))

(defun emacspeak-url-template-date-month/date ()
  "Return today as mm/dd"
  (emacspeak-url-template-collect-date "Date:"
                                       "%m/%d") )

(emacspeak-url-template-define
 "CNN Tecnology "
 "http://www.cnn.com/TECH/"
 nil
 #'(lambda nil
     (declare (special emacspeak-w3-class-filter))
     (setq emacspeak-w3-class-filter "cnnStoryContent"))
 "CNN Technology news."
 #'(lambda (url)
     (emacspeak-w3-extract-by-class-list
      (list
       "cnnSectT2s"
       "cnnSectT2head"
       "cnnSectBoxHeadW"
       "cnnSectBox")
      url 'speak)))

(emacspeak-url-template-define
 "CNN Market News "
 "http://money.cnn.com/markets/news/"
 nil
 nil
 "CNN Money"
 #'(lambda (url)
     (emacspeak-w3-extract-tables-by-position-list
      '(10 12 15 18 20 21)
      url 'speak)))

(emacspeak-url-template-define
 "CNN Market Data "
 "http://money.cnn.com/markets/data/"
 nil
 nil
 "CNN Money"
 #'(lambda (url)
     (emacspeak-w3-extract-tables-by-position-list
      '(14 15 20 21) url 'speak)))

(emacspeak-url-template-define
 "CNN Content "
 "http://www.cnn.com/"
 nil
 nil
 "CNN Content"
 #'(lambda (url)
     (emacspeak-w3-xslt-filter
      "//p|//h1|//h2|//h3|//ul|//ol"
      url
      'speak)))

;;}}}
;;{{{ pbs --pulpit
(emacspeak-url-template-define
 "Pulpit --- I Cringely"
 "http://www.pbs.org/cringely/pulpit/pulpit%s.html"
 (list
  #'(lambda nil
      (emacspeak-url-template-collect-date "Date: (most recent Thursday)"
                                           "%Y%m%d")))
 nil
 "Read pulpit from PBS. Published on the Thursday of the week."
 #'(lambda (url)
     (emacspeak-w3-xslt-filter
      "//p|ul|ol|dl|h1|h2|h3|h4|h5|h6|blockquote" url 'speak)))

;;}}}
;;{{{  NPR programs

(emacspeak-url-template-define
 "American Life On Demand."
                                        ;"http://www.wbez.org/ta/%s.rm"
 "http://www.thislife.org/ra/%s.ram"
 (list "Episode: ")
 nil
 "Play This American Life  shows on demand."
 #'(lambda (url)
     (funcall emacspeak-media-player url 'play-list)))

(emacspeak-url-template-define
 "Wait Wait, Dont Tell Me (NPR)"
 "http://www.npr.org/dmg/dmg.php?mediaURL=/waitwait/%s_waitwait&mediaType=RM"
 (list
  #'(lambda ()
      (emacspeak-url-template-collect-date "Date: (Saturdays)"
                                           "%Y%m%d")))
 nil
 "Play Wait, Wait Dont Tell Me from NPR."
 #'(lambda (url)
     (funcall emacspeak-media-player url 'play-list)))

(emacspeak-url-template-define
 "NPR On Demand"
 "http://www.npr.org/dmg/dmg.php?prgCode=%s&showDate=%s&segNum=%s&mediaPref=RM"
 (list
  #'(lambda ()
      (upcase (read-from-minibuffer "Program code:")))
  #'(lambda ()
      (emacspeak-url-template-collect-date "Date:"
                                           "%d-%b-%Y"))
  "Segment:")
 nil
 "Play NPR shows on demand.
Program is specified as a program code:

ME              Morning Edition
ATC             All Things Considered
day             Day To Day
newsnotes       News And Notes
totn            Talk Of The Nation
fa              Fresh Air
wesat           Weekend Edition Saturday
wesun           Weekend Edition Sunday
fool            The Motley Fool

Segment is specified as a two digit number --specifying a blank value
plays entire program."
 #'(lambda (url)
     (funcall emacspeak-media-player url 'play-list)
     (emacspeak-w3-browse-url-with-style
      (expand-file-name "smil-anchors.xsl"
                        emacspeak-xslt-directory)
      url)))

(emacspeak-url-template-define
 "All Things Considered Stream from NPR"

 "http://www.npr.org/dmg/dmg.php?prgCode=ATC&showDate=%s&segNum=&mediaPref=RM"
 (list
  #'(lambda ()
      (emacspeak-url-template-collect-date "Date:"
                                           "%d-%b-%Y")))
 nil
 "Play NPR All Things Considered stream."
 #'(lambda (url)
     (funcall emacspeak-media-player url 'play-list)))

(emacspeak-url-template-define
 "Talk Of The Nation  Stream from NPR"
 "http://www.npr.org/ramfiles/totn/%s.totn.ram"
 (list 'emacspeak-url-template-date-YearMonthDate)
 nil
 "Play NPR Talk Of The Nation  stream."
 #'(lambda (url)
     (funcall emacspeak-media-player url 'play-list)))

(emacspeak-url-template-define
 "Morning Edition Stream from NPR"
 "http://www.npr.org/dmg/dmg.php?prgCode=ME&showDate=%s&segNum=&mediaPref=RM"
 (list
  #'(lambda ()
      (emacspeak-url-template-collect-date "Date:"
                                           "%d-%b-%Y")))
 nil
 "Play NPR Morning Edition  stream."
 #'(lambda (url)
     (funcall emacspeak-media-player url 'play-list)))

(emacspeak-url-template-define
 "Motley Fool Radio from NPR"
 "http://www.npr.org/dmg/dmg.php?prgCode=FOOL&showDate=%s&segNum=&mediaPref=RM"
 (list
  #'(lambda ()
      (emacspeak-url-template-collect-date "Date:"
                                           "%d-%b-%Y")))
 nil
 "Play NPR Motley Fool   stream."
 #'(lambda (url)
     (funcall emacspeak-media-player url 'play-list)))

(emacspeak-url-template-define
 "Talk Of The Nation from NPR"
 "rtsp://audio.npr.org/totn/%s_totn_%s.rm"
 (list
  'emacspeak-url-template-date-YearMonthDate
  "Segment: ")
 nil
 "Play NPR Talk Of The Nation segment."
 #'(lambda (url)
     (funcall emacspeak-media-player url 'play-list)))

(emacspeak-url-template-define
 "All Things Considered from NPR"
 "rtsp://audio.npr.org/atc/%s_atc_%s.rm"
 (list
  'emacspeak-url-template-date-YearMonthDate
  "Segment: ")
 nil
 "Play All Things Considered segment."
 #'(lambda (url)
     (funcall emacspeak-media-player url 'play-list)))

(emacspeak-url-template-define
 "Morning Edition from NPR"
 "rtsp://audio.npr.org/me/%s_me_%s.rm"
 (list
  'emacspeak-url-template-date-YearMonthDate
  "Segment:")
 nil
 "Play Morning Edition segment."
 #'(lambda (url)
     (funcall emacspeak-media-player url 'play-list)))

;;}}}
;;{{{  The Linux Show
(emacspeak-url-template-define
 "Geek Linux Daily"
 "http://thelinuxdaily.com/shows/%s.m3u"
 (list
  #'(lambda ()
      (emacspeak-url-template-collect-date "Date:"
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
             (emacspeak-url-template-collect-date
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
;;{{{ technet cast from DDJ

(emacspeak-url-template-define
 "DDJ TechNetCast Save"
 "http://technetcast.ddj.com/tnc_save_mp3.html?stream_id=%s"
 (list "Download Stream ")
 nil
 "Browse to a specified DDJ Technetcast stream and save  it.")

(emacspeak-url-template-define
 "DDJ TechNetCast Play"
 "http://technetcast.ddj.com/tnc_play.m3u?stream_id=%s"
 (list "Stream Id")
 nil
 "Play Technetcast stream from DDJ."
 #'(lambda (url)
     (funcall emacspeak-media-player  url 'play-list)))

;;}}}
;;{{{  linux today

(emacspeak-url-template-define
 "Linux Today News"
 "http://www.linuxtoday.com/"
 nil
 nil
 "Get news column from Linux Today."
 #'(lambda (url)
     (emacspeak-w3-xslt-filter
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
 "mlb standings"
 "http://midatlantic.comcastsportsnet.com/apfeed/sportstats/BBN/STND/mlb-standings.xml"
 nil
 nil
 "Get XML feed containing team standings."
 #'(lambda (url)
     (let ((buffer (get-buffer-create "MLB Standings")))
       (save-excursion
         (set-buffer  buffer)
         (erase-buffer)
         (kill-all-local-variables)
         (shell-command
          (format "xsltproc  %s %s 2>/dev/null"
                  "http://midatlantic.comcastsportsnet.com/baseball-standings.xsl "
                  url)
          (current-buffer)))
       (browse-url-of-buffer buffer)
       (goto-char (point-min))
       (emacspeak-speak-mode-line))))

(emacspeak-url-template-define
 "MLB Scorecard"
                                        ;"http://gd.mlb.com/components/game/mlb/%s/master_scoreboard.xml"
 "http://gd.mlb.com/components/game/mlb/%s/scoreboard.xml"
 (list
  #'(lambda nil
      (let ((date
             (emacspeak-url-template-collect-date
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
     (emacspeak-w3-browse-xml-url-with-style
      (expand-file-name "mlb-scorecard.xsl" emacspeak-xslt-directory)
      url)))

(emacspeak-url-template-define
 "Baseball standings"
 "http://www.mlb.com/NASApp/mlb/mlb/standings/index.jsp"
 nil
 nil
 "Display MLB standings."
 #'(lambda (url)
     (emacspeak-w3-extract-table-by-match
      "Standings"
      url 'speak)))

(emacspeak-url-template-define
 "Baseball Game Index"
                                        ;"http://gd.mlb.com/components/game/%s"
 "http://gd.mlb.com/components/game/mlb/%s/"
 (list
  #'(lambda nil
      (let ((date
             (emacspeak-url-template-collect-date "Date: "
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
             (emacspeak-url-template-collect-date
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
             (emacspeak-url-template-collect-date
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
;;{{{  Virtually There --Sabre Trip Reports
;; (emacspeak-url-template-define
;;  "Sabre Travel From Virtually There"
;;  "https://www.virtuallythere.com/new/printerFriendly.html?pnr=%s&name=%s&style=3&language=0&clocktype=12&host=1W&emailAddr=%s"
;;  (list
;;   #'(lambda nil
;;       (read-from-minibuffer "Record Locator: "))
;;   #'(lambda nil
;;       (read-from-minibuffer "User Name"))
;;   #'(lambda nil
;;       (read-from-minibuffer "Email: ")))
;;  nil
;;  "Display Trip Details"
;;                                         ; #'(lambda (url)
;;                                         ;      (let ((temp-file (format "/tmp/sabre-%s.html" (gensym))))
;;                                         ;        (shell-command
;;                                         ;         (format "lynx -base '%s' -source '%s' > %s"
;;                                         ;                 url url temp-file))
;;                                         ;        (w3-open-local temp-file)
;;                                         ;        (delete-file temp-file)))
;;  )

;;}}}
;;{{{ flights from travelocity

(emacspeak-url-template-define
 "Travelocity Lookup"
 "http://dps1.travelocity.com/dparflifo.ctl?Service=TRAVELOCITY&GUEST=Y&last_pgd_page=ushpdeparr.pgd&aln_name=%s&flt_num=%s"
 (list
  "Airline:"
  "Flight Number:")
 nil
 "Show arrival/departure information from Travelocity."
 #'(lambda (url)
     (emacspeak-w3-extract-table-by-match
      "Schedule" url 'speak)))

;;}}}
;;{{{  viewtrip --travel reports
(emacspeak-url-template-define
 "Travel itenerary from ViewTrip.com"
 "https://www.viewtrip.com/vt.asp"
 nil
 nil
 "Display Trip Details"
 #'(lambda (url)
     (let ((pnr (read-from-minibuffer "Record locator: "))
           (name (read-from-minibuffer "Last name: ")))
       (emacspeak-websearch-do-post "POST"
                                    url
                                    (format "rloc=%s&lastname=%s"
                                            pnr name)))))

;;}}}
;;{{{  times of india

;;; create url rewrite url to get print page
(emacspeak-url-template-define
 "Times Of India"
 "http://www.timesofindia.com"
 nil
 #'(lambda ()
     (declare (special emacspeak-w3-url-rewrite-rule))
     (setq emacspeak-w3-url-rewrite-rule
           (list "$" "&prtPage=1")))
 "Retrieve Times Of India.
Set up URL rewrite rule to get print page."
 )

(emacspeak-url-template-define
 "Cartoon You Said It By Laxman"
 "http://www1.indiatimes.com/cartoon/%scart%s.htm"
 (list
  (lambda ()
    (read-from-minibuffer "Month: "
                          (downcase
                           (format-time-string "%h"))))
  (lambda ()
    (read-from-minibuffer "Date: "
                          (format-time-string "%d"))))
 'emacspeak-speak-buffer
 "Retrieve Cartoon Times Of India."
 )

;;}}}
;;{{{ meerkat
(defvar emacspeak-url-template-meerkat-profiles (make-hash-table
                                                 :test #'equal)
  "Map Meerkat profile names to profile codes.
Meerkat realy needs an xml-rpc method for getting this.")

(loop for e in
      (list
       '(1 "DEFAULT")
       '(10586 ".NET")
       '(1064 "Perl")
       '(1065 "Macintosh")
       '(13886 "WebServices.XML.com")
       '(1398 "O'ReillyNetForums")
       '(1665 "BSD")
       '(17 "Python")
       '(1746 "PHP")
       '(19 "Software")
       '(2281 "XML")
       '(27 "Web")
       '(4 "Apache")
       '(441 "Mozilla")
       '(4862 "P2P")
       '(4999 "O'Reilly")
       '(5 "Linux")
       '(563 "O'ReillyNetwork")
       '(6304 "XML.com")
       '(760 "Weblogs")
       '(9 "Wireless"))
      do
      (puthash (second e)  (first e)
               emacspeak-url-template-meerkat-profiles))

(defsubst emacspeak-url-template-meerkat-profile-names ()
  "Return alist of Meerkat profile names."
  (declare (special emacspeak-url-template-meerkat-profiles))
  (loop for k being the hash-keys of
        emacspeak-url-template-meerkat-profiles
        collect (cons k k )))

(emacspeak-url-template-define
 "Meerkat Profile"
 "http://meerkat.oreillynet.com/?_fl=rss10&p=%s"
 (list
  #'(lambda nil
      (let((completion-ignore-case t)
           (profile
            (completing-read "Meerkat Profile:"
                             (emacspeak-url-template-meerkat-profile-names)
                             nil t)))
        (gethash profile emacspeak-url-template-meerkat-profiles))))
 nil
 "Meerkat Profile"
 #'(lambda (url)
     (emacspeak-rss-display url 'speak)))

(emacspeak-url-template-define
 "Meerkat Recipe"
 "http://meerkat.oreillynet.com/?_fl=rss10&%s"
 (list
  #'(lambda nil
      (read-from-minibuffer "Meerkat recipe: ")))
 nil
 "Meerkat tool"
 #'(lambda (url)
     (emacspeak-rss-display url 'speak)))

;;}}}
;;{{{  flight arrival
(emacspeak-url-template-define
 "Flight Tracker"
 "http://tracker.flightview.com/fvAirwise/fvCPL.exe?qtype=htm&AL=%s&acid=%s&FIND1=Find+flight"
 (list "Airline: " "Flight number: ")
 #'(lambda nil
     (search-forward "Airline: " nil t)
     (emacspeak-speak-line))
 "Display flight arrival and departure information.")

;;}}}
;;{{{ weather underground
;;;###autoload
(defcustom emacspeak-url-template-weather-city-state
  "CA/San_Jose"
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
 #'(lambda (url)
     (emacspeak-rss-display url 'speak)))

(emacspeak-url-template-define
 "Weather forecast from Weather Underground"
 "http://mobile.wunderground.com/cgi-bin/findweather/getForecast?query=%s"
 (list "Zip: ")
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
     (emacspeak-w3-extract-table-by-match "Status"
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

;;}}}
;;{{{ Interactive commands

;;;###autoload
(defun emacspeak-url-template-open (ut)
  "Fetch resource identified by URL template."
  (declare (special  emacspeak-w3-post-process-hook))
  (let ((fetcher (or (emacspeak-url-template-fetcher ut)
                     'browse-url))
        (url (emacspeak-url-template-url ut)))
    (when (and (emacspeak-url-template-post-action ut)
               (or (emacspeak-url-template-fetcher ut)
                   (eq browse-url-browser-function 'w3-fetch)
                   (eq browse-url-browser-function 'browse-url-w3)))
      (add-hook 'emacspeak-w3-post-process-hook
                (emacspeak-url-template-post-action ut))
      (add-hook 'emacspeak-w3-post-process-hook
                #'(lambda ()
                    (declare (special emacspeak-w3-post-process-hook
                                      emacspeak-url-template-current-ut))
                    (rename-buffer
                     (downcase
                      (mapconcat #'identity
                                 (or
                                  emacspeak-url-template-current-ut
                                  (list "Untitled"))
                                 ": "))
                     'unique)
                    (setq emacspeak-w3-post-process-hook nil))))
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
    (save-excursion
      (set-buffer standard-output)
      (fill-region (point-min)
                   (point-max)))
    (print-help-return-message))
  (emacspeak-speak-help)
  (emacspeak-auditory-icon 'help))

(defvar emacspeak-url-template-current-ut nil
  "Records name and args of URL template we're executing.")

;;;###autoload
(defun emacspeak-url-template-fetch (&optional documentation)
  "Fetch a pre-defined resource.
Use Emacs completion to obtain a list of available resources.
Resources typically prompt for the relevant information
before completing the request.
Optional interactive prefix arg displays documentation for specified resource."
  (interactive "P")
  (declare (special emacspeak-url-template-name-alist
                    emacspeak-url-template-current-ut
                    emacspeak-speak-messages))
  (let ((completion-ignore-case t)
        (emacspeak-speak-messages nil)
        (name  nil))
    (setq name
          (completing-read "Resource: "
                           emacspeak-url-template-name-alist
                           nil
                           'must-match))
    (cond
     (documentation (emacspeak-url-template-help-internal name))
     (t
      (emacspeak-url-template-open
       (emacspeak-url-template-get name))
      (emacspeak-auditory-icon 'open-object)))))

(defun emacspeak-url-template-help ()
  "Display documentation for  a URL template.
Use Emacs completion to obtain a list of available
resources."
  (interactive)
  (declare (special emacspeak-url-template-name-alist))
  (let ((completion-ignore-case t)
        (name nil))
    (setq name
          (completing-read "Resource: "
                           emacspeak-url-template-name-alist))
    (emacspeak-url-template-help-internal  name)))

;;}}}
;;{{{ Generate texinfo documentation for all defined url

(defun emacspeak-url-template-generate-texinfo-documentation (buffer)
  "Generates texinfo section documenting all defined URL
  templates."
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

As an example, the URL templates that enable access to NPR media
streams prompt for a program id and date, and automatically
launch the realmedia player after fetching the resource.\n\n"
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
