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

;;; Copyright (C) 1995 -- 2003, T. V. Raman<raman@cs.cornell.edu>
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
;;; The user provides values for the parameterized portons
;;; of the URL e.g. the date.

;;; Code:

;;}}}
;;{{{ required modules

(require 'emacspeak-preamble)
(require 'webjump)
(eval-when-compile (require 'emacspeak-w3))
;;}}}
;;{{{  structures 

(defstruct (emacspeak-url-template
            (:constructor emacspeak-url-template-constructor))
  name                                  ;Human-readable name
  template                              ;template URL string 
  generators                            ; list of param generator
  post-action			      ;action to perform after opening
  documentation                         ;resource  documentation
  fetcher                               ; custom fetcher 
  )

;;}}}
;;{{{ Helpers

(defun emacspeak-url-template-url (ut)
  "Instantiate URL identified by URL template."
  (apply 'format
         ( emacspeak-url-template-template ut)
         (mapcar
          (function
           (lambda (g)
             (cond
              ((stringp g)
               (webjump-url-encode (read-from-minibuffer g)))
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
                                           documentation fetcher)
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
"
  (declare (special emacspeak-url-template-table
                    emacspeak-url-template-name-alist))
  (unless (emacspeak-url-template-get  name)
    (push (list name name )
          emacspeak-url-template-name-alist))
  (emacspeak-url-template-set
   name
   (emacspeak-url-template-constructor :name name
                                       :template template
                                       :generators generators
                                       :post-action  post-action
                                       :documentation
                                       documentation
                                       :fetcher fetcher)))
                               
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
 "Bookshare Login"
 #'(lambda (url)
     (emacspeak-lynx url)))

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
        (webjump-url-encode query))))
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
 "http://uptime.netcraft.com/up/graph/?mode_u=off&mode_w=on&site=%s&submit=Examine "
 (list "Site to analyze: ")
 nil
 "Analyze WWW site using Netcraft.")
;;}}}
;;{{{ bbc 
(emacspeak-url-template-define
 "BBC 7 Schedule"
 "http://www.bbc.co.uk/bbc7/listings/index.shtml?%s"
 (list
  #'(lambda nil
      (read-from-minibuffer"Day:"
                           "Today")))
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
     (emacspeak-realaudio-play url)))

(emacspeak-url-template-define
 "BBC Radio7 On Demand"
 "rtsp://rmv8.bbc.net.uk/bbc7/%s.ra"
 (list "WeekdayTime: ")
 nil
 "Specify a week day (three letters -- lower case -- and a time spec
-- e.g. 1230 --
to play a BBC Radio7 program on demand."
 #'(lambda (url)
     (emacspeak-realaudio-play url)))

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

(emacspeak-url-template-define
 "BBC Programs On Demand"
 "http://www.bbc.co.uk/radio/aod/rpms/%s.rpm"
 (list "BBC Program: ")
 nil
 "Play BBC programs on demand."
 'emacspeak-realaudio-play)

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
;;{{{ google translation service
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
 (list "Google search:"
       #'(lambda nil
           (declare (special  emacspeak-websearch-google-number-of-results))
           emacspeak-websearch-google-number-of-results))
 #'(lambda nil
     (emacspeak-auditory-icon 'open-object))
 "Only show Google hits."
 #'(lambda (url)
     (declare (special emacspeak-xslt-directory))
     (emacspeak-w3-browse-url-with-style
      (expand-file-name "google-hits.xsl"
                        emacspeak-xslt-directory)
      url)))

;;}}}
;;{{{  cnet news 

(emacspeak-url-template-define
 "Tech News From CNet"
 "http://rss.com.com/2547-12-0-20.xml"
 nil
 #'(lambda nil
     (declare (special emacspeak-w3-url-rewrite-rule))
     (setq emacspeak-w3-url-rewrite-rule
	   (list "feed" "st_util__print"))
     (emacspeak-speak-buffer))
 "Display tech news from CNET"
 #'(lambda (url)
     (emacspeak-rss-display url))) 

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
;;{{{ google OverviewOfNews 

(emacspeak-url-template-define
 "Google Headline News"
 "http://www.google.com/news/newsheadlines.html"
 nil
 #'(lambda nil
     (search-forward "Top Headlines")
     (emacspeak-speak-rest-of-buffer))
 "Retrieve and speak Google News Overview.")

(emacspeak-url-template-define
 "Google News Search"
 "http://news.google.com/news?hl=en&q=%s&scoring=d&btnG=Google+Search"
 (list "Search news for: ")
 #'(lambda nil
     (search-forward "Sorted by")
     (forward-line 4)
     (emacspeak-speak-line))
 "Search Google news."
 #'(lambda (url)
     (emacspeak-w3-without-xsl
      (browse-url url))))

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
     (emacspeak-w3-extract-table-by-match "DIRECTIONS"
                                          url 'speak)))

;;}}}
;;{{{ yahoo daily news 
(emacspeak-url-template-define
 "Yahoo RSS Feeds"
 "http://news.yahoo.com/rss"
 nil
 nil
 "List Yahoo RSS Feeds."
 #'(lambda (url)
     (emacspeak-w3-xslt-filter
      "//a[contains(@href,\"rss\")]"
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
   "(//*[@class=\"article\"])//td[1]"
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
 "pdf2html"
 "http://access.adobe.com/perl/convertPDF.pl?url=%s"
 (list "PDF URL: ")
 nil
 "Use access.adobe.com to  convert a remote PDF document to
HTML.
The PDF document needs to be available on the public Internet.")

;;}}}
;;{{{ oasis 
(emacspeak-url-template-define
 "OASIS  Lists"
 "http://lists.oasis-open.org/archives/%s/%s/maillist.html"
 (list "OASIS Group: "
       #'(lambda ()
	   (read-from-minibuffer  "YearMonth: "
				  (format-time-string "%Y%m")
				  nil nil
				  (format-time-string "%Y%m"))))
 "Use this to pull up the
archived  mail from the OASIS list. You need to know the exact name of the list.")

;;}}}
;;{{{ w3c 

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
  (read-from-minibuffer "Date range: "
			(format-time-string "%Y%h"
					    (current-time))))

;;}}}
;;{{{ cnn 

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
     (add-hook 'emacspeak-w3-post-process-hook
               #'(lambda ()
                   (declare (special emacspeak-w3-class-filter))
                   (setq emacspeak-w3-class-filter "cnnStoryContent")))
     (emacspeak-w3-extract-by-class-list
      (list "cnnMainT1"
            "cnnMainNewT2"
            "cnnMainSections")
      url
      'speak)))

;;}}}
;;{{{ pbs --pulpit 
(emacspeak-url-template-define
 "Pulpit --- I Cringely"
 "http://www.pbs.org/cringely/pulpit/pulpit%s.html"
 (list
  #'(lambda nil
      (read-from-minibuffer "Date:"
                            (format-time-string "%Y%m%d"))))
 nil
 "Read pulpit from PBS."
 #'(lambda (url)
     (emacspeak-w3-xslt-filter
      "//p" url 'speak)))

;;}}}
;;{{{  NPR programs 

(emacspeak-url-template-define
 "American Life On Demand."
 "http://www.wbez.org/ta/%s.rm"
 (list "Episode: ")
 nil
 "Play This American Life  shows on demand."
 'emacspeak-realaudio-play)

(emacspeak-url-template-define
 "NPR On Demand"
 "http://www.npr.org/dmg/dmg.php?prgCode=%s&showDate=%s&segNum=%s&mediaPref=RM"
 (list
  #'(lambda ()
      (upcase (read-from-minibuffer "Program code:")))
  #'(lambda ()
      (read-from-minibuffer "Date:"
                            (format-time-string "%d-%b-%Y")))
  "Segment:")
 nil
 "Play NPR shows on demand.
Program is specified as a program code:

ME Morning Edition
ATC All Things Considered

Segment is specified as a two digit number --specifying a blank value
plays entire program."
 'emacspeak-realaudio-play)

(emacspeak-url-template-define
 "All Things Considered Stream from NPR"
 
 "http://www.npr.org/dmg/dmg.php?prgCode=ATC&showDate=%s&segNum=&mediaPref=RM"
 (list
  #'(lambda ()
      (read-from-minibuffer "Date:"
                            (format-time-string "%d-%b-%Y"))))
 nil
 "Play NPR All Things Considered stream."
 'emacspeak-realaudio-play)

(emacspeak-url-template-define
 "Talk Of The Nation  Stream from NPR"
 "http://www.npr.org/ramfiles/totn/%s.totn.ram"
 (list 'emacspeak-url-template-date-YearMonthDate)
 nil
 "Play NPR Talk Of The Nation  stream."
 'emacspeak-realaudio-play)
(emacspeak-url-template-define
 "Morning Edition Stream from NPR"
 "http://www.npr.org/dmg/dmg.php?prgCode=ME&showDate=%s&segNum=&mediaPref=RM"
 (list
  #'(lambda ()
      (read-from-minibuffer "Date:"
                            (format-time-string "%d-%b-%Y"))))
 nil
 "Play NPR Morning Edition  stream."
 'emacspeak-realaudio-play)

(emacspeak-url-template-define
 "Motley Fool Radio from NPR"
 "http://www.npr.org/dmg/dmg.php?prgCode=FOOL&showDate=%s&segNum=&mediaPref=RM"
 (list
  #'(lambda ()
      (read-from-minibuffer "Date:"
                            (format-time-string "%d-%b-%Y"))))
 nil
 "Play NPR Motley Fool   stream."
 'emacspeak-realaudio-play)

(emacspeak-url-template-define
 "Talk Of The Nation from NPR"
 "rtsp://audio.npr.org/totn/%s_totn_%s.rm"
 (list
  'emacspeak-url-template-date-YearMonthDate
  "Segment: ")
 nil
 "Play NPR Talk Of The Nation segment."
 'emacspeak-realaudio-play)

(emacspeak-url-template-define
 "All Things Considered from NPR" 
 "rtsp://audio.npr.org/atc/%s_atc_%s.rm"
 (list
  'emacspeak-url-template-date-YearMonthDate
  "Segment: ")
 nil
 "Play All Things Considered segment."
 'emacspeak-realaudio-play)

(emacspeak-url-template-define
 "Morning Edition from NPR" 
 "rtsp://audio.npr.org/me/%s_me_%s.rm" 
 (list
  'emacspeak-url-template-date-YearMonthDate
  "Segment:")
 nil
 "Play Morning Edition segment."
 'emacspeak-realaudio-play)

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
     (emacspeak-realaudio-play url)))

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
 "sourceforge Stats" 
 "http://sourceforge.net/project/stats/?group_id=%s"
 (list
  (lambda nil 
    (read-from-minibuffer "Project Id"
                          "2238")))
 nil
 "Display project usage statistics."
 #'(lambda (url)
     (emacspeak-w3-extract-tables-by-match-list
      (list "Date" "Lifespan")
      url)))

(emacspeak-url-template-define
 "sourceforge project" 
 "http://sourceforge.net/projects/%s"
 (list "Project name")
 nil
 "Open specified project page at SourceForge.")

(emacspeak-url-template-define
 "sourceforge browse download" 
 "http://prdownloads.sourceforge.net/%s"
 (list "Project name")
 nil
 "Retrieve download page at Sourceforge for specified project.")

(emacspeak-url-template-define
 "sourceforge download for North America" 
 "http://osdn.dl.sourceforge.net/sourceforge/%s/?C=M&O=A"
 (list "Project name")
 nil
 "Retrieve download page at Sourceforge for specified project.")

;;}}}
;;{{{  MLB scores
(emacspeak-url-template-define
 "MLB Scorecard"
 "http://gd.mlb.com/components/game/%s/scoreboard.xml"
 (list
  #'(lambda nil
      (let ((date 
             (read-from-minibuffer
              "Date: "
              (format-time-string "%Y-%m-%d")))
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
 "Baseball summary" 
 "http://www.mlb.com/NASApp/mlb/index.jsp?c_id=%s"
 (list
  #'(lambda nil
      (read-from-minibuffer  "Team Code: "
                             "sf")))
 nil
 "Display baseball team summary."
 #'(lambda (url)
     (emacspeak-w3-extract-table-by-match 
      "PCT"
      url 'speak)))
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
 "http://gd.mlb.com/components/game/%s"
 (list
  #'(lambda nil
      (let ((date 
             (read-from-minibuffer
              "Date: "
              (format-time-string "%Y-%m-%d")))
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
             (read-from-minibuffer
              "Date: "
              (format-time-string "%Y-%m-%d")))
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
 "http://gd.mlb.com/components/game/%s_%smlb_%smlb_1/boxscore.html"
 (list
  #'(lambda nil
      (let ((date 
             (read-from-minibuffer
              "Date: "
              (format-time-string "%Y-%m-%d")))
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

(emacspeak-url-template-define
 "Baseball Results"
 "http://gd.mlb.com/components/game/%s/gameLite.txt"
 (list
  #'(lambda nil
      (let ((date 
             (read-from-minibuffer
              "Date: "
              (format-time-string "%Y-%m-%d")))
            (fields nil)
            (result nil))
        (setq fields (split-string date "-"))
        (setq result 
              (format 
               "year_%s/month_%s/day_%s"
               (first fields)
               (second fields)
               (third fields)))
        result)))
 #'(lambda nil
     (ems-modify-buffer-safely
      (save-excursion
        (goto-char (point-min))
        (while (search-forward "<br>" nil t)
          (replace-match " " nil t))
        (goto-char (point-min))
        (while (search-forward "&" nil t)
          (replace-match "\n"))
        (flush-lines"|" (point-min) (point-max))
        (flush-lines "^ *$" (point-min) (point-max))
        (goto-char (point-min))
        (emacspeak-speak-line))))
 "Baseball results for a given date.")

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
		(emacspeak-url-template-post-action ut)))
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
;;;###autoload
(defun emacspeak-url-template-fetch (&optional documentation)
  "Fetch a pre-defined resource.
Use Emacs completion to obtain a list of available resources.
Resources typically prompt for the relevant information
before completing the request.
Optional interactive prefix arg displays documentation for specified resource."
  (interactive "P")
  (declare (special emacspeak-url-template-name-alist
		    emacspeak-speak-messages))
  (let ((completion-ignore-case t)
        (emacspeak-speak-messages nil)
        (name nil))
    (setq name (completing-read "Resource: "
                                emacspeak-url-template-name-alist
                                nil
                                'must-match))
    (cond
     (documentation (emacspeak-url-template-help-internal name))
     (t 
      (emacspeak-url-template-open
       (emacspeak-url-template-get
        name))
      (emacspeak-auditory-icon 'open-object)))))

(defun emacspeak-url-template-help ()
  "Display documentation for  a URL template.
Use Emacs completion to obtain a list of available
resources."
  (interactive)
  (declare (special emacspeak-url-template-table))
  (let ((completion-ignore-case t)
        (name nil)
        (table
         (loop for key being the hash-keys of
               emacspeak-url-template-table
               collect (list 
                        (format "%s" key)
                        (format "%s" key)))))
    (setq name
          (completing-read "Resource: "
                           table))
    (emacspeak-url-template-help-internal  name)))

;;}}}
(provide 'emacspeak-url-template)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
