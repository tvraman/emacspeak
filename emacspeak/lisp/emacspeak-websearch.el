;;; emacspeak-websearch.el --- search utilities
;;; $Id$
;;; $Author$
;;; Description:  Emacspeak extension to make Web searching convenient
;;; Keywords: Emacspeak, WWW interaction
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
;;;Copyright (C) 1995 -- 2000, T. V. Raman 
;;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
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
(require 'advice)
(require 'emacspeak-speak)
(require 'emacspeak-table-ui)
(require 'thingatpt)
(require 'voice-lock)
(require 'emacspeak-fix-interactive)
(require 'emacspeak-sounds)
(require 'webjump)
(require 'browse-url)
(when (locate-library "url-vars")
  (load-library "url-vars"))
(when (locate-library "w3")
  (require 'w3))
;;}}}
;;{{{  Introduction:

;;; Commentary:

;;; This module provides utility functions for searching the WWW

;;; Code:

;;}}}
;;{{{ let's use W3 if it is available:

(eval-when (load)
  (declaim (special browse-url-browser-function))
  (when (locate-library "w3")
    (setq browse-url-browser-function 
          'browse-url-w3)))

;;}}}
;;{{{ searcher table

(defvar emacspeak-websearch-table (make-hash-table)
  "Table holding mapping from search engine names to appropriate searcher functions.")

(defsubst emacspeak-websearch-set-searcher  (engine searcher)
  (declare (special emacspeak-websearch-table))
  (setf (gethash engine emacspeak-websearch-table) searcher))

(defsubst emacspeak-websearch-get-searcher (engine)
  (declare (special emacspeak-websearch-table))
  (gethash engine emacspeak-websearch-table))

;;}}}
;;{{{ Key table

(defvar emacspeak-websearch-keytable (make-hash-table)
  "Table holding mapping from keys to appropriate search engine names.")

(defsubst emacspeak-websearch-set-key  (key engine)
  (declare (special emacspeak-websearch-keytable))
  (setf (gethash key emacspeak-websearch-keytable) engine))

(defsubst emacspeak-websearch-get-engine (key)
  (declare (special emacspeak-websearch-keytable))
  (gethash key emacspeak-websearch-keytable))

;;}}}
;;{{{ top-level dispatch 

(defun emacspeak-websearch-dispatch  (&optional prefix)
  "Launches specific websearch queries.
Search engine is selected by keystrokes listed below.
Once selected, the selected searcher prompts for additional
information as appropriate.

Key     Engine              Search
a       AltaVista                   AltaVista Simple Search
Cap A     AllTheWeb                   All The Web
b       BBC                         BBC Archives
c       CNN                         CNN Interactive
Cap C   Company News            Lookup Company News By Ticker
d       dejanews                    Usenet Archives At Dejanews
Cap D   Dictionary                  Hypertext Websters Dictionary
e   Encyclopedia                    Encyclopedia Britannica
f       CNN-FN                      CNN  Financial Network
cap F   FolDoc                      Free Online Dictionary Of Computing
g       Google                      The Google WWW Index
h       HotBot                      HotBot WWW Index
i       Inference                   Inference WWW Search 
m       Map                         Driving directions from Yahoo
Cap M   Dictionary                  Merriam Webster Dictionary
n       news-yahoo                  News Wire at Yahoo
Cap N   NLight                      Northern Light Search
o       open-directory              Open Directory Search 
r       RedHat                      Search RedHat Via Google
Cap R   RFBD                        RFB&D Catalog search
p       Packages                    Locate Linux Packages
s       Software                  Search for software
u       usenet-altavista            Usenet Index At AltaVista
w       Weather                     Weather Channel  By Zip Code
cap W   W3C                         Search W3C Site
v   Vickers                         Vickers Insider Trades
Cap VVectorVestVectorVest Stock Reports
y       yahoo                       Master Yahoo Index

When using W3,  this interface attempts to speak the most relevant information on the result page."
  (interactive)
  (let ((engine nil)
        (searcher nil))
    (while (null engine)
      (setq engine
            (emacspeak-websearch-get-engine 
             (read-char 
              (concat "Websearch? "
                      (documentation this-command))))))
    (setq searcher (emacspeak-websearch-get-searcher engine))
    (if searcher 
        (call-interactively searcher)
      (error "I do not know how to search using %s" engine))))

;;}}}
;;{{{ helpers 

;;{{{ helpers to read the query 

(defvar emacspeak-websearch-history nil
  "Holds history of search queries.")

(defsubst emacspeak-websearch-read-query (prompt &optional
                                                 default
                                                 initial )
  (let ((answer 
         (read-from-minibuffer 
          prompt
          initial  nil nil
          (car emacspeak-websearch-history)
          (or default (word-at-point)))))
    (pushnew answer  emacspeak-websearch-history :test
             #'string-equal)
    answer))

;;}}}
;;{{{ post processer hook

(defsubst emacspeak-websearch-post-process (locator speaker &rest args)
  "Post processing steps on a result page.
LOCATOR is a string to search for in the results page.
SPEAKER is a function to call to speak relevant information.
ARGS specifies additional arguments to SPEAKER if any."
  (when (eq browse-url-browser-function 'browse-url-w3)
    (cond
     ((search-forward locator nil t)
      (recenter 0)
      (apply speaker args))
     (t (message "Your search appears to have ffailed.")))))

;;}}}

;;}}}
;;{{{ websearch utilities

;;{{{ AllTheWeb

(emacspeak-websearch-set-searcher 'alltheweb
                                  'emacspeak-websearch-alltheweb-search)
(emacspeak-websearch-set-key ?A  'alltheweb)

(defvar emacspeak-websearch-alltheweb-uri
  "http://www.alltheweb.com/cgi-bin/search?"
  "*URI for AllTheWeb search")

(defun emacspeak-websearch-alltheweb-search (query &optional prefix)
  "Perform an AllTheWeb  search.
Optional prefix arg prompts for type of search:
--use `all' `phrase' or  `any'
to specify the type of search."
  (interactive
   (list
    (emacspeak-websearch-read-query "All The Web Query: ")
    current-prefix-arg))
  (declare (special emacspeak-websearch-alltheweb-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-alltheweb-uri
             "query="
             (webjump-url-encode query)
             (and prefix
                  (format "&type=%s"
                          (read-from-minibuffer "Search type: "))))))
  (emacspeak-websearch-post-process
   "documents found"
   'emacspeak-speak-line))

;;}}}
;;{{{  altavista 

(emacspeak-websearch-set-searcher 'altavista
                                  'emacspeak-websearch-altavista-search)
(emacspeak-websearch-set-key ?a 'altavista)

(defvar emacspeak-websearch-altavista-uri 
  "http://ragingsearch.altavista.com/cgi-bin/query?search=Search&q="
  "URI for simple Altavista search")

(defun emacspeak-websearch-altavista-search (query)
  "Perform an Altavista search"
  (interactive
   (list (emacspeak-websearch-read-query "Altavista Query: ")))
  (declare (special emacspeak-websearch-altavista-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-altavista-uri
             "&q="
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   "Results"
  'emacspeak-speak-line))

;;}}}
;;{{{ altavista emacspeak archive

(defvar emacspeak-websearch-emacspeak-archive-uri
  "http://www.cs.vassar.edu/cgi-bin/wwwwais?selection=Emacspeak+E-list+Archive&sorttype=score&useicons=no&maxhits=40"
  "URI to search Emacspeak mail archive at Vassar.")

(defun emacspeak-websearch-emacspeak-archive (query)
  "Search Emacspeak mail archives.
For example to find messages about Redhat at the Emacspeak
archives, type +redhat"
  (interactive
   (list
    (emacspeak-websearch-read-query "Search for messages matching: ")))
  (declare (special emacspeak-websearch-emacspeak-archive-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-emacspeak-archive-uri
             "&keywords="
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   "Here is"
  'emacspeak-speak-line))

;;}}}
;;{{{ Computer Science Bibliography

(emacspeak-websearch-set-searcher 'biblio
                                  'emacspeak-websearch-biblio-search)

(emacspeak-websearch-set-key 2 'biblio)

(defvar emacspeak-websearch-biblio-uri
  "http://liinwww.ira.uka.de/searchbib/index?partial=on&case=on&results=citation&maxnum=200&query="
"URI to search the Computer Science Bibliographies.")

(defun emacspeak-websearch-biblio-search (query)
  "Search Computer Science Bibliographies."
  (interactive
   (list
    (emacspeak-websearch-read-query "Search CS Bibliographies  for: ")))
  (declare (special emacspeak-websearch-biblio-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-biblio-uri
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   query
   'emacspeak-speak-line))

;;}}}
;;{{{ bbc

(emacspeak-websearch-set-searcher 'bbc
                                  'emacspeak-websearch-bbc-search)

(emacspeak-websearch-set-key ?b 'bbc)

(defvar emacspeak-websearch-bbc-uri
  "http://search.bbc.co.uk/search/search.shtml?DB=all&TOPDOC=0&DB=all&P="
"URI to search the BBC archives.")

(defun emacspeak-websearch-bbc-search (query)
  "Search BBC archives."
  (interactive
   (list
    (emacspeak-websearch-read-query "Search BBC for: ")))
  (declare (special emacspeak-websearch-bbc-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-bbc-uri
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   "match"
   'w3-table-focus-on-this-cell))

;;}}}
;;{{{ CNN

(emacspeak-websearch-set-searcher 'cnn
                                  'emacspeak-websearch-cnn-search)
(emacspeak-websearch-set-key ?c 'cnn)

(defvar emacspeak-websearch-cnn-uri
  "http://search.cnn.com/query.html?qt="
  "*URI for launching a CNN Interactive  search.")

(defvar emacspeak-websearch-cnn-options
  "&col=cnni&lk=1&rf=1"
"*Additional default options to pass to CNN.")


(defun emacspeak-websearch-cnn-search (query &optional prefix)
  "Perform an CNN search.  
Optional interactive prefix arg
prompts for additional search parameters.  The default is to
sort by date and show summaries.  To sort by relevance
specify additional parameter &rf=0.  To hide summaries,
specify additional parameter &lk=2.
You can customize the defaults by setting variable
emacspeak-websearch-cnn-options to an appropriate string."
  (interactive
   (list
    (emacspeak-websearch-read-query "CNN Interactive Query:
")
    current-prefix-arg))
  (declare (special emacspeak-websearch-cnn-uri
                    emacspeak-websearch-cnn-options))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-cnn-uri
             (webjump-url-encode query)
             emacspeak-websearch-cnn-options
             (if prefix 
                 (read-from-minibuffer
                  "Additional query parameters: ")
               ""))))
  (emacspeak-websearch-post-process "Results"
                                    'w3-table-focus-on-this-cell)
  (emacspeak-websearch-post-process
   "Results"
   'emacspeak-speak-line))

;;}}}
;;{{{ CNN FN

(emacspeak-websearch-set-searcher 'cnn-fn
                                  'emacspeak-websearch-fn-cnn-search)
(emacspeak-websearch-set-key ?f 'cnn-fn)

(defvar emacspeak-websearch-fn-cnn-uri
  "http://search.cnnfn.com/query.html?qt="
  "*URI for launching a CNN FN  search.")

(defvar emacspeak-websearch-fn-cnn-options
  "&col=cnnfn&rq=0&qc=cnnfn&qm=1&st=1&nh=10&lk=1&rf=1&go=+seek+"
"*Additional default options to pass to CNN.")


(defun emacspeak-websearch-fn-cnn-search (query &optional prefix)
  "Perform an CNN FNsearch.  
Optional interactive prefix arg
prompts for additional search parameters.  The default is to
sort by date and show summaries.  To sort by relevance
specify additional parameter &rf=0.  To hide summaries,
specify additional parameter &lk=2.
You can customize the defaults by setting variable
emacspeak-websearch-fn-cnn-options to an appropriate string."
  (interactive
   (list
    (emacspeak-websearch-read-query "CNN FN Query: ")
    current-prefix-arg))
  (declare (special emacspeak-websearch-fn-cnn-uri
                    emacspeak-websearch-fn-cnn-options))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-fn-cnn-uri
             (webjump-url-encode query)
             emacspeak-websearch-fn-cnn-options
             (if prefix 
                 (read-from-minibuffer
                  "Additional query parameters: ")
               ""))))
  (emacspeak-websearch-post-process
   "Results"
  'emacspeak-speak-line))

;;}}}
;;{{{ FolDoc

(emacspeak-websearch-set-searcher 'foldoc
                                  'emacspeak-websearch-foldoc-search)
(emacspeak-websearch-set-key ?F 'foldoc)

(defvar emacspeak-websearch-foldoc-uri
  "http://wombat.doc.ic.ac.uk/foldoc/foldoc.cgi?action=Search&query="
  "*URI for launching a FolDoc  search.")

(defun emacspeak-websearch-foldoc-search (query)
  "Perform a FolDoc search. "
  (interactive
   (list
    (emacspeak-websearch-read-query "Computing Dictionary Query: ")))
  (declare (special emacspeak-websearch-foldoc-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-foldoc-uri
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   query
  'emacspeak-speak-line))

;;}}}
;;{{{ quotes from yahoo

(emacspeak-websearch-set-searcher 'quotes-yahoo
                                  'emacspeak-websearch-quotes-yahoo-search)

(emacspeak-websearch-set-key ?q 'quotes-yahoo)

(defvar emacspeak-websearch-quotes-yahoo-uri
  "http://finance.yahoo.com/q?o=t&s="
  "*URI for launching a Yahoo Quotes  search.")

(defvar emacspeak-websearch-quotes-csv-yahoo-uri
  "http://finance.yahoo.com/d/quotes.csv?f=sl1d1t1c1ohgv&s="
  "*URI for launching a Yahoo Quotes  search.")

(defvar emacspeak-websearch-quotes-yahoo-options
  "&d=v1"
"*Additional default options to pass to Yahoo.")

(defvar emacspeak-websearch-personal-portfolio nil
  "Set this to the stock tickers you want to check by
default.")
(defvar emacspeak-websearch-lynx-program "lynx"
  "Name of lynx executable")
(defun emacspeak-websearch-quotes-yahoo-search (query &optional prefix)
  "Perform a Quotes Yahoo .  
Default tickers to look up is taken from variable emacspeak-websearch-personal-portfolio.
Optional interactive prefix arg
causes data to be presented in an emacspeak table.
You can customize the defaults by setting variable
emacspeak-websearch-quotes-yahoo-options to an appropriate string."
  (interactive
   (list
    (emacspeak-websearch-read-query "Lookup quotes: "
                                    emacspeak-websearch-personal-portfolio
                                    emacspeak-websearch-personal-portfolio)
    current-prefix-arg))
  (declare (special emacspeak-websearch-quotes-yahoo-uri
                    emacspeak-websearch-quotes-yahoo-options
                    emacspeak-websearch-lynx-program
                    emacspeak-websearch-personal-portfolio
                    emacspeak-websearch-quotes-csv-yahoo-uri))
  (let ((url-be-asynchronous nil))
    (cond
     (prefix
      (let ((uri (concat emacspeak-websearch-quotes-csv-yahoo-uri
                         (webjump-url-encode (format "%s" query))))
            (results (format "*%s*" query))
            (process nil))
        (setq process
              (start-process   "lynx"
                               results 
                               emacspeak-websearch-lynx-program
                               "-dump"
                               uri))
        (set-process-sentinel process 'emacspeak-websearch-view-csv-data)))
     (t 
      (browse-url 
       (concat emacspeak-websearch-quotes-yahoo-uri
               (webjump-url-encode query)
               emacspeak-websearch-quotes-yahoo-options))
      (emacspeak-websearch-post-process
      "Symbol"
      'emacspeak-speak-line)))))

;;}}}
;;{{{ Lookup company news at Yahoo 

(emacspeak-websearch-set-searcher 'company-news
                                  'emacspeak-websearch-company-news)
(emacspeak-websearch-set-key ?C 'company-news)

(defvar emacspeak-websearch-company-news-uri
  "http://biz.yahoo.com/"
  "*URI for launching a company news lookup")

(defvar emacspeak-websearch-yahoo-charts-uri
  "http://chart.yahoo.com/t?"
  "*URI for locating historical chart data.")

(defvar emacspeak-websearch-yahoo-csv-charts-uri
  "http://chart.yahoo.com/table.csv?"
  "*URI for locating historical chart data.")

(defun emacspeak-websearch-company-news (ticker )
  "Perform an company news lookup.
Retrieves company news, research, profile, insider trades,  or upgrades/downgrades."
  (interactive
   (list
    (emacspeak-websearch-read-query
     "Enter stock ticker of company to lookup: ")))
  (declare (special emacspeak-websearch-company-news-uri))
  (let ((url-be-asynchronous nil)
        (type nil)
        (type-char
         (read-char
          "c Upgrades, h history, n news, r Research, p profile, t insider trades")))
    (setq type
          (case type-char
            (?r "z/a")
            (otherwise (format "%c" type-char))))
    (cond
     ((char-equal type-char ?H)
      (emacspeak-websearch-yahoo-historical-chart ticker))
     ((char-equal type-char ?h)
      (emacspeak-websearch-yahoo-historical-chart ticker
                                                  'csv)
      (emacspeak-auditory-icon 'select-object)
      (message "Fetching data --just a minute."))
     (t
      (browse-url 
       (concat emacspeak-websearch-company-news-uri
               (format "%s/" type)
               (format "%c/" (aref ticker 0))
               (format "%s.html" ticker)))
      (emacspeak-websearch-post-process
       (format-time-string "%Y")
       'emacspeak-speak-line)))))

(defun emacspeak-websearch-view-csv-data (process state )
  "Process csv data and put it in emacspeak table mode. "
(emacspeak-table-view-csv-buffer (process-buffer process)))

(defun emacspeak-websearch-yahoo-historical-chart (ticker
                                                   &optional data)
  "Look up historical stock data.
Optional second arg data processes the results as data rather than HTML."
  (interactive
   (list
    (emacspeak-websearch-read-query "Stock ticker:")
    current-prefix-arg))
  (declare (special emacspeak-websearch-lynx-program))
  (declare (special emacspeak-websearch-yahoo-charts-uri
                    emacspeak-websearch-yahoo-csv-charts-uri))
  (let (
        (start-month
         (read-from-minibuffer "Start Month: "
                               (format-time-string "%m")))
        (start-date
         (read-from-minibuffer "Start Date: "
                               (format-time-string  "%d")))
        (start-year
         (read-from-minibuffer "Start Year: "
                               (format-time-string "%y")))
        (end-month
         (read-from-minibuffer "End Month: "
                               (format-time-string "%m")))
        (end-date (read-from-minibuffer "End Date: "
                                        (format-time-string
                                         "%d")))
        (end-year
         (read-from-minibuffer "End Year: "
                               (format-time-string "%y")))
        (period
         (format "%c"
                 (read-char
                  "Daily: d Weekly: w Monthly: m"))))
    (cond
     ((eq data 'csv)
      (let ((uri (concat emacspeak-websearch-yahoo-csv-charts-uri
                         (format "a=%s" start-month)
                         (format "&b=%s" start-date)
                         (format "&c=%s" start-year)
                         (format "&d=%s" end-month)
                         (format "&e=%s" end-date)
                         (format "&f=%s" end-year)
                         (format "&g=%s" period)
                         (format "&s=%s" ticker)
                         "&q=q&x=.csv"))
            (results (format "*%s*" ticker))
            (process nil))
        (setq process
              (start-process   "lynx"
                               results 
                               emacspeak-websearch-lynx-program
                               "-dump"
                               uri))
        (set-process-sentinel process 'emacspeak-websearch-view-csv-data)))
     (t (browse-url 
         (concat emacspeak-websearch-yahoo-charts-uri
                 (format "a=%s" start-month)
                 (format "&b=%s" start-date)
                 (format "&c=%s" start-year)
                 (format "&d=%s" end-month)
                 (format "&e=%s" end-date)
                 (format "&f=%s" end-year)
                 (format "&g=%s" period)
                 (format "&s=%s" ticker)))
        (emacspeak-websearch-post-process
         "Open"
         'emacspeak-speak-line)))))

        

   
;;}}}
;;{{{  dejanews

(emacspeak-websearch-set-searcher 'dejanews
                                  'emacspeak-websearch-dejanews-search)

(emacspeak-websearch-set-key ?d 'dejanews)

(defvar emacspeak-websearch-dejanews-uri 
  "http://www.dejanews.com/[ST_rn=ps]/dnquery.xp?ST=PS&defaultOp=AND&DBS=1&format=terse&maxhits=100&LNG=english&QRY="
  "URI for Dejanews search")

(defun emacspeak-websearch-dejanews-search (query &optional prefix)
  "Perform a Dejanews search"
  (interactive
   (list
    (emacspeak-websearch-read-query "Dejanews Query: ")
    current-prefix-arg))
  (declare (special emacspeak-websearch-dejanews-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-dejanews-uri
             (webjump-url-encode query)
             (if prefix 
                 (read-from-minibuffer
                  "Additional query parameters: ")
               ""))))
  (emacspeak-websearch-post-process
   "Messages"
   'w3-table-speak-this-cell))

;;}}}
;;{{{ Webster

(emacspeak-websearch-set-searcher 'dictionary-hypertext-webster
                                  'emacspeak-websearch-dictionary-hypertext-webster-search)
(emacspeak-websearch-set-key ?D 'dictionary-hypertext-webster)

(defvar emacspeak-websearch-dictionary-hypertext-webster-uri 
"http://work.ucsd.edu:5141/cgi-bin/http_webster?isindex="
"URI for searching the hypertext Webster dictionary.")

(defun emacspeak-websearch-dictionary-hypertext-webster-search (query)
  "Search the Webster Dictionary."
  (interactive
   (list
    (emacspeak-websearch-read-query "Lookup word in Webster:")))
  (declare (special emacspeak-websearch-dictionary-hypertext-webster-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-dictionary-hypertext-webster-uri
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   query
  'emacspeak-speak-line))

;;}}}
;;{{{ source forge 

(emacspeak-websearch-set-searcher 'software
                                  'emacspeak-websearch-software-search)

(emacspeak-websearch-set-key ?s 'software)

(defvar emacspeak-websearch-sourceforge-search-uri 
"http://sourceforge.net/search/?type_of_search=soft&exact=1&forum_id=&is_bug_page=&group_id=&Search=Search&words="
"URI for searching the SourceForge site.")

(defun emacspeak-websearch-sourceforge-search (query)
  "Search SourceForge Site. "
  (interactive
   (list
    (emacspeak-websearch-read-query "Search SourceForge for: ")))
  (declare (special emacspeak-websearch-sourceforge-search-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-sourceforge-search-uri
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   query
  'w3-table-focus-on-this-cell))

(defvar emacspeak-websearch-freshmeat-search-uri 
"http://www.freshmeat.net/search.php3?query="
"URI for searching Freshmeat site. ")

(defun emacspeak-websearch-freshmeat-search (query)
  "Search Freshmeat  Site. "
  (interactive
   (list
    (emacspeak-websearch-read-query "Search Freshmeat  for: ")))
  (declare (special emacspeak-websearch-freshmeat-search-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-freshmeat-search-uri
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   query
  'emacspeak-speak-line))

(defvar emacspeak-websearch-appwatch-search-uri 
"http://appwatch.com/Linux/Users/find?q="
"URI for searching Freshmeat site. ")

(defun emacspeak-websearch-appwatch-search (query)
  "Search AppWatch  Site. "
  (interactive
   (list
    (emacspeak-websearch-read-query "Search AppWatch  for: ")))
  (declare (special emacspeak-websearch-appwatch-search-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-appwatch-search-uri
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   "Search results"
  'emacspeak-speak-line))



(defvar emacspeak-websearch-ctan-search-uri 
"http://theory.uwinnipeg.ca/mod_perl/ctan-search.pl?topic="
"URI for searching CTAN archives for tex and latex utilities. ")

(defun emacspeak-websearch-ctan-search (query)
  "Search CTAN Comprehensive TeX Archive Network   Site. "
  (interactive
   (list
    (emacspeak-websearch-read-query
     "Lookup Comprehensive TEX Archive for: ")))
  (declare (special emacspeak-websearch-ctan-search-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-ctan-search-uri
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   query
   'emacspeak-speak-line))



(defvar emacspeak-websearch-cpan-search-uri 
"http://search.cpan.org/search?mode=module&query="
"URI for searching CPAN  archives for perl modules . ")

(defun emacspeak-websearch-cpan-search (query)
  "Search CPAN  Comprehensive Perl Archive Network   Site. "
  (interactive
   (list
    (emacspeak-websearch-read-query
     "Locate PERL Module: ")))
  (declare (special emacspeak-websearch-cpan-search-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-cpan-search-uri
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   query
   'emacspeak-speak-line))




(defvar emacspeak-websearch-software-sites
"a AppWatch f FreshMeat p Perl s SourceForge t TEX "
"Sites searched for open source software. ")

;;; top level dispatcher for searching source locations 
(defun emacspeak-websearch-software-search  ()
  "Search SourceForge, Freshmeat and other sites. "
  (interactive)
  (declare (special emacspeak-websearch-software-sites))
  (let ((site
         (read-char emacspeak-websearch-software-sites)))
    (case site
      (?a (call-interactively 'emacspeak-websearch-appwatch-search))
      (?f (call-interactively
           'emacspeak-websearch-freshmeat-search))
      (?p (call-interactively 'emacspeak-websearch-cpan-search))
      (?s (call-interactively 'emacspeak-websearch-sourceforge-search))
      (?t (call-interactively 'emacspeak-websearch-ctan-search))
      (otherwise (message emacspeak-websearch-software-sites )))))


;;}}}
;;{{{  Encyclopeadia Britannica 

(emacspeak-websearch-set-searcher 'britannica
                                  'emacspeak-websearch-britannica-search)
(emacspeak-websearch-set-key ?e 'britannica)

(defvar emacspeak-websearch-britannica-uri 
"http://www.eb.com:180/bol/search?type=topic&I3.x=0&I3.y=0&DBase=Articles"
"URI for searching Britannica online.")

(defun emacspeak-websearch-britannica-search (query)
  "Search Encyclopedia Britannica."
  (interactive
   (list
    (emacspeak-websearch-read-query
     "Search Encyclopedia Britannica  for: ")))
  (declare (special emacspeak-websearch-britannica-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-britannica-uri
"&query="
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   "your search"
   'emacspeak-speak-line))

;;}}}
;;{{{ google

(emacspeak-websearch-set-searcher 'google
                                  'emacspeak-websearch-google)
(emacspeak-websearch-set-key ?g 'google)

(defvar emacspeak-websearch-google-uri
  "http://www.google.com/search?q="
  "*URI for Google search")

(defun emacspeak-websearch-google (query)
  "Perform an Google search"
  (interactive
   (list (emacspeak-websearch-read-query "Google Query: ")))
  (declare (special emacspeak-websearch-google-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-google-uri
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   "results"
   'emacspeak-speak-line))

;;}}}
;;{{{ hotbot

(emacspeak-websearch-set-searcher 'hotbot
                                  'emacspeak-websearch-hotbot-search)
(emacspeak-websearch-set-key ?h 'hotbot)

(defvar emacspeak-websearch-hotbot-uri 
"http://www.hotbot.com/?submit=SEARCH&_v=2"
"URI for searching Hotbot.")

(defvar emacspeak-websearch-hotbot-options 
"&SM=MC&DV=30&LG=english&DC=10&DE=2"
"*Search options for Hotbot search.
SM -- specify phrase search, title search etc.
DV -- specify how many days to cover
LG -- specify language 
DC -- Number of hits 
DE -- full description, summary or URLs only")


(defun emacspeak-websearch-hotbot-search (query &optional prefix)
  "Perform a Hotbot  search.  
Optional interactive prefix arg
prompts for additional search parameters.  The default is to
sort by date and show summaries.  To sort by relevance
specify additional parameter &rf=0.  To hide summaries,
specify additional parameter &lk=2.
You can customize the defaults by setting variable
emacspeak-websearch-hotbot-options to an appropriate string."
  (interactive
   (list
    (emacspeak-websearch-read-query "Hotbot Query: ")
    current-prefix-arg))
  (declare (special emacspeak-websearch-hotbot-uri
                    emacspeak-websearch-hotbot-options))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-hotbot-uri
             "&MT="
             (webjump-url-encode query)
             emacspeak-websearch-hotbot-options
             (if prefix 
                 (read-from-minibuffer
                  "Additional query parameters: ")
               ""))))
  (emacspeak-websearch-post-process
   "Web Matches"
  'w3-table-speak-this-cell))

;;}}}
;;{{{  Inference 

(emacspeak-websearch-set-searcher 'inference
                                  'emacspeak-websearch-inference-search)
(emacspeak-websearch-set-key ?i 'inference)

(defvar emacspeak-websearch-inference-uri 
  "http://www.infind.com/infind/infind.exe?query="
  "URI for Inference search")

(defun emacspeak-websearch-inference-search (query)
  "Perform an Inference search."
  (interactive
   (list (emacspeak-websearch-read-query "Inference Query: ")))
  (declare (special emacspeak-websearch-inference-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-inference-uri
             (webjump-url-encode query)))))

;;}}}
;;{{{  Ask Jeeves  

(emacspeak-websearch-set-searcher 'jeeves
                                  'emacspeak-websearch-ask-jeeves)
(emacspeak-websearch-set-key ?j 'jeeves)

(defvar emacspeak-websearch-jeeves-uri 
  "http://www.askjeeves.com/main/askJeeves.asp?site_name=Jeeves&metasearch=yes&ask="
  "URI for Ask Jeeves  search")

(defun emacspeak-websearch-ask-jeeves (query)
  "Ask Jeeves for the answer."
  (interactive
   (list (emacspeak-websearch-read-query "Ask Jeeves for: ")))
  (declare (special emacspeak-websearch-jeeves-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-jeeves-uri
             (webjump-url-encode query)))
    (emacspeak-websearch-post-process 
     "You asked"
     'emacspeak-speak-line)))

;;}}}
;;{{{ Driving directions from Yahoo

(emacspeak-websearch-set-searcher 'map-directions
                                  'emacspeak-websearch-map-directions-search)
(emacspeak-websearch-set-key ?m 'map-directions)

(defvar emacspeak-websearch-map-directions-uri
  "http://maps.yahoo.com/py/ddResults.py?Pyt=Tmap&doit=1&newname=&newdesc=&Get+Directions=Get+Directions"
  "URI for getting driving directions from Yahoo.")

(defvar emacspeak-websearch-map-maps-uri 
  "http://maps.yahoo.com/py/maps.py?Pyt=Tmap&Get%A0Map=Get+Map&"
"URI for obtaining location maps.")

(defsubst emacspeak-websearch-map-maps-get-location ()
  "Convenience function for prompting and constructing the route component."
  (concat 
   (format "&addr=%s"
           (webjump-url-encode 
            (read-from-minibuffer "Street Address: ")))
   (format "&csz=%s"
           (webjump-url-encode
            (read-from-minibuffer "City/State or Zip:")))))


(defsubst emacspeak-websearch-map-directions-get-locations ()
  "Convenience function for prompting and constructing the route component."
  (concat 
   (format "&newaddr=%s"
           (webjump-url-encode 
            (read-from-minibuffer "Start Address: ")))
   (format "&newcsz=%s"
           (webjump-url-encode
            (read-from-minibuffer "City/State or Zip:")))
   (format "&newtaddr=%s"
           (webjump-url-encode
            (read-from-minibuffer "Destination Address: ")))
   (format "&newtcsz=%s"
           (webjump-url-encode
            (read-from-minibuffer "City/State or Zip:")))))

(defun emacspeak-websearch-map-directions-search (query
                                                  &optional map)
  "Get driving directions from Yahoo.
With optional interactive prefix arg MAP shows the location map instead."
  (interactive
   (list 
    (if current-prefix-arg
        (emacspeak-websearch-map-maps-get-location)
      (emacspeak-websearch-map-directions-get-locations))
    current-prefix-arg))
  (declare (special emacspeak-websearch-map-directions-uri
                    emacspeak-websearch-map-maps-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat
      (if map
          emacspeak-websearch-map-maps-uri
        emacspeak-websearch-map-directions-uri)
      query))
    (if  map
        (emacspeak-websearch-post-process
         "Nearby"
         'emacspeak-speak-line)
      (emacspeak-websearch-post-process
       "Start out"
       'w3-table-speak-current-table-column))))

;;}}}
;;{{{  news yahoo

(emacspeak-websearch-set-searcher 'news-yahoo
                                  'emacspeak-websearch-news-yahoo)
(emacspeak-websearch-set-key ?n 'news-yahoo)

(defvar emacspeak-websearch-news-yahoo-uri
  "http://search.news.yahoo.com/search/news?n=30&p="
  "*URI for launching a Yahoo News search")

(defun emacspeak-websearch-news-yahoo (query)
  "Perform an Yahoo News search"
  (interactive
   (list (emacspeak-websearch-read-query "Yahoo News Query: ")))
  (declare (special emacspeak-websearch-news-yahoo-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-news-yahoo-uri
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   "Matches"
  'emacspeak-speak-line))

;;}}}
;;{{{  Northern Lights Search

(emacspeak-websearch-set-searcher 'northern-light
                                  'emacspeak-websearch-northern-light)

(emacspeak-websearch-set-key ?N 'northern-light)

(defvar emacspeak-websearch-northern-light-uri
  "http://www.northernlight.com/nlquery.fcg?cb=0&orl=2%3A1&qr="
  "*URI for launching a Northern Light  search")

(defun emacspeak-websearch-northern-light (query)
  "Perform a Northern Light  search"
  (interactive
   (list (emacspeak-websearch-read-query "Search Northern
Light for: ")))
  (declare (special emacspeak-websearch-northern-light-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-northern-light-uri
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   "Your search"
  'emacspeak-speak-line))

;;}}}
;;{{{  Open Directory

(emacspeak-websearch-set-searcher 'open-directory
                                  'emacspeak-websearch-open-directory-search)
(emacspeak-websearch-set-key ?o 'open-directory)


(defvar emacspeak-websearch-open-directory-uri
  "http://search.dmoz.org/cgi-bin/search?search="
  "*URI for launching a Open Directory search")

(defun emacspeak-websearch-open-directory-search (query)
  "Perform an Open Directory search"
  (interactive
   (list
    (emacspeak-websearch-read-query
     "Search Open Directory for: ")))
  (declare (special emacspeak-websearch-open-directory-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-open-directory-uri
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   "Search results"
  'emacspeak-speak-line))

;;}}}
;;{{{ RedHat (via google)

(emacspeak-websearch-set-searcher 'redhat
                                  'emacspeak-websearch-redhat)
(emacspeak-websearch-set-key ?r 'redhat)

(defvar emacspeak-websearch-redhat-uri
  "http://redhat.google.com/redhat?q="
  "*URI for RedHat Site search")

(defun emacspeak-websearch-redhat (query)
  "Search RedHat site."
  (interactive
   (list
    (emacspeak-websearch-read-query "Search RedHat Site for: ")))
  (declare (special emacspeak-websearch-redhat-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-redhat-uri
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   "matches"
   'emacspeak-speak-line))

;;}}}
;;{{{  RFB

(emacspeak-websearch-set-searcher 'rfb
                                  'emacspeak-websearch-rfb-search)
(emacspeak-websearch-set-key ?R 'rfb)

(defvar emacspeak-websearch-rfb-uri
"http://www.rfbd.org/catalog/search_process.asp"
"URI for searching RFB catalogues.")

(defun emacspeak-websearch-rfb-search (author title)
  "Search RFB&D catalog."
  (interactive
   (list
    (emacspeak-websearch-read-query "Author:")
    (emacspeak-websearch-read-query "Title:")))
  (declare (special emacspeak-websearch-rfb-uri))
  (let ((url-be-asynchronous nil))
    (emacspeak-websearch-do-post "POST" emacspeak-websearch-rfb-uri
			       (concat "author="
				       (webjump-url-encode author)
				       "&title="
				       (webjump-url-encode title))))
  (emacspeak-websearch-post-process
   "Number"
  'emacspeak-speak-line))

;;}}}
;;{{{ Linux Package Index

(emacspeak-websearch-set-searcher 'packages-linux
                                  'emacspeak-websearch-packages-linux)
(emacspeak-websearch-set-key ?p 'packages-linux)

(defvar emacspeak-websearch-packages-linux-uri
  "http://www.firstlinux.com/cgi-bin/package/search.cgi?query="
  "*URI for Linux Package  Site search")

(defun emacspeak-websearch-packages-linux (query)
  "Search for Linux packages."
  (interactive
   (list
    (emacspeak-websearch-read-query "Search Linux packagesfor: ")))
  (declare (special emacspeak-websearch-packages-linux-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-packages-linux-uri
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   "returned"
   'emacspeak-speak-line))

;;}}}
;;{{{ Merriam Webster

(emacspeak-websearch-set-searcher 'merriam-webster
                                  'emacspeak-websearch-merriam-webster-search)
(emacspeak-websearch-set-key ?M 'merriam-webster)

(defvar emacspeak-websearch-merriam-webster-uri 
"http://www.m-w.com/cgi-bin/dictionary?va="
"URI for searching the Merriam Webster dictionary.")

(defun emacspeak-websearch-merriam-webster-search (query)
  "Search the Merriam Webster Dictionary."
  (interactive
   (list
    (emacspeak-websearch-read-query "Lookup word in Webster:")))
  (declare (special emacspeak-websearch-merriam-webster-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-merriam-webster-uri
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   "Main Entry"
  'emacspeak-speak-line))

;;}}}
;;{{{  usenet altavista 

(emacspeak-websearch-set-searcher 'usenet-altavista
                                  'emacspeak-websearch-usenet-altavista-search)
(emacspeak-websearch-set-key ?u 'usenet-altavista)

(defvar emacspeak-websearch-usenet-altavista-uri 
  "http://www.altavista.com/cgi-bin/query?pg=q&what=news&KL=en&enc=iso88591&text=yes"
  "URI for Usenet  Altavista search")

(defun emacspeak-websearch-usenet-altavista-search (query)
  "Perform a Usenet  Altavista search"
  (interactive
   (list (emacspeak-websearch-read-query "Usenet Altavista Query: ")))
  (declare (special emacspeak-websearch-usenet-altavista-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-usenet-altavista-uri
             "&q="
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   "AltaVista found"
  'emacspeak-speak-line))

;;}}}
;;{{{ Vickers Insider Trading

(emacspeak-websearch-set-searcher 'vickers
                                  'emacspeak-websearch-vickers-search)
(emacspeak-websearch-set-key ?v 'vickers)

(defvar emacspeak-websearch-vickers-uri 
"http://www.tscn.com/Fortune/Vickers_Insider_Trading.html?Button=Get+Report&Symbol="
"URI for searching Vickers Insider Trading..")

(defun emacspeak-websearch-vickers-search (query)
  "Search Vickers insider trading."
  (interactive
   (list
    (emacspeak-websearch-read-query
     "Search Vickers  for: ")))
  (declare (special emacspeak-websearch-vickers-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-vickers-uri
             (webjump-url-encode
              (upcase query)))))
  (emacspeak-websearch-post-process
   "SEC"
   'emacspeak-speak-line))

;;}}}
;;{{{ VectorVest Reports

(emacspeak-websearch-set-searcher 'vector-vest
                                  'emacspeak-websearch-vector-vest-search)
(emacspeak-websearch-set-key ?V 'vector-vest)

(defvar emacspeak-websearch-vector-vest-uri 
"http://www.vectorvest.com/stockreport/eval.phtml?vvnumber=1068&symbol="
"URI for looking up VectorVest reports.")

(defun emacspeak-websearch-vector-vest-search (query)
  "Look up VectorVest reports ."
  (interactive
   (list
    (emacspeak-websearch-read-query
     "VectorVest Lookup Stock:")))
  (declare (special emacspeak-websearch-vector-vest-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-vector-vest-uri
             (webjump-url-encode
              query))))
  (emacspeak-websearch-post-process
   (upcase query)
   'emacspeak-speak-line))

;;}}}
;;{{{ Weather

(emacspeak-websearch-set-searcher 'weather
                                  'emacspeak-websearch-weather)
(emacspeak-websearch-set-key ?w 'weather)

(defvar emacspeak-websearch-weather-uri
  "http://www.weather.com/weather/us/zips/"
  "*URI for getting weather forecast.")

(defun emacspeak-websearch-weather (zip)
  "Get weather forecast for specified zip code."
  (interactive
   (list (emacspeak-websearch-read-query "Zip Code: ")))
  (declare (special emacspeak-websearch-weather-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-weather-uri
             zip
".html")))
  (emacspeak-websearch-post-process
   zip
  'w3-table-speak-this-cell))

;;}}}
;;{{{ W3C

(emacspeak-websearch-set-searcher 'w3c
                                  'emacspeak-websearch-w3c-search)
(emacspeak-websearch-set-key ?W 'w3c)

(defvar emacspeak-websearch-w3c-search-uri 
"http://search.w3.org/Member/cgi-bin/query?mss=simple&pg=q&what=web&filter=all&fmt=."
"URI for searching the member area of the W3C site.")

(defun emacspeak-websearch-w3c-search (query)
  "Search the W3C Site."
  (interactive
   (list (emacspeak-websearch-read-query "Search W3C site: ")))
  (declare (special emacspeak-websearch-w3c-search-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-w3c-search-uri
             "&q="
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   "match"
  'emacspeak-speak-line))

;;}}}
;;{{{ yahoo

(emacspeak-websearch-set-searcher 'yahoo
                                  'emacspeak-websearch-yahoo)
(emacspeak-websearch-set-key ?y 'yahoo)

(defvar emacspeak-websearch-yahoo-uri
  "http://search.yahoo.com/bin/search?p="
  "*URI for launching a Yahoo  search")

(defun emacspeak-websearch-yahoo (query)
  "Perform an Yahoo  search"
  (interactive
   (list (emacspeak-websearch-read-query "Yahoo Query: ")))
  (declare (special emacspeak-websearch-yahoo-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-yahoo-uri
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   "Web Pages"
   'emacspeak-speak-line))

;;}}}
;;; Load site-specific searchers 

(when (locate-library "emacspeak-w3search")
  (load-library "emacspeak-w3search"))

;;}}}
;;{{{ Browse usenet using Dejanews

(defvar emacspeak-dejanews-uri 
"http://x5.dejanews.com/[ST_rn=bg]/topics_bg.xp?search=topic&group="
"URI to open a group on Dejanews.")

(defun emacspeak-dejanews-browse-group (group &optional prefix)
  "Prompt and browse a newsgroup on Dejanews."
  (interactive
   (list
    (read-from-minibuffer "Newsgroup: ")
current-prefix-arg))
  (declare (special emacspeak-dejanews-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-dejanews-uri
             (if prefix 
                 (read-from-minibuffer
                  "Additional query parameters: ")
               "")
             (webjump-url-encode group)
))
    (search-forward group)
    (w3-table-speak-this-cell)))

(defun emacspeak-websearch-do-post (the-method the-url query &optional enctype)
  ;; Adapted from w3-submit-form
  (require 'w3)
  (let* ((enctype (or enctype "application/x-www-form-urlencoded")))
    (if (and (string= "GET" the-method)
	     (string-match "\\([^\\?]*\\)\\?" the-url))
	(setq the-url (url-match the-url 1)))
    (cond
     ((or (string= "POST" the-method)
	  (string= "PUT" the-method))
      (if (consp query)
	  (setq enctype (concat enctype "; boundary="
				(substring (car query) 2 nil)
				"")
		query (cdr query)))
      (let ((url-request-method the-method)
	    (url-request-data query)
	    (url-request-extra-headers
	     (cons (cons "Content-type" enctype) url-request-extra-headers)))
	(w3-fetch the-url)))
     ((string= "GET" the-method)
      (let ((the-url (concat the-url (if (string-match "gopher" enctype)
				       "" "?") query)))
	(w3-fetch the-url)))
     (t
      (w3-warn 'html (format "Unknown submit method: %s" the-method))
      (let ((the-url (concat the-url "?" query)))
	(w3-fetch the-url))))))

;;}}}
(provide 'emacspeak-websearch)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
