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
;;;Copyright (C) 1995 -- 2001, T. V. Raman 
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

(eval-when-compile (require 'cl))
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
Once selected, the selected searcher prompts for additional information as appropriate.

Key     Search
a       AltaVista  Search
Cap     AAll Web
b       BBC Archives
C-b     Computer Science Bibliography
c       CNN Interactive
Cap C   Company News By Ticker
C-c     Computer Science Citation  Index
d       Usenet Archives (formerly Dejanews)
Cap D   Hypertext Websters Dictionary
e       Encyclopedia Britannica
f       CNN  Financial Network
cap F   Free Online Dictionary Of Computing
g       The Google Search
.       Advanced Google Search
Cap G   Locate Gutenberg Etexts
h       HotBot WWW Index
j       Ask Jeeves
m       Maps from Yahoo
Cap M   Merriam Webster Dictionary
n       -yahoo                  News Wire at Yahoo
Cap N   Northern Light Search
o       Open Directory Search 
p       People Search (Yahoo)
r       RedHat Search Via Google
Cap r   RFBD Catalog search
C-r     Find RPM packages
s       Software  Search
t       Machine translation 
u       Usenet Index from Google
w       Weather Channel  By Zip Code
cap W   Search W3C Site
v       Vickers Insider Trades
Cap V   VectorVest Stock Reports
y       Master Yahoo Index

When using W3,  this interface attempts to speak the most relevant information on the result page."
  (interactive "P")
  (let ((engine nil)
        (saved-syntax-table (copy-syntax-table  (syntax-table)))
        (searcher nil))
    (unwind-protect
        (progn
          (modify-syntax-entry 10  ">")
          (while (null engine)
            (setq engine
                  (emacspeak-websearch-get-engine 
                   (read-char 
                    (concat "Websearch? "
                            (documentation this-command)))))))
      (set-syntax-table saved-syntax-table))
    (setq searcher (emacspeak-websearch-get-searcher engine))
    (if searcher
        (call-interactively searcher)
      (error "I do not know how to search using %s" engine))))

;;}}}
;;{{{ helpers 
(defun emacspeak-websearch-do-post (the-method the-url query
                                               &optional
                                               enctype)
  "Submit a post request. "
  (declare (special url-request-extra-headers))
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
      (message "Unknown submit method: %s" the-method)
      (let ((the-url (concat the-url "?" query)))
	(w3-fetch the-url))))))
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
"http://www.altavista.com/sites/search/res_text?sc=on&hl=on&amb=txt&kl=en&search=Search&q="
  "URI for simple Altavista search")

(defun emacspeak-websearch-altavista-search (query)
  "Perform an Altavista search"
  (interactive
   (list (emacspeak-websearch-read-query "Altavista Query: ")))
  (declare (special emacspeak-websearch-altavista-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-altavista-uri
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   "Results"
   'emacspeak-speak-line))

;;}}}
;;{{{ altavista emacspeak archive

(defvar emacspeak-websearch-emacspeak-archive-uri
  "http://www.cs.vassar.edu/cgi-bin/emacspeak-search"
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
    (emacspeak-websearch-do-post "POST"
      emacspeak-websearch-emacspeak-archive-uri
             (format "query=%s"
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
;;{{{ CiteSeer Citation index 

(defvar emacspeak-websearch-citeseer-uri 
  "http://citeseer.nj.nec.com/cs?"
  "URI for searching CiteSeer index. ")

(defvar emacspeak-websearch-citeseer-citation-options 
  "cs=1&submit=Search+Citations&cf=Any&co=Citations&cm=50"
  "* Options for performing a citation search on CiteSeer.")


(defvar emacspeak-websearch-citeseer-article-options 
  "cs=1&cf=Author&co=Citations&cm=50&submit=Search+Indexed+Articles&af=Any&ao=Citations&am=50"
  "* Options for performing an article search on CiteSeer. ")




(emacspeak-websearch-set-searcher 'citeseer

                                  'emacspeak-websearch-citeseer-search)

(emacspeak-websearch-set-key 3 'citeseer)
(defun emacspeak-websearch-citeseer-search(term )
  "Perform a CiteSeer search. "
  (interactive
   (list
    (emacspeak-websearch-read-query
     "Enter CiteSeer query term:")))
  (declare (special emacspeak-websearch-citeseer-uri
                    emacspeak-websearch-citeseer-citation-options
                    emacspeak-websearch-citeseer-article-options))
  (let ((url-be-asynchronous nil)
        (options nil)
        (type-char
         (read-char
          "a Articles c Citations")))
    (setq options
          (case type-char
            (?a
             emacspeak-websearch-citeseer-article-options)
            (?c emacspeak-websearch-citeseer-citation-options)))
    (browse-url 
     (concat emacspeak-websearch-citeseer-uri
             "q="
             (webjump-url-encode term)
             "&"
             options))
    (cond
     ((char-equal type-char ?a)
      (emacspeak-websearch-post-process "documents found"
                                        'emacspeak-speak-line))
     ((char-equal ?c type-char)
      (emacspeak-websearch-post-process "citations found" 'emacspeak-speak-line)))))

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
Default tickers to look up is taken from variable
emacspeak-websearch-personal-portfolio.
Default is to present the data in emacspeak's table browsing
mode --optional interactive prefix arg
causes data to be displayed y W3 as a WWW page.
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
     ((null prefix)
      (let ((uri (concat emacspeak-websearch-quotes-csv-yahoo-uri
                         (webjump-url-encode (format "%s" query))))
            (results "*quotes-table*")
            (process nil))
        ;;; nuke old results if any
        (when (get-buffer results )
          (kill-buffer results))
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

(defvar emacspeak-websearch-yahoo-company-news-quotes-uri 
"http://finance.yahoo.com/q?d=t&o=t"
"URI for looking up detailed quote information. ")

(defun emacspeak-websearch-company-news (ticker &optional prefix)
  "Perform an company news lookup.
Retrieves company news, research, profile, insider trades,  or upgrades/downgrades."
  (interactive
   (list
    (emacspeak-websearch-read-query
     "Enter stock ticker of company to lookup: ")
    current-prefix-arg))
  (declare (special emacspeak-websearch-company-news-uri
                    emacspeak-websearch-yahoo-company-news-quotes-uri))
  (let ((url-be-asynchronous nil)
        (type nil)
        (type-char
         (read-char
          "c Upgrades, h history, n news, r Research, p profile, q Quotes, t insider trades")))
    (setq type
          (case type-char
            (?r "z/a")
            (otherwise (format "%c" type-char))))
    (cond
     ((char-equal type-char ?h)
      (emacspeak-websearch-yahoo-historical-chart ticker prefix)
      (emacspeak-auditory-icon 'select-object)
      (message "Fetching data --just a minute."))
     ((char-equal type-char ?q)
      (browse-url
       (concat
        emacspeak-websearch-yahoo-company-news-quotes-uri 
        (format "&s=%s" ticker)))
      (emacspeak-websearch-post-process "Markets close" 'emacspeak-speak-line))
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
                                                   &optional as-html)
  "Look up historical stock data.
Optional second arg as-html processes the results as HTML rather than data."
  (interactive
   (list
    (emacspeak-websearch-read-query "Stock ticker:")
    current-prefix-arg))
  (declare (special emacspeak-websearch-lynx-program
   emacspeak-websearch-yahoo-charts-uri
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
     ((not as-html)
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
;;{{{  usenet

(emacspeak-websearch-set-searcher 'dejanews
                                  'emacspeak-websearch-usenet)

(emacspeak-websearch-set-key ?d 'dejanews)



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
  "http://sourceforge.net/search/"
  "URI for searching the SourceForge site.")

(defun emacspeak-websearch-sourceforge-search (query)
  "Search SourceForge Site. "
  (interactive
   (list
    (emacspeak-websearch-read-query "Search SourceForge for: ")))
  (declare (special emacspeak-websearch-sourceforge-search-uri))
  (let ((url-be-asynchronous nil))
    (emacspeak-websearch-do-post "POST"
                                 emacspeak-websearch-sourceforge-search-uri
                                 (concat 
                                  "type_of_search=soft"
                                  "&exact=1"
                                  "&words="
                                  (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   query
   'w3-table-focus-on-this-cell))

(defvar emacspeak-websearch-freshmeat-search-uri 
  "http://www.freshmeat.net/search?q="
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
   "search results"
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
  "a AppWatch f FreshMeat l Linux p Perl s SourceForge t TEX "
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
      (?l (call-interactively 'emacspeak-websearch-packages-linux))
      (?p (call-interactively 'emacspeak-websearch-cpan-search))
      (?s (call-interactively 'emacspeak-websearch-sourceforge-search))
      (?t (call-interactively 'emacspeak-websearch-ctan-search))
      (otherwise (message emacspeak-websearch-software-sites )))))

;;{{{ Linux Package Index




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
;;{{{ Gutenberg

(emacspeak-websearch-set-searcher 'gutenberg
                                  'emacspeak-websearch-gutenberg)
(emacspeak-websearch-set-key ?G 'gutenberg)

(defvar emacspeak-websearch-gutenberg-uri
  "http://digital.library.upenn.edu/webbin/book/search?"
  "*URI for Gutenberg search")

(defun emacspeak-websearch-gutenberg (type query)
  "Perform an Gutenberg search"
  (interactive
   (list
    (read-char "Author a, Title t")
    (emacspeak-websearch-read-query "Gutenberg query: ")))
  (declare (special emacspeak-websearch-gutenberg-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-gutenberg-uri
             (ecase type
               (?a "author=")
               (?t "title="))
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   query
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
   "Results"
   'emacspeak-speak-line))

;;}}}
;;{{{ google advanced search 

(emacspeak-websearch-set-searcher 'google-advanced
                                  'emacspeak-websearch-google-advanced)

(emacspeak-websearch-set-key ?. 'google-advanced)


(defvar emacspeak-websearch-google-advanced-form
  "
<title>Advanced Google Search</title>
<form method=GET  action=\"http://www.google.com/search\">
  <table>
    <tr>
      <td><label for=\"1\">all   words </td>
      <td> <input id=\"1\" type=text  name=as_q > </td>
    </tr>
    <tr>
      <td > <label for=\"2\">exactSearch</label> </td>
      <td> <input id=\"2\" type=text   name=as_epq> </td>
    </tr>
    <tr>
      <td > <label for=\"3\">Partial match</label> </td>
      <td> <input id=\"3\" type=text   name=as_oq> </td>
    </tr>
    <tr>
      <td > <label for=\"4\">Not containing</label> </td>
      <td> <input id=\"4\" type=text   name=as_eq> </td>
    </tr>
    <tr>
      <td > <label for=\"5\">Language</label> </td>
      <td>
        <select id=\"5\" name=lr >
          <option >any language
            <option value=\"lang_ar\">Arabic
              <option value=\"lang_zh-CN\">Chinese&nbsp;(Simplified)
                <option value=\"lang_zh-TW\">Chinese&nbsp;(Traditional)
                  <option value=\"lang_cs\">Czech
                    <option value=\"lang_da\">Danish
                      <option value=\"lang_nl\">Dutch
                        <option value=\"lang_en\">English
                          <option value=\"lang_et\">Estonian
                            <option value=\"lang_fi\">Finnish
                              <option value=\"lang_fr\">French
                                <option value=\"lang_de\">German
                                  <option value=\"lang_el\">Greek
                                    <option value=\"lang_iw\">Hebrew
                                      <option value=\"lang_hu\">Hungarian
                                        <option value=\"lang_is\">Icelandic
                                          <option value=\"lang_it\">Italian
                                            <option value=\"lang_ja\">Japanese
                                              <option value=\"lang_ko\">Korean
                                                <option value=\"lang_lv\">Latvian
                                                  <option value=\"lang_lt\">Lithuanian
                                                    <option value=\"lang_no\">Norwegian
                                                      <option value=\"lang_pl\">Polish
                                                        <option value=\"lang_pt\">Portuguese
                                                          <option value=\"lang_ro\">Romanian
                                                            <option value=\"lang_ru\">Russian
                                                              <option value=\"lang_es\">Spanish
                                                                <option value=\"lang_sv\">Swedish
                                                                  <option value=\"lang_tr\">Turkish
        </select>
      </td>
    </tr>
    <tr>
      <td > <label for=\"6\">Newer than</label> </td>
      <td>
        <select id=\"6\" name=as_qdr>
          <option value=all> anytime 
            <option value=m3 >past 3 months
              <option value=m6 > past  6 months
                <option value=y1 > past  year
        </select>
      </td>
    </tr>
    <tr>
      <td> <label for=\"7\">Match in</label></td>
      <td>
        <select id=\"7\" name=as_occt>
          <option value=any selected>anywhere in the page
            <option value=title >in the title of the page
              <option value=body >in the text of the page
                <option value=url >in the url of the page
                  <option value=links >in links to the page
        </select>
      </td>
    </tr>
    <tr>
      <td >
<label for=\"site\">Site Search</label>
        <select id=\"site\" name=as_dt>
          <option value=i>Only
            <option value=e >Don't
        </select>
<label for=\"8\">look in   domain</label>
      </td>
      <td>
        <input   id=\"8\" name=as_sitesearch>
      </td>
    </tr>
    <tr >
      <td>Filters</td>
      <td>
        <input id=\"9\" type=radio checked value=off
          name=safe> <label for=\"9\">No filtering</label>
          <input id=\"10\" type=radio  value=active name=safe>
          <label for=\"10\">Safe</label>
      </td>
    </tr>
    <tr>
      <td  >
        <select id=\"11\" name=num>
          <option value=\"10\" selected>10 results
            <option value=\"20\">20 results
              <option value=\"30\">30 results
                <option value=\"50\">50 results
                  <option value=\"100\">100 results
        </select> 
        <input type=submit name=btnG value=\"Google Search\">
      </td>
    </tr>
  </table>
</form>
"
"Markup for Google advanced search form.")

(defun emacspeak-websearch-google-advanced ()
  "Present Google advanced search form simplified for speech interaction."
  (interactive)
  (declare (special emacspeak-websearch-google-advanced-form))
  (let ((buffer (get-buffer-create " *google-advanced*")))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (insert emacspeak-websearch-google-advanced-form)
      (w3-preview-this-buffer)
      (widget-forward 1)
      (emacspeak-auditory-icon 'open-object)
      (emacspeak-speak-line)
      (kill-buffer buffer))))

;;}}}
;;{{{  advanced usenet search 

(emacspeak-websearch-set-searcher 'google-usenet-advanced
                                  'emacspeak-websearch-google-usenet-advanced)

(emacspeak-websearch-set-key ?u 'google-usenet-advanced)


(defvar emacspeak-websearch-google-usenet-advanced-form
  "
<title>Advanced Usenet Search</title>
<form method=GET action=\"http://groups.google.com/groups\" >
  <table >
    <tr >
      <td > <label for=\"1\">All Words</label> </td>
      <td> <input id=\"1\" type=\"text\" value=\"\" name=as_q
      size=25> </td>
<td>
        <input type=submit  value=\"Google Search\">
      </td>
    </tr>
    <tr>
      <td > <label for=\"2\">Exact Match</label> </td>
      <td> <input id=\"2\" type=\"text\" size=25 value=\"\" name=as_epq> </td>
    </tr>
    <tr>
      <td><label for=\"3\">Partial Match</label></td>
      <td> <input id=\"3\" type=\"text\" size=25 value=\"\" name=as_oq> </td>
    </tr>
    <tr>
      <td><label for=\"4\">Not containing</label></td>
      <td> <input id=\"4\" type=\"text\" size=25 value=\"\" name=as_eq> </td>
    </tr>
    <tr>
      <td > <label for=\"group\">Group search</label> </td>
      <td> <input id=\"group\" size=30 value=\"\" name=as_ugroup> </td>
    </tr>
    <tr>
      <td > <label for=\"subject\">Subject Search</label> </td>
      <td> <input id=\"subject\" size=30 value=\"\" name=as_usubject>  </td>
    </tr>
    <tr>
      <td > <label for=\"author\">Author Search</label> </td>
      <td> <input id=\"author\" size=30 value=\"\" name=as_uauthors>  </td>
    </tr>
    <tr>
      <td > <label for=\"msgid\">Message ID</label> </td>
      <td> <input id=\"msgid\" size=30 value=\"\" name=as_umsgid> </td>
    </tr>
    <tr >
      <td> <label for=\"lang\">Language</label> </td>
      <td >
        <select id=\"lang\" name=lr>
          <option  value=\"\">any language
          <option  value=\"lang_ar\">Arabic
          <option  value=\"lang_zh-CN\">Chinese&nbsp;(Simplified)
          <option  value=\"lang_zh-TW\">Chinese&nbsp;(Traditional)
          <option  value=\"lang_cs\">Czech
          <option  value=\"lang_da\">Danish
          <option  value=\"lang_nl\">Dutch
          <option  value=\"lang_en\">English
          <option  value=\"lang_et\">Estonian
          <option  value=\"lang_fi\">Finnish
          <option  value=\"lang_fr\">French
          <option  value=\"lang_de\">German
          <option  value=\"lang_el\">Greek
          <option  value=\"lang_iw\">Hebrew
          <option  value=\"lang_hu\">Hungarian
          <option  value=\"lang_is\">Icelandic
          <option  value=\"lang_it\">Italian
          <option  value=\"lang_ja\">Japanese
          <option  value=\"lang_ko\">Korean
          <option  value=\"lang_lv\">Latvian
          <option  value=\"lang_lt\">Lithuanian
          <option  value=\"lang_no\">Norwegian
          <option  value=\"lang_pl\">Polish
          <option  value=\"lang_pt\">Portuguese
          <option  value=\"lang_ro\">Romanian
          <option  value=\"lang_ru\">Russian
          <option  value=\"lang_es\">Spanish
          <option  value=\"lang_sv\">Swedish
          <option  value=\"lang_tr\">Turkish
        </select>
      </td>
    </tr>
    <tr >
      <td > <label for=\"dates\">Since</label> 
        <input id=\"dates\" name=as_drrb type=radio value=q checked>
        </input></td>
<td>
          <label for=\"since\"> messages posted </label>
          <select id=\"since\" name=as_qdr>
            <option  value=\"\" selected>anytime
            <option  value=\"d\">in the last 24 hours
            <option  value=\"w\">in the last week
            <option  value=\"m\">in the last month
            <option  value=\"y\">in the last year
          </select>
      </td>
    </tr>
    <tr>
      <td><label for=\"daterange\">Date Range</label></td>
      <td > <input id=\"daterange\" name=as_drrb
                   type=radio value=b> </td>
    </tr>
  </table>
<p> Articles posted between 
<label for=\"miny\">Year</label>
<select id=\"miny\" name=as_miny >
<option value=\"1995\" selected>1995
<option value=\"1996\">1996
<option value=\"1997\">1997
<option value=\"1998\">1998
<option value=\"1999\">1999
<option value=\"2000\">2000
<option value=\"2001\">2001
</select>
<label for=\"minm\">Month </label>
<select id=\"minm\" name=as_minm >
<option value=\"1\">Jan
<option value=\"2\">Feb
<option value=\"3\" selected>Mar
<option value=\"4\">Apr
<option value=\"5\">May
<option value=\"6\">Jun
<option value=\"7\">Jul
<option value=\"8\">Aug
<option value=\"9\">Sep
<option value=\"10\">Oct
<option value=\"11\">Nov
<option value=\"12\">Dec
</select>
<label for=\"mind\">Day</label>
<select id=\"mind\" name=as_mind >
<option value=\"1\">1
<option value=\"2\">2
<option value=\"3\">3
<option value=\"4\">4
<option value=\"5\">5
<option value=\"6\">6
<option value=\"7\">7
<option value=\"8\">8
<option value=\"9\">9
<option value=\"10\">10
<option value=\"11\">11
<option value=\"12\">12
<option value=\"13\">13
<option value=\"14\">14
<option value=\"15\">15
<option value=\"16\">16
<option value=\"17\">17
<option value=\"18\">18
<option value=\"19\">19
<option value=\"20\">20
<option value=\"21\">21
<option value=\"22\">22
<option value=\"23\">23
<option value=\"24\">24
<option value=\"25\">25
<option value=\"26\">26
<option value=\"27\">27
<option value=\"28\">28
<option value=\"29\" selected>29
<option value=\"30\">30
<option value=\"31\">31
</select> and 
and <label for=\"maxy\">Year</label>
<select id=\"maxy\" name=as_maxy >
<option value=\"1995\">1995
<option value=\"1996\">1996
<option value=\"1997\">1997
<option value=\"1998\">1998
<option value=\"1999\">1999
<option value=\"2000\">2000
<option value=\"2001\" selected>2001
</select>
<label for=\"maxm\">Month</label>
<select id=\"maxm\" name=as_maxm >
<option value=\"1\">Jan
<option value=\"2\">Feb
<option value=\"3\">Mar
<option value=\"4\">Apr
<option value=\"5\">May
<option value=\"6\">Jun
<option value=\"7\">Jul
<option value=\"8\">Aug
<option value=\"9\">Sep
<option value=\"10\" selected>Oct
<option value=\"11\">Nov
<option value=\"12\">Dec
</select>
<label for=\"maxd\">Day</label>
<select id=\"maxd\" name=as_maxd >
<option value=\"1\">1
<option value=\"2\">2
<option value=\"3\">3
<option value=\"4\">4
<option value=\"5\">5
<option value=\"6\">6
<option value=\"7\">7
<option value=\"8\">8
<option value=\"9\">9
<option value=\"10\">10
<option value=\"11\">11
<option value=\"12\">12
<option value=\"13\">13
<option value=\"14\">14
<option value=\"15\">15
<option value=\"16\">16
<option value=\"17\">17
<option value=\"18\">18
<option value=\"19\" selected>19
<option value=\"20\">20
<option value=\"21\">21
<option value=\"22\">22
<option value=\"23\">23
<option value=\"24\">24
<option value=\"25\">25
<option value=\"26\">26
<option value=\"27\">27
<option value=\"28\">28
<option value=\"29\">29
<option value=\"30\">30
<option value=\"31\">31
</select>
</p>
  <table>
    <tr>
      <td >
        <label for=\"num\">Results</label>
        <select id=\"num\"name=num>
          <option  value=\"10\">10 messages
          <option  value=\"20\">20 messages
          <option  value=\"30\">30 messages
          <option  value=\"50\">50 messages
          <option  value=\"100\">100 messages
        </select> 
      </td>
      <td>
        <label for=\"scoring\">Sort by</label>
        <select id=\"scoring\" name=as_scoring>
          <option  value=r selected>Sort by relevance
          <option  value=d>Sort by date
        </select> 
      </td>
      <td>
        <input type=submit  value=\"Google Search\">
      </td>
    </tr>
  </table>
</form>
"
"Usenet advanced search from google.")

(defun emacspeak-websearch-google-usenet-advanced ()
  "Present Google Usenet advanced search form simplified for speech interaction."
  (interactive)
  (declare (special emacspeak-websearch-google-usenet-advanced-form))
  (let ((buffer (get-buffer-create " *google-advanced*")))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (insert emacspeak-websearch-google-usenet-advanced-form)
      (w3-preview-this-buffer)
      (widget-forward 1)
      (emacspeak-auditory-icon 'open-object)
      (emacspeak-speak-line)
      (kill-buffer buffer))))

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
;;{{{  kerbango radio tuner 

(emacspeak-websearch-set-searcher 'kerbango
                                  'emacspeak-websearch-kerbango-search)
(emacspeak-websearch-set-key ?k 'kerbango)


(defvar emacspeak-websearch-kerbango-uri
  "http://www.kerbango.com/tuner/index.html"
  "URI for searching Kerbango radio tuner.")

(defun emacspeak-websearch-kerbango-search (keywords)
  "Search Kerbango radio tuner."
  (interactive
   (list
    (emacspeak-websearch-read-query "Keywords:")))
  (declare (special emacspeak-websearch-kerbango-uri))
  (let ((url-be-asynchronous nil))
    (emacspeak-websearch-do-post "POST" emacspeak-websearch-kerbango-uri
                                 (concat "keywords="
                                         (webjump-url-encode keywords)
                                         "&bwc="
                                         (webjump-url-encode ""))))
  (emacspeak-websearch-post-process
   "Your search "
   'emacspeak-speak-line))

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
  "http://search.news.yahoo.com/search/news?"
  "*URI for launching a Yahoo News search")

(defun emacspeak-websearch-news-yahoo (query)
  "Perform an Yahoo News search"
  (interactive
   (list (emacspeak-websearch-read-query "Yahoo News Query: ")))
  (declare (special emacspeak-websearch-news-yahoo-uri
                    emacspeak-w3-url-rewrite-rule))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-news-yahoo-uri
             (format "p=%s&n=20&c=news"
                     (webjump-url-encode query)))))
  (emacspeak-websearch-post-process
   "Matches"
   'w3-table-focus-on-this-cell)
  (setq emacspeak-w3-url-rewrite-rule
        '("/h/" "/htx/")))

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
;;{{{ RPMFind

(emacspeak-websearch-set-searcher 'rpm-find
                                  'emacspeak-websearch-rpm-find)

(emacspeak-websearch-set-key 18 'rpm-find)


(defvar emacspeak-websearch-rpm-find-uri
  "http://rpmfind.net/linux/rpm2html/search.php?query="
  "*URI for RPM  Site search")

(defun emacspeak-websearch-rpm-find (query)
  "Search RPM  catalog  site."
  (interactive
   (list
    (emacspeak-websearch-read-query "Find RPM: ")))
  (declare (special emacspeak-websearch-rpm-find-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-rpm-find-uri
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   query
   'emacspeak-speak-line))

;;}}}
;;{{{  RFB

(emacspeak-websearch-set-searcher 'rfb
                                  'emacspeak-websearch-rfb-search)
(emacspeak-websearch-set-key ?R 'rfb)

(defvar emacspeak-websearch-rfb-uri
  "http://www.rfbd.org/search_process.asp"
  "URI for searching RFB catalogues.")

(defun emacspeak-websearch-rfb-search (author title)
  "Search RFB&D catalog."
  (interactive
   (list
    (emacspeak-websearch-read-query "Author:")
    (emacspeak-websearch-read-query "Title:")))
  (declare (special emacspeak-websearch-rfb-uri))
  (let ((url-be-asynchronous nil))
    (emacspeak-websearch-do-post "POST"
                                 emacspeak-websearch-rfb-uri
                                 (concat "author="
                                         (webjump-url-encode author)
                                         "&title="
                                         (webjump-url-encode title))))
  (emacspeak-websearch-post-process
   "Number"
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
;;{{{  IBM Machine Translation  

(emacspeak-websearch-set-searcher 'machine-translate
                                  'emacspeak-websearch-machine-translate)

(emacspeak-websearch-set-key ?t 'machine-translate)

(defvar emacspeak-websearch-machine-translate-uri
  "http://demo.alphaworks.ibm.com/cgi-bin/mtconnect"
  "*URI for launching a machine translation from IBM Alphaworks")

(defun emacspeak-websearch-machine-translate (lang query)
  "Perform a machine translation request"
  (interactive
   (list
    (emacspeak-websearch-read-query "Target language: ")
    (emacspeak-websearch-read-query "Translate:")))
  (declare (special emacspeak-websearch-machine-translate-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-machine-translate-uri
             (format "?lang=%s" lang)
             "&text="
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process ""
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
"http://www.weather.com/weather/local/"
  "*URI for getting weather forecast.")

(defun emacspeak-websearch-weather (zip)
  "Get weather forecast for specified zip code."
  (interactive
   (list (emacspeak-websearch-read-query "Zip Code: ")))
  (declare (special emacspeak-websearch-weather-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-weather-uri
             zip)))
  (emacspeak-websearch-post-process
   zip
   'w3-table-focus-on-this-cell))

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
;;{{{ People from yahoo

(emacspeak-websearch-set-searcher 'people-yahoo
                                  'emacspeak-websearch-people-yahoo)
(emacspeak-websearch-set-key ?p 'people-yahoo)

(defvar emacspeak-websearch-people-yahoo-uri
  "http://people.yahoo.com/py/psPhoneSearch.py?"
  "*URI for launching a People   search on Yahoo.")

(defun emacspeak-websearch-people-yahoo ()
  "Perform an Yahoo  people search"
  (interactive)
  (declare (special emacspeak-websearch-people-yahoo-uri))
  (let ((url-be-asynchronous nil))
    (browse-url 
     (concat emacspeak-websearch-people-yahoo-uri
(format "FirstName=%s&LastName=%s&City=%s&State=%s"
        (webjump-url-encode (read-from-minibuffer "First name: "))
                            (webjump-url-encode (read-from-minibuffer "Last name: "))
                            (webjump-url-encode (read-from-minibuffer "City: "))
                                                (webjump-url-encode (read-from-minibuffer "State: "))))))
  (emacspeak-websearch-post-process
   "First"
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
  "http://groups.google.com/groups?"
  "URI to open a group on Usenet archive.")

(defun emacspeak-websearch-usenet (group &optional prefix)
  "Prompt and browse a Usenet newsgroup.
Optional interactive prefix arg results in prompting for a search term."
  (interactive
   (list
    (read-from-minibuffer "Newsgroup: ")
    current-prefix-arg))
  (declare (special emacspeak-dejanews-uri))
  (let ((url-be-asynchronous nil)
        (url nil))
    (cond
     (prefix                            ;search
      (setq url
            (concat emacspeak-dejanews-uri
                    (format "meta=group%%3D%s&q=%s"
                            group
                            (read-from-minibuffer
                             (format "Search %s for:" group))))))
     (t                                 ;browse
      (setq url 
            (concat emacspeak-dejanews-uri
                    (format "as_ugroup=%s" group)
                    ))))
    (browse-url  url)
    (if prefix
        (search-forward "Sorted " nil t)
      (search-forward "Threads" nil t))
    (emacspeak-speak-line)))



;;}}}
(provide 'emacspeak-websearch)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
