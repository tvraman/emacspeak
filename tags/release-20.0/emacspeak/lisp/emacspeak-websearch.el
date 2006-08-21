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
;;;Copyright (C) 1995 -- 2003, T. V. Raman 
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

(require 'emacspeak-preamble)
(eval-when-compile
  (condition-case nil
      (require  'emacspeak-w3)
    (error nil)))
(require 'webjump)
;;}}}
;;{{{  Introduction:

;;; Commentary:

;;; This module provides utility functions for searching the WWW

;;; Code:

;;}}}
;;{{{ searcher table
;;;###autoload
(defgroup emacspeak-websearch nil
  "Websearch tools for the Emacspeak desktop."
  :group 'emacspeak)
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
(defun emacspeak-websearch-help ()
  "Displays key mapping used by Emacspeak Websearch."
  (interactive)
  (let ((map (loop for key being the hash-keys of 
                   emacspeak-websearch-keytable
                   collect
                   (cons key (gethash key emacspeak-websearch-keytable)))))
    (setq map (sort map
                    #'(lambda (a b)
                        (< (car a)
                           (car b)))))
    (with-output-to-temp-buffer "*Help*"
      (save-excursion
        (set-buffer "*Help*")
        (princ "Websearch Keys:\n\n")
        (loop for m in map 
              do 
              (princ (key-description (list (car m))))
              (move-to-column 16 )
              (princ "`")
              (princ (emacspeak-websearch-get-searcher (cdr m)))
              (princ "'")
              (princ "\n"))
        (help-setup-xref
         (list #'emacspeak-websearch-help)
         (interactive-p))))
    (pop-to-buffer "*Help*")
    (help-mode)
    (goto-char (point-min))
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'help)))
                                
(emacspeak-websearch-set-searcher  'help
                                   'emacspeak-websearch-help)

(emacspeak-websearch-set-key ?? 'help)
;;;###autoload
(defun emacspeak-websearch-dispatch  (&optional prefix)
  "Launches specific websearch queries.
Press `?' to list available search engines.
Once selected, the selected searcher prompts for additional information as appropriate.
When using W3,  this interface attempts to speak the most relevant information on the result page."
  (interactive "P")
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
;;;###autoload
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

(defun emacspeak-websearch-post-process (locator speaker &rest args)
  "Set up post processing steps on a result page.
LOCATOR is a string to search for in the results page.
SPEAKER is a function to call to speak relevant information.
ARGS specifies additional arguments to SPEAKER if any."
  (declare (special emacspeak-w3-post-process-hook))
  (when (or   (eq browse-url-browser-function 'w3-fetch)
	      (eq browse-url-browser-function 'browse-url-w3))
    (add-hook  'emacspeak-w3-post-process-hook
	       (`
		(lambda nil
		  (cond
		   ((search-forward (, locator) nil t)
		    (recenter 0)
		    (apply(quote 
			   (, speaker))
			  (, args)))
		   (t (message "Your search appears to have ffailed."))))))))

;;}}}

;;}}}
;;{{{ websearch utilities

;;{{{ display form 

(emacspeak-websearch-set-searcher 'display-form
                                  'emacspeak-websearch-display-form)

(emacspeak-websearch-set-key ?/ 'display-form)

(defun emacspeak-websearch-display-form (form-markup)
  "Display form specified by form-markup."
  (interactive
   (list
    (let ((emacspeak-speak-messages nil))
      (emacspeak-pronounce-define-local-pronunciation
       (expand-file-name "xml-forms"
                         emacspeak-lisp-directory)
       " xml forms ")
      (read-file-name "Display Form: "
                      (expand-file-name "xml-forms/" emacspeak-lisp-directory)))))
  (declare (special emacspeak-w3-xsl-p
                    emacspeak-w3-post-process-hook
                    emacspeak-lisp-directory))
  (let ((buffer (get-buffer-create " *search-form*"))
        (emacspeak-w3-xsl-p nil))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (insert-file  form-markup)
      (add-hook 'emacspeak-w3-post-process-hook
		#'(lambda ()
		    (goto-char (point-min))
		    (widget-forward 1)
		    (emacspeak-auditory-icon 'open-object)
		    (emacspeak-widget-summarize (widget-at (point)))))
      (emacspeak-w3-preview-this-buffer)
      (kill-buffer buffer))))

;;}}}
;;{{{ AllTheWeb

(emacspeak-websearch-set-searcher 'alltheweb
                                  'emacspeak-websearch-alltheweb-search)
(emacspeak-websearch-set-key ?A  'alltheweb)

(defvar emacspeak-websearch-alltheweb-uri
  "http://www.alltheweb.com/search?avkw=fogg&cat=web&cs=utf-8&_sb_lang=pref"
  "*URI for AllTheWeb search")

(defun emacspeak-websearch-alltheweb-search (query  )
  "Perform an AllTheWeb  search."
  (interactive
   (list
    (emacspeak-websearch-read-query "All The Web Query: ")))
  (declare (special emacspeak-websearch-alltheweb-uri))
  (browse-url 
   (concat emacspeak-websearch-alltheweb-uri
	   "&q="
	   (webjump-url-encode query)))
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
  (let (
	)
    (browse-url 
     (concat emacspeak-websearch-altavista-uri
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   "Results"
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
  (let (
	)
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
  (let (
	
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
  "http://www.bbc.co.uk/cgi-bin/search/results.pl?q="
  "URI to search the BBC archives.")

(defun emacspeak-websearch-bbc-search (query)
  "Search BBC archives."
  (interactive
   (list
    (emacspeak-websearch-read-query "Search BBC for: ")))
  (declare (special emacspeak-websearch-bbc-uri))
  (emacspeak-w3-extract-nested-table-list
   (list  4 5 6 7 8 9 10 11 12)
   (concat emacspeak-websearch-bbc-uri
           (webjump-url-encode query))
   'speak))

;;}}}
;;{{{ CNN

;; (emacspeak-websearch-set-searcher 'cnn
;;                                   'emacspeak-websearch-cnn-search)
;; (emacspeak-websearch-set-key ?c 'cnn)

;; (defvar emacspeak-websearch-cnn-uri
;;   "http://search.cnn.com/cnn/search?sites=cnn&source=cnn&"
;;   "*URI for launching a CNN Interactive  search.")

;; (defun emacspeak-websearch-cnn-search (query )
;;   "Perform an CNN search.  "
;;   (interactive
;;    (list
;;     (emacspeak-websearch-read-query "CNN Interactive Query: ")))
;;   (declare (special emacspeak-websearch-cnn-uri))
;;   (browse-url 
;;    (concat emacspeak-websearch-cnn-uri
;;            (format "query=%s&qt=%s"
;;                    (webjump-url-encode query)
;;                    (webjump-url-encode query))))
;;   (emacspeak-websearch-post-process
;;    "Results"
;;    'emacspeak-speak-line))

;;}}}
;;{{{ CNN FN

;; (emacspeak-websearch-set-searcher 'cnn-fn
;;                                   'emacspeak-websearch-fn-cnn-search)
;; (emacspeak-websearch-set-key ?f 'cnn-fn)

;; (defvar emacspeak-websearch-fn-cnn-uri
;;   "http://search.cnnfn.com/query.html?qt="
;;   "*URI for launching a CNN FN  search.")

;; (defvar emacspeak-websearch-fn-cnn-options
;;   "&col=cnnfn&rq=0&qc=cnnfn&qm=1&st=1&nh=10&lk=1&rf=1&go=+seek+"
;;   "*Additional default options to pass to CNN.")

;; (defun emacspeak-websearch-fn-cnn-search (query &optional prefix)
;;   "Perform an CNN FNsearch.  
;; Optional interactive prefix arg
;; prompts for additional search parameters.  The default is to
;; sort by date and show summaries.  To sort by relevance
;; specify additional parameter &rf=0.  To hide summaries,
;; specify additional parameter &lk=2.
;; You can customize the defaults by setting variable
;; emacspeak-websearch-fn-cnn-options to an appropriate string."
;;   (interactive
;;    (list
;;     (emacspeak-websearch-read-query "CNN FN Query: ")
;;     current-prefix-arg))
;;   (declare (special emacspeak-websearch-fn-cnn-uri
;;                     emacspeak-websearch-fn-cnn-options))
;;   (let (
;; 	)
;;     (browse-url 
;;      (concat emacspeak-websearch-fn-cnn-uri
;;              (webjump-url-encode query)
;;              emacspeak-websearch-fn-cnn-options
;;              (if prefix 
;;                  (read-from-minibuffer
;;                   "Additional query parameters: ")
;;                ""))))
;;   (emacspeak-websearch-post-process
;;    "Results"
;;    'emacspeak-speak-line))

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
  (let (
	)
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
  (let (
	)
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
  (let (
	
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
                                  'emacspeak-websearch-usenet-search)

(emacspeak-websearch-set-key ?d 'dejanews)

;;;###autoload
(defun emacspeak-websearch-usenet-search (group)
  "Search a Usenet newsgroup."
  (interactive
   (list
    (read-from-minibuffer "Newsgroup search: ")))
  (emacspeak-websearch-usenet group 'search))

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
  (let (
	)
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
  "http://sourceforge.net/search/?"
  "URI for searching the SourceForge site.")

(defun emacspeak-websearch-sourceforge-search (query)
  "Search SourceForge Site. "
  (interactive
   (list
    (emacspeak-websearch-read-query "Search SourceForge for: ")))
  (declare (special emacspeak-websearch-sourceforge-search-uri))
                                        ;(emacspeak-websearch-do-post "POST"
  (emacspeak-w3-extract-nested-table-list
   (list 5 6)
   (concat
    emacspeak-websearch-sourceforge-search-uri
    "type_of_search=soft"
    "&exact=1"
    "&words="
    (webjump-url-encode query))))
  

(defvar emacspeak-websearch-freshmeat-search-uri 
  "http://www.freshmeat.net/search?q="
  "URI for searching Freshmeat site. ")

(defun emacspeak-websearch-freshmeat-search (query)
  "Search Freshmeat  Site. "
  (interactive
   (list
    (emacspeak-websearch-read-query "Search Freshmeat  for: ")))
  (declare (special emacspeak-websearch-freshmeat-search-uri))
  (let (
	)
    (browse-url 
     (concat emacspeak-websearch-freshmeat-search-uri
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   "search results"
   'emacspeak-speak-line))

(defvar emacspeak-websearch-ctan-search-uri 
  "http://www.ctan.org/tools/filesearch?action=/search/&filename="
  "URI for searching CTAN archives for tex and latex utilities. ")

(defun emacspeak-websearch-ctan-search (query)
  "Search CTAN Comprehensive TeX Archive Network   Site. "
  (interactive
   (list
    (emacspeak-websearch-read-query
     "Lookup Comprehensive TEX Archive for: ")))
  (declare (special emacspeak-websearch-ctan-search-uri))
  (let (
	)
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
  (let (
	)
    (browse-url 
     (concat emacspeak-websearch-cpan-search-uri
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   query
   'emacspeak-speak-line))

(defvar emacspeak-websearch-software-sites
  "f FreshMeat p Perl s SourceForge t TEX "
  "Sites searched for open source software. ")

;;; top level dispatcher for searching source locations 
(defun emacspeak-websearch-software-search  ()
  "Search SourceForge, Freshmeat and other sites. "
  (interactive)
  (declare (special emacspeak-websearch-software-sites))
  (let ((site
         (read-char emacspeak-websearch-software-sites)))
    (case site
      (?f (call-interactively 'emacspeak-websearch-freshmeat-search))
      (?p (call-interactively 'emacspeak-websearch-cpan-search))
      (?s (call-interactively 'emacspeak-websearch-sourceforge-search))
      (?t (call-interactively 'emacspeak-websearch-ctan-search))
      (otherwise (message emacspeak-websearch-software-sites )))))

;;}}}
;;{{{  Encyclopeadia Britannica 

(emacspeak-websearch-set-searcher 'britannica
                                  'emacspeak-websearch-britannica-search)
(emacspeak-websearch-set-key ?e 'britannica)

;;; this requires a password
                                        ;(defvar emacspeak-websearch-britannica-uri 
                                        ;"http://www.eb.com:180/bol/search?type=topic&I3.x=0&I3.y=0&DBase=Articles"
                                        ;"URI for searching Britannica online.")

(defvar emacspeak-websearch-britannica-uri 
  "http://search.britannica.com/search?query="
  "URI for searching Britannica online.")

(defun emacspeak-websearch-britannica-search (query)
  "Search Encyclopedia Britannica."
  (interactive
   (list
    (emacspeak-websearch-read-query
     "Search Encyclopedia Britannica  for: ")))
  (declare (special emacspeak-websearch-britannica-uri))
  (let (
	)
    (browse-url 
     (concat emacspeak-websearch-britannica-uri
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   query
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
  (let (
	)
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

(defcustom emacspeak-websearch-google-number-of-results 25
  "Number of results to return from google search."
  :type 'number
  :group 'emacspeak-websearch)

(defvar emacspeak-websearch-google-uri
  "http://www.google.com/search?q="
  "*URI for Google search")

(defcustom emacspeak-websearch-google-feeling-lucky-p nil
  "If non-nil, then Google search will use the 
I'm Feeling Lucky button by default."
  :type 'boolean 
  :group 'emacspeak-websearch)
;;;###autoload
(defun emacspeak-websearch-google (query &optional lucky)
  "Perform an Google search.
Optional interactive prefix arg `lucky' is equivalent to hitting the 
I'm Feeling Lucky button on Google.
Meaning of the `lucky' flag can be inverted by setting option emacspeak-websearch-google-feeling-lucky-p."
  (interactive
   (list
    (emacspeak-websearch-read-query 
     (format "Google %s: "
             (if
                 (if emacspeak-websearch-google-feeling-lucky-p
                     (not current-prefix-arg)
                   current-prefix-arg)
                 "feeling lucky"
               "query ")))
    current-prefix-arg))
  (declare (special emacspeak-websearch-google-uri
                    emacspeak-websearch-google-feeling-lucky-p emacspeak-websearch-google-number-of-results))
  (let ((lucky-flag (if emacspeak-websearch-google-feeling-lucky-p
                        (not lucky)
                      lucky)))
    (emacspeak-w3-without-xsl
     (browse-url 
      (concat emacspeak-websearch-google-uri
	      (webjump-url-encode query)
	      (format "&num=%s"
		      emacspeak-websearch-google-number-of-results)
	      (when lucky-flag
		(concat 
		 "&btnI="
		 (webjump-url-encode
		  "I'm Feeling Lucky"))))))
    (if lucky-flag
        (emacspeak-speak-line)
      (emacspeak-websearch-post-process
       "results"
       'emacspeak-speak-line))))

(emacspeak-websearch-set-searcher 'google-lucky
                                  'emacspeak-websearch-google-feeling-lucky)

(emacspeak-websearch-set-key ?\  'google-lucky)

(defun emacspeak-websearch-google-feeling-lucky (query)
  "Do a I'm Feeling Lucky Google search."
  (interactive
   (list
    (emacspeak-websearch-read-query 
     "Google Lucky Search: ")))
  (let ((emacspeak-websearch-google-feeling-lucky-p t))
    (emacspeak-websearch-google query)))

;;;###autoload
(defun emacspeak-websearch-google-search-in-date-range ()
  "Use this from inside the calendar to do Google date-range searches."
  (interactive)
  (declare (special calendar-mark-ring))
  (let ((query (emacspeak-websearch-read-query "Google for: "))
        (from (read (calendar-astro-date-string (calendar-cursor-to-date t))))
        (to (read (calendar-astro-date-string (or (car calendar-mark-ring)
                                                  (error "No mark set in this buffer"))))))
    (emacspeak-websearch-google
     
     (concat
      (webjump-url-encode query )
      (format " daterange:%s-%s"
              (min from to)
              (max from to))))))

(when (featurep 'calendar)
  (declaim (special calendar-mode-map))
  (define-key calendar-mode-map "gg"
    'emacspeak-websearch-google-search-in-date-range))

;;}}}
;;{{{ froogle

(emacspeak-websearch-set-searcher 'froogle
                                  'emacspeak-websearch-froogle)
(emacspeak-websearch-set-key 6 'froogle)

(defvar emacspeak-websearch-froogle-uri
  "http://froogle.google.com/froogle?q=%s&btnG=Froogle+Searchs/"
  "*URI for Froogle search")

(defun emacspeak-websearch-froogle (query )
  "Perform a Froogle search."
  (interactive
   (list
    (emacspeak-websearch-read-query "Froogle Search: ")))
  (declare (special emacspeak-websearch-froogle-uri))
  (emacspeak-w3-without-xsl
   (browse-url 
    (format emacspeak-websearch-froogle-uri
	    (webjump-url-encode query)))
   (emacspeak-websearch-post-process
    query
    'emacspeak-speak-line)))

;;}}}
;;{{{ teoma

(emacspeak-websearch-set-searcher 'teoma
                                  'emacspeak-websearch-teoma)

(emacspeak-websearch-set-key ?T 'teoma)

(defvar emacspeak-websearch-teoma-uri
  "http://s.teoma.com/search?qcat=1&qsrc=1&q="
  "*URI for Teoma  search")

(defun emacspeak-websearch-teoma (query )
  "Perform an Teoma  search."
  (interactive
   (list
    (emacspeak-websearch-read-query 
     (format "Teoma Search: "))
    ))
  (declare (special emacspeak-websearch-teoma-uri))
  (browse-url 
   (concat emacspeak-websearch-teoma-uri
	   (webjump-url-encode query))))

;;}}}
;;{{{ google advanced search 

(emacspeak-websearch-set-searcher 'google-advanced
                                  'emacspeak-websearch-google-advanced)

(emacspeak-websearch-set-key ?. 'google-advanced)

(defvar emacspeak-websearch-google-advanced-form
  (expand-file-name "xml-forms/google-advanced.xml"
                    emacspeak-lisp-directory)
  "Markup for Google advanced search form.")

(defun emacspeak-websearch-google-advanced ()
  "Present Google advanced search form simplified for speech interaction."
  (interactive)
  (declare (special emacspeak-websearch-google-advanced-form))
  (emacspeak-websearch-display-form emacspeak-websearch-google-advanced-form))

;;}}}
;;{{{  advanced usenet search 

(emacspeak-websearch-set-searcher 'google-usenet-advanced
                                  'emacspeak-websearch-google-usenet-advanced)

(emacspeak-websearch-set-key ?u 'google-usenet-advanced)

(defvar emacspeak-websearch-google-usenet-advanced-form
  (expand-file-name "xml-forms/google-usenet-advanced.xml"
                    emacspeak-lisp-directory)
  "Usenet advanced search from google.")

(defun emacspeak-websearch-google-usenet-advanced ()
  "Present Google Usenet advanced search form simplified for speech interaction."
  (interactive)
  (declare (special emacspeak-websearch-google-usenet-advanced-form))
  (emacspeak-websearch-display-form emacspeak-websearch-google-usenet-advanced-form))

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
  (let (
	)
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
  "http://maps.yahoo.com/py/ddResults.py?Pyt=Tmap&doit=1&newname=&newdesc=&Get+Directions=Get+Directions&textonly=1"
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
                    emacspeak-xslt-use-wget-to-download
                    emacspeak-websearch-map-maps-uri))
  (let ((emacspeak-xslt-use-wget-to-download t))
    (cond
     (map
      (browse-url 
       (concat
	emacspeak-websearch-map-maps-uri
	query))
      (emacspeak-websearch-post-process
       "Nearby"
       'emacspeak-speak-line))
     (t 
      (emacspeak-w3-extract-table-by-match "Start"
					   (concat
					    emacspeak-websearch-map-directions-uri
					    query)
					   'speak)))))
         
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
  (add-hook 'emacspeak-w3-post-process-hook
	    #'(lambda nil
		(declare (special  emacspeak-w3-url-rewrite-rule
				   emacspeak-w3-class-filter))
		(setq emacspeak-w3-class-filter "article"
		      emacspeak-w3-url-rewrite-rule
		      '("$" "&printer=1"))))
  (emacspeak-w3-xslt-filter
   "//ol"
   (concat emacspeak-websearch-news-yahoo-uri
           (format "p=%s&n=20&c=news"
                   (webjump-url-encode query)))
   'speak-result))

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
  (let (
	)
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
  (let (
	)
    (browse-url 
     (concat emacspeak-websearch-open-directory-uri
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   "Search results"
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
  (let (
	)
    (browse-url 
     (concat emacspeak-websearch-rpm-find-uri
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   query
   'emacspeak-speak-line))

;;}}}
;;{{{  Recorded books

(emacspeak-websearch-set-searcher 'recorded-books
                                  'emacspeak-websearch-recorded-books-search)

(emacspeak-websearch-set-key ?r 'recorded-books)

(defvar emacspeak-websearch-recorded-books-advanced-form
  (expand-file-name "xml-forms/recorded-books-advanced.xml"
                    emacspeak-lisp-directory)
  "Search form for finding recorded books.")

(defun emacspeak-websearch-recorded-books-search ()
  "Present advanced search form for recorded books."
  (interactive)
  (declare (special emacspeak-websearch-recorded-books-advanced-form))
  (emacspeak-websearch-display-form emacspeak-websearch-recorded-books-advanced-form))

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
  (let (
	)
    (browse-url 
     (concat emacspeak-websearch-merriam-webster-uri
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   "Main Entry"
   'emacspeak-speak-line))

;;}}}
;;{{{ Weather

(emacspeak-websearch-set-searcher 'weather
                                  'emacspeak-websearch-weather)
(emacspeak-websearch-set-key ?w 'weather)

(defvar emacspeak-websearch-weather-uri
  "http://www.srh.noaa.gov/zipcity.php?inputstring="
  "*URI for getting weather forecast.")

(defun emacspeak-websearch-weather (query)
  "Get weather forecast for specified zip code."
  (interactive
   (list (emacspeak-websearch-read-query "City,State or Zip: ")))
  (declare (special emacspeak-websearch-weather-uri))
  (emacspeak-w3-extract-tables-by-position-list
   (list 5 6 7 8 9 )
   (concat emacspeak-websearch-weather-uri
	   (webjump-url-encode query))
   'speak))
  

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
  (let (
	)
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
  (let (
	)
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
  (let (
	)
    (browse-url 
     (concat emacspeak-websearch-yahoo-uri
             (webjump-url-encode query))))
  (emacspeak-websearch-post-process
   "Web Pages"
   'emacspeak-speak-line))

;;}}}
;;{{{ Exchange rate convertor 

(emacspeak-websearch-set-searcher 'exchange-rate-convertor
                                  'emacspeak-websearch-exchange-rate-convertor)

(emacspeak-websearch-set-key ?x 'exchange-rate-convertor)

(defvar emacspeak-websearch-exchange-rate-form 
  (expand-file-name "xml-forms/exchange-rate-convertor.xml"
                    emacspeak-lisp-directory)
  "Form for performing currency conversion.")

(defvar emacspeak-websearch-exchange-rate-convertor-uri
  "http://www.xe.com/ucc/convert.cgi?Amount=1&From=%s&To=%s&submit=Perform+Conversion"
  "URI template  for currency conversion.")

(defun emacspeak-websearch-exchange-rate-convertor (conversion-spec)
  "Currency convertor."
  (interactive
   (list
    (read-from-minibuffer
     "Currency Convertor: FROM|TO:")))
  (declare (special emacspeak-websearch-exchange-rate-convertor-uri))
  (let ((fields (split-string conversion-spec "|"))
        (url nil))
    (setq url
          (format emacspeak-websearch-exchange-rate-convertor-uri
                  (upcase (first fields))
                  (upcase (second fields))))
    (emacspeak-w3-extract-table-by-match
     (format "%s" (upcase  (first fields)))
     url 'speak)))
      

;;}}}
;;{{{ my rss

(emacspeak-websearch-set-searcher 'my-rss-search
                                  'emacspeak-websearch-my-rss-search)

(emacspeak-websearch-set-key 13 'my-rss-search)

(defvar emacspeak-websearch-my-rss-search-uri
  "http://myrss.com/cgi-bin/search.cgi?__mod=search&__act=&search=Search+Channels&s="
  "URI for My RSS search.")

(defun emacspeak-websearch-my-rss-search (query)
  "My RSS search."
  (interactive
   (list
    (emacspeak-websearch-read-query "My RSS: ")))
  (declare (special emacspeak-websearch-my-rss-search-uri))
  (browse-url
   (format "%s%s"
           emacspeak-websearch-my-rss-search-uri
           query))
  (emacspeak-websearch-post-process query
                                    'emacspeak-speak-line))

;;}}}
;;{{{ Shopping at Amazon

(emacspeak-websearch-set-searcher 'amazon-search
                                  'emacspeak-websearch-amazon-search)

(emacspeak-websearch-set-key 1 'amazon-search)

(defvar emacspeak-websearch-amazon-search-form
  "http://www.amazon.com/access"
  "Form for Amazon store search.")

(defun emacspeak-websearch-amazon-search ()
  "Amazon search."
  (interactive)
  (declare (special emacspeak-websearch-amazon-search-form))
  (browse-url emacspeak-websearch-amazon-search-form))

;;}}}
;;{{{ Shopping at ebay 

(emacspeak-websearch-set-searcher 'ebay-search
                                  'emacspeak-websearch-ebay-search)

(emacspeak-websearch-set-key 5 'ebay-search)

(defvar emacspeak-websearch-ebay-search-form
  (expand-file-name "xml-forms/ebay-search.xml"
                    emacspeak-lisp-directory)
  "Form for Ebay store search.")

(defun emacspeak-websearch-ebay-search ()
  "Ebay search."
  (interactive)
  (declare (special emacspeak-websearch-ebay-search-form))
  (emacspeak-websearch-display-form emacspeak-websearch-ebay-search-form))

;;}}}
;;{{{ Shoutcast

(emacspeak-websearch-set-searcher 'shoutcast-search
                                  'emacspeak-websearch-shoutcast-search)

(emacspeak-websearch-set-key ?S 'shoutcast-search)

(defvar emacspeak-websearch-shoutcast-search-form
  (expand-file-name "xml-forms/shoutcast-search.xml"
                    emacspeak-lisp-directory)
  "Form for Shoutcast  search.")

(defun emacspeak-websearch-shoutcast-search ()
  "Shoutcast search."
  (interactive)
  (declare (special emacspeak-websearch-shoutcast-search-form))
  (emacspeak-websearch-display-form emacspeak-websearch-shoutcast-search-form))

;;}}}
;;{{{  site-specific search tools

;;; Load site-specific searchers 

(when (locate-library "emacspeak-w3search")
  (load-library "emacspeak-w3search"))

;;}}}
;;{{{ Browse usenet 

(defvar emacspeak-usenet-uri 
  "http://groups.google.com/groups?"
  "URI to open a group on Usenet archive.")
;;;###autoload
(defun emacspeak-websearch-usenet (group &optional prefix)
  "Prompt and browse a Usenet newsgroup.
Optional interactive prefix arg results in prompting for a search term."
  (interactive
   (list
    (read-from-minibuffer "Newsgroup: ")
    current-prefix-arg))
  (declare (special emacspeak-usenet-uri))
  (let ((url nil))
    (cond
     (prefix                            ;search
      (setq url
            (concat emacspeak-usenet-uri
                    (format "meta=group%%3D%s&q=%s&scoring=d"
                            group
                            (webjump-url-encode
			     (read-from-minibuffer
			      (format "Search %s for:" group)))))))
     (t                                 ;browse
      (setq url 
            (concat emacspeak-usenet-uri
                    (format "as_ugroup=%s" group)
                    ))))
    (browse-url  url)
    (emacspeak-websearch-post-process
     (if prefix
         "Sort by " 
       "Threads" )
     'emacspeak-speak-line)))

;;}}}

;;}}}
(provide 'emacspeak-websearch)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
