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
;;;Copyright (C) 1995 -- 2002, T. V. Raman 
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
              (move-to-column-force 16 )
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
  (let (
	)
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
  (let (
	)
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
  (let (
	)
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
  (let (
	)
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
  (let (
	)
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

(defvar emacspeak-websearch-appwatch-search-uri 
  "http://appwatch.com/Linux/Users/find?q="
  "URI for searching Freshmeat site. ")

(defun emacspeak-websearch-appwatch-search (query)
  "Search AppWatch  Site. "
  (interactive
   (list
    (emacspeak-websearch-read-query "Search AppWatch  for: ")))
  (declare (special emacspeak-websearch-appwatch-search-uri))
  (let (
	)
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
  (let (
	)
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

(defvar emacspeak-websearch-google-uri
  "http://www.google.com/search?q="
  "*URI for Google search")

(defcustom emacspeak-websearch-google-feeling-lucky-p nil
  "If non-nil, then Google search will use the 
I'm Feeling Lucky button by default."
  :type 'boolean 
  :group 'emacspeak-websearch)

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
  (declare (special emacspeak-websearch-google-uri))
  (let ((lucky-flag (if emacspeak-websearch-google-feeling-lucky-p
                        (not lucky)
                      lucky))
        (emacspeak-w3-xsl-p nil))
    (browse-url 
     (concat emacspeak-websearch-google-uri
             (webjump-url-encode query)
             (when lucky-flag
               (concat 
                "&btnI="
                (webjump-url-encode
                 "I'm Feeling Lucky")))))
    (if lucky-flag
        (emacspeak-speak-line)
      (emacspeak-websearch-post-process
       "Results"
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
  (let (
	)
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
  (let (
	)
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
                    emacspeak-websearch-map-maps-uri))
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
    (emacspeak-w3-extract-table-by-position 20
					    (concat
					     emacspeak-websearch-map-directions-uri
					     query)
					    'speak))))
         
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
   "/html/body/table[6]//p"
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
  (let (
	)
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

(emacspeak-websearch-set-key ?R 'recorded-books)

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
  (let (
	)
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
  (let (
	)
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
  (let (
	)
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
  "http://www.weather.com/weather/tendayprint/"
  "*URI for getting weather forecast.")

(defun emacspeak-websearch-weather (zip)
  "Get weather forecast for specified zip code."
  (interactive
   (list (emacspeak-websearch-read-query "Zip Code: ")))
  (declare (special emacspeak-websearch-weather-uri))
  (emacspeak-w3-extract-nested-table-list
   (list 4 5)
   (concat emacspeak-websearch-weather-uri
	   zip)
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
;;{{{ streaming audio 

(emacspeak-websearch-set-searcher 'streaming-audio
                                  'emacspeak-websearch-real-tuner)

(emacspeak-websearch-set-key 19'streaming-audio)

(defvar emacspeak-websearch-real-tuner-form
  (expand-file-name "xml-forms/real-radio-tuner.xml"
                    emacspeak-lisp-directory)
  "Real tuner from Real Networks.")

(defun emacspeak-websearch-real-tuner ()
  "Search using Real Tuner from Real Networks."
  (interactive)
  (declare (special emacspeak-websearch-real-tuner-form))
  (emacspeak-websearch-display-form
   emacspeak-websearch-real-tuner-form))

(defvar emacspeak-websearch-streaming-audio-search-uri 
  "http://www.billsparks.org/links/search.cgi?query="
  "URI for searching for streaming audio.")

(defun emacspeak-websearch-streaming-audio-search (query)
  "Search for streaming audio. "
  (interactive
   (list
    (emacspeak-websearch-read-query "Audio Search: ")))
  (declare (special emacspeak-websearch-streaming-audio-search-uri))
  (let (
	)
    (browse-url
     (concat
      emacspeak-websearch-streaming-audio-search-uri
      (webjump-url-encode query)))
    (emacspeak-websearch-post-process
     query
     'emacspeak-speak-line)))

;;}}}
;;{{{ Exchange rate convertor 

(emacspeak-websearch-set-searcher 'exchange-rate-convertor
                                  'emacspeak-websearch-exchange-rate-convertor)

(emacspeak-websearch-set-key ?x 'exchange-rate-convertor)

(defvar emacspeak-websearch-exchange-rate-form 
  (expand-file-name "xml-forms/exchange-rate-convertor.xml"
                    emacspeak-lisp-directory)
  "Form for performing currency conversion.")

(defun emacspeak-websearch-exchange-rate-convertor ()
  "Currency convertor."
  (interactive)
  (declare (special emacspeak-websearch-exchange-rate-form))
  (emacspeak-websearch-display-form emacspeak-websearch-exchange-rate-form))

;;}}}
;;{{{ Shopping at Amazon

(emacspeak-websearch-set-searcher 'amazon-search
                                  'emacspeak-websearch-amazon-search)

(emacspeak-websearch-set-key 1 'amazon-search)

(defvar emacspeak-websearch-amazon-search-form
  (expand-file-name "xml-forms/amazon-search.xml"
                    emacspeak-lisp-directory)
  "Form for Amazon store search.")

(defun emacspeak-websearch-amazon-search ()
  "Amazon search."
  (interactive)
  (declare (special emacspeak-websearch-amazon-search-form))
  (emacspeak-websearch-display-form emacspeak-websearch-amazon-search-form))

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
  "Ebay search."
  (interactive)
  (declare (special emacspeak-websearch-shoutcast-search-form))
  (emacspeak-websearch-display-form emacspeak-websearch-shoutcast-search-form))

;;}}}

;;{{{  site-specific search tools

;;; Load site-specific searchers 

(when (locate-library "emacspeak-w3search")
  (load-library "emacspeak-w3search"))

;;}}}

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
  (let (
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
    (emacspeak-websearch-post-process
     (if prefix
         "Sorted " 
       "Threads" )
     'emacspeak-speak-line)))

;;}}}
(provide 'emacspeak-websearch)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
