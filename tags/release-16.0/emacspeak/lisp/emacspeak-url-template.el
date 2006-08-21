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

;;; Copyright (C) 1995 -- 2002, T. V. Raman<raman@cs.cornell.edu>
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
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
(eval-when-compile (require 'webjump))
;;}}}
;;{{{  Introduction:

;;; Commentary:


;;; It is often useful to have ``parameterized hot list entries''
;;; i.e., hotlist entries  that are ``templates'' for the
;;; actual URL.
;;; The user provides values for the parameterized portons
;;; of the URL e.g. the date.

;;}}}
;;{{{  structures 

(defstruct (emacspeak-url-template
            (:constructor emacspeak-url-template-constructor))
  name                                  ;Human-readable name
  template                              ;template URL string 
  generators                            ; list of param generator
  post-action                           ;action to perform after opening
  documentation                         ;resource  documentation
  fetcher                               ; custom fetcher 
  )

;;}}}
;;{{{ Helpers

(defun emacspeak-url-template-url (ut)
  "Instnatiate URL identified by URL template."
  (apply 'format
         ( emacspeak-url-template-template ut)
         (mapcar
          (function
           (lambda (g)
             (cond
              ((stringp g)
               (read-from-minibuffer g))
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

(defun emacspeak-url-template-get (key)
  "Lookup key and return corresponding template. "
  (declare (special emacspeak-url-template-table))
  (gethash key emacspeak-url-template-table))

;;}}}
;;{{{  define resources 

(defun emacspeak-url-template-define (name template
                                           &optional generators
                                           post-action
                                           documentation fetcher)
  "Define a URL template."
  (declare (special emacspeak-url-template-table))
  (emacspeak-url-template-set
   name
   (emacspeak-url-template-constructor :name name
                                       :template template
                                       :generators generators
                                       :post-action post-action
                                       :documentation
                                       documentation
                                       :fetcher fetcher)))
                               

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
;;{{{  KLIV news 

(emacspeak-url-template-define
 "KLIV News" 
 "http://newsclicker.com/wire/?200102170000023087&501"
 nil
 nil
 "Display headline news from KLIV 1590 San Jose")

;;}}}
;;{{{ google news overview 

(emacspeak-url-template-define
 "Google News Overview"
 "http://www.google.com/news/newsheadlines.html"
 nil
 #'(lambda nil
     (search-forward "Top Headlines")
     (emacspeak-speak-rest-of-buffer))
 "Retrieve and speak Google News Overview.")

;;}}}
;;{{{ yahoo daily news 
(emacspeak-url-template-define
 "My Yahoo "
 "http://my.yahoo.com"
 nil
 nil
 "Apply content.xsl to my.yahoo.com and speak the relevant contents."
 #'(lambda (url)
     (declare (special emacspeak-xslt-directory
                       emacspeak-w3-url-rewrite-rule))
     (emacspeak-wizards-browse-url-with-style
      (expand-file-name "content.xsl"
                        emacspeak-xslt-directory)
      url)
     (search-forward
      (format-time-string "%A") nil t)
     (setq emacspeak-w3-url-rewrite-rule
           '("$" "&print=1"))
     (beginning-of-line)
     (emacspeak-speak-rest-of-buffer)))

(emacspeak-url-template-define
 "Yahoo Politics"
 "http://dailynews.yahoo.com/htx/pl/?u"
 nil
 #'(lambda nil
     (search-forward "Sources:")
     (forward-line 2)
     (emacspeak-speak-rest-of-buffer))
 "Retrieve and speak Politics section from Yahoo Daily News.")

(emacspeak-url-template-define
 "Yahoo Entertainment"
 "http://dailynews.yahoo.com/htx/en/?u"
 nil
 #'(lambda nil
     (search-forward "Sources:")
     (forward-line 3)
     (emacspeak-speak-rest-of-buffer))
 "Retrieve and speak Entertainment section from Yahoo Daily News.")

(emacspeak-url-template-define
 "Yahoo Sports"
 "http://dailynews.yahoo.com/htx/sp/?u"
 nil
 #'(lambda nil
     (search-forward "Sources:")
     (forward-line 2)
     (emacspeak-speak-rest-of-buffer))
 "Retrieve and speak Sports section from Yahoo Daily News.")

(emacspeak-url-template-define
 "Yahoo Business News"
 "http://story.news.yahoo.com/news?tmpl=index&cid=749"
 nil
 #'(lambda nil
     (declare (special  emacspeak-w3-url-rewrite-rule))
     (setq emacspeak-w3-url-rewrite-rule
           '("$" "&print=1"))
     (search-forward "News Stories")
     (emacspeak-w3-next-doc-element)
     (w3-table-move-to-table-start)
     (w3-table-move-to-next-table-column)
     (w3-table-speak-this-cell))
 "Retrieve and speak business  section from Yahoo Daily News."
 #'(lambda (url)
     (declare (special emacspeak-w3-xsl-p
                       emacspeak-w3-xsl-transform))
     (let ((emacspeak-w3-xsl-p nil))
       (w3-fetch url))))

(emacspeak-url-template-define
 "Yahoo Science"
 "http://dailynews.yahoo.com/htx/sc/nm/?u"
 nil
 #'(lambda nil
     (search-forward "Sources:")
     (forward-line 2)
     (emacspeak-speak-rest-of-buffer))
 "Retrieve and speak Science section from Yahoo Daily News.")


(emacspeak-url-template-define
 "Yahoo Local"
 "http://dailynews.yahoo.com/htx/lo/me/%s/"
 (list
  #'(lambda nil
      (read-from-minibuffer
       "Two-letter Area Code")))
 #'(lambda nil
     (search-forward "Sources:")
     (forward-line 3)
     (emacspeak-speak-rest-of-buffer))
 "Retrieve and speak Local
section from Yahoo Daily News.
Area is specified as a two-letter code --sf for San
Francisco ny for New york etc.")

(emacspeak-url-template-define
 "Yahoo Top Stories"
 "http://dailynews.yahoo.com/htx/ts/?u"
 nil
 #'(lambda nil
     (search-forward
      "Sources:" nil t)
     (forward-line 3)
     (emacspeak-speak-rest-of-buffer))
 "Retrieve and speak Top Stories  section from Yahoo Daily News.")

(emacspeak-url-template-define
 "Yahoo Health"
 "http://dailynews.yahoo.com/htx/hl/?u"
 nil
 #'(lambda nil
     (search-forward
      "Sources:" nil t)
     (forward-line 2)
     (emacspeak-speak-rest-of-buffer))
 "Retrieve and speak Health section from Yahoo Daily News.")

(emacspeak-url-template-define
 "Yahoo Oddly"
 "http://dailynews.yahoo.com/htx/od//?u"
 nil
 #'(lambda nil
     (search-forward
      "Sources:" nil t)
     (forward-line 2)
     (emacspeak-speak-rest-of-buffer))
 "Retrieve and speak Oddity section from Yahoo Daily News.")


(emacspeak-url-template-define
 "Yahoo Technology  News"
                                        ;"http://dailynews.yahoo.com/htx/tc/nm/?u"
 "http://dailynews.yahoo.com/h/tc/?u"
 nil
 #'(lambda nil
     (declare (special  emacspeak-w3-url-rewrite-rule))
     (setq emacspeak-w3-url-rewrite-rule
           '("$" "&print=1"))
     (search-forward "News Stories")
     (emacspeak-w3-next-doc-element)
     (w3-table-move-to-table-start)
     (w3-table-move-to-next-table-column)
     (w3-table-speak-this-cell))
 "Retrieve and speak Technology  section from Yahoo Daily News."
 #'(lambda (url)
     (declare (special emacspeak-w3-xsl-p
                       emacspeak-w3-xsl-transform))
     (let ((emacspeak-w3-xsl-p nil))
       (w3-fetch url))))

;;}}}
;;{{{ Adobe pdf conversion 

(emacspeak-url-template-define
 "pdf2html"
 "http://access.adobe.com/perl/convertPDF.pl?url=%s"
 (list
  #'(lambda ()
      (webjump-url-encode
       (read-from-minibuffer "PDF URL: "))))
 nil
 "Use access.adobe.com to  convert a remote PDF document to
HTML.
The PDF document needs to be available on the public Internet.")

;;}}}
;;{{{ oasis 
(emacspeak-url-template-define
 "OASIS  Lists"
 "http://lists.oasis-open.org/archives/%s/%s/maillist.html"
 (list
  #'(lambda ()
      (read-from-minibuffer "OASIS Group: "))
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
 "http://lists.w3.org/Archives/Member/w3c-%s/%s/"
 (list
  'emacspeak-url-template-get-w3c-group 
  'emacspeak-url-template-get-w3c-year/month)
 nil
 "Use this to pull up the
archived  mail from the W3C list. You need to know the exact
name of the list.")

(defun emacspeak-url-template-get-w3c-group ()
  "Get name of W3C group "
  (read-from-minibuffer "W3C group: "))

(defun emacspeak-url-template-get-w3c-year/month ()
  "Get year/month"
  (read-from-minibuffer "Date range: "
                        (downcase 
                         (format-time-string "%Y%h"
                                             (current-time)))))

;;}}}
;;{{{ cnn 

(emacspeak-url-template-define
 "CNN Weather "
 "http://weather.cnnaudience.com/cgi-bin/weather/redirect?zip=%s"
 (list
  #'(lambda nil
      (read-from-minibuffer "US Zip Code: ")))
 nil
 "Weather Forecast from CNN"
 #'(lambda (url)
     (emacspeak-w3-extract-table 5  url)))

(emacspeak-url-template-define
 "CNN headlines "
 "http://www.cnn.com/QUICKNEWS/print.html"
 nil
 #'(lambda nil
     (search-forward "TOP STORIES" nil t)
     (forward-line 1)
     (emacspeak-speak-rest-of-buffer))
 "Retrieve and speak headline news from CNN.")

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
 "http://www.cnn.com/2002/TECH/science/%s/index.html"
 (list
  'emacspeak-url-template-date-month/date)
 nil
 "Browse to the plain index of
technology articles at CNN.")

(emacspeak-url-template-define
 "CNN computing "
 "http://www.cnn.com/2002/TECH/computing/%s/index.html"
 (list
  'emacspeak-url-template-date-month/date)
 nil
 "Browse to the plain index of
Computing News at CNN.")

(emacspeak-url-template-define
 "CNN HotStocks "
 "http://money.cnn.com/%s/markets/hotstox/"
 (list 
  'emacspeak-url-template-date-year/month/date))


(emacspeak-url-template-define
 "CNN Markets New York"
 "http://money.cnn.com/%s/markets/markets_newyork/"
 (list 'emacspeak-url-template-date-year/month/date)
 #'(lambda nil
     (declare (special emacspeak-w3-xsl-p
                       emacspeak-xslt-directory
                       emacspeak-w3-xsl-transform))
     (let ((emacspeak-w3-xsl-p t)
           (emacspeak-w3-xsl-transform (expand-file-name
                                        "sort-tables.xsl"
                                        emacspeak-xslt-directory)))
       (goto-char (point-min))
       (search-forward (format-time-string "%B"))
       (w3-table-speak-this-cell)))
 "Speak CNN Market Update.")


;;}}}
;;{{{ technet cast from DDJ



(emacspeak-url-template-define
 "TechNetCast Save" 
 "http://technetcast.ddj.com/tnc_save_mp3.html?stream_id=%s"
 (list
  (lambda nil 
    (read-from-minibuffer "Download Stream ")))
 nil
 "Browse to a specified DDJ Technetcast stream and save  it.")

(emacspeak-url-template-define
 "TechNetCast Play" 
 "http://technetcast.ddj.com/tnc_play.m3u?stream_id=%s"
 (list
  (lambda nil 
    (read-from-minibuffer "Stream Id")))
 nil
 "Play Technetcast stream from DDJ.")

;;}}}
;;{{{ sourceforge
(emacspeak-url-template-define
 "sourceforge Stats" 
 "http://sourceforge.net/project/stats?group_id=%s"
 (list
  (lambda nil 
    (read-from-minibuffer "Project Id"
                          "2238")))
 nil
 "Display project usage statistics."
 #'(lambda (url)
     (emacspeak-w3-extract-table 9 url)))

(emacspeak-url-template-define
 "sourceforge project" 
 "http://sourceforge.net/projects/%s"
 (list
  (lambda nil 
    (read-from-minibuffer "Project name")))
 nil
 "Open specified project page at SourceForge.")

(emacspeak-url-template-define
 "sourceforge download" 
 "http://prdownloads.sourceforge.net/%s"
 (list
  (lambda nil 
    (read-from-minibuffer "Project name")))
 nil
 "Retrieve download page at Sourceforge for specified project.")

;;}}}
;;{{{  Virtually There --Sabre Trip Reports 
(emacspeak-url-template-define
 "Sabre Travel From Virtually There" 
 "https://www.virtuallythere.com/new/reservations.html?pnr=%s&name=%s&style=3&language=0&clocktype=12&host=1W"
                                        ;"https://www.virtuallythere.com/cgi-bin/nph-itinerary?pnr=%s&name=%s&language=0&host=1W&clocktype=12"
 (list
  (lambda nil 
    (read-from-minibuffer "Record Locator: "))
  (lambda nil 
    (read-from-minibuffer "User Name")))
 nil
 "Display Trip Details"
                                        ; #'(lambda (url)
                                        ;      (let ((temp-file (format "/tmp/sabre-%s.html" (gensym))))
                                        ;        (shell-command
                                        ;         (format "lynx -base '%s' -source '%s' > %s"
                                        ;                 url url temp-file))
                                        ;        (w3-open-local temp-file)
                                        ;        (delete-file temp-file)))
 )


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


;;}}}
;;{{{ India Today 

(emacspeak-url-template-define
 "India Today "
 "http://www.india-today.com/itoday/%s/index.shtml"
 (list
  'emacspeak-url-template-date-YearMonthDate)
 nil
 "Retrieve India Today. Published every Monday --specified appropriate date.")

;;}}}

;;}}}
;;{{{ Interactive commands 

(defun emacspeak-url-template-open (ut)
  "Fetch resource identified by URL template."
  (let
      ((fetcher (or (emacspeak-url-template-fetcher ut)
                    'browse-url)))
    (funcall fetcher   (emacspeak-url-template-url ut))
    (when (emacspeak-url-template-post-action ut)
      (funcall (emacspeak-url-template-post-action ut)))))

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

(defun emacspeak-url-template-fetch (&optional documentation)
  "Fetch a pre-defined resource.
Use Emacs completion to obtain a list of available
resources.
Resources typically prompt for the relevant information
before completing the request.
Optional interactive prefix arg displays documentation for specified resource."
  (interactive "P")
  (declare (special emacspeak-url-template-table
                    url-be-asynchronous emacspeak-speak-messages))
  (let ((completion-ignore-case t)
        (emacspeak-speak-messages nil)
        (url-be-asynchronous nil)
        (name nil)
        (table
         (loop for key being the hash-keys of
               emacspeak-url-template-table
               collect (list 
                        (format "%s" key)
                        (format "%s" key)))))
    (setq name (completing-read "Resource: "
                                table))
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

