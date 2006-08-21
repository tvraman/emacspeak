;;; emacspeak-rss.el --- Emacspeak RSS Wizard
;;; $Id$
;;; $Author$
;;; Description:  RSS Wizard for the emacspeak desktop
;;; Keywords: Emacspeak,  Audio Desktop RSS
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

;;{{{  introduction

;;; Simple RSS wizard for Emacspeak

;;}}}
;;{{{  Required modules

(require 'emacspeak-preamble)
(require 'browse-url)
;;}}}
;;{{{ RSS feed cache
;;;###autoload
(defgroup emacspeak-rss nil
  "RSS Feeds for the Emacspeak desktop."
  :group 'emacspeak)

(defcustom emacspeak-rss-feeds
  '(
    ("Wired News" "http://www.wired.com/news_drop/netcenter/netcenter.rdf")
    ("BBC News"  "http://www.bbc.co.uk/syndication/feeds/news/ukfs_news/front_page/rss091.xml")
    ("CNet Tech News"  "http://rss.com.com/2547-12-0-5.xml")
    ("XML.COM"  "http://www.xml.com/xml/news.rss")
    )
  "Table of RSS feeds."
  :type '(repeat
	  (list :tag "RSS Feed"
		(string :tag "Title")
		(string :tag "URI")))
  :group 'emacspeak-rss)

;;}}}
;;{{{  view feed

;;;###autoload
(defun emacspeak-rss-display (rss-url &optional speak)
  "Retrieve and display RSS news feed."
  (interactive
   (list
    (car
     (browse-url-interactive-arg "RSS Feed: "))))
  (declare (special emacspeak-xslt-directory))
  (when (or (interactive-p)speak)
    (add-hook 'emacspeak-w3-post-process-hook
	      'emacspeak-speak-buffer))
  (emacspeak-w3-browse-xml-url-with-style
   (expand-file-name "rss.xsl" emacspeak-xslt-directory)
   rss-url
   'unescape-charent))

;;;###autoload
(defun emacspeak-rss-browse (feed)
  "Browse specified RSS feed."
  (interactive
   (list
    (let ((completion-ignore-case t))
      (completing-read "Feed:"
		       emacspeak-rss-feeds))))
  (let ((uri (cadr
              (assoc feed emacspeak-rss-feeds))))
    (emacspeak-rss-display uri 'speak)))

;;}}}
(provide 'emacspeak-rss)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
