;;; emacspeak-feeds.el --- Feeds Support (Atom, RSS) For Emacspeak
;;; $Id: emacspeak-webutils.el 8904 2014-03-21 20:28:21Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak Feeds Support 
;;; Keywords: Emacspeak, RSS, Atom
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2008-08-14 11:23:31 -0700 (Thu, 14 Aug 2008) $ |
;;;  $Revision: 4634 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1999, 2011 T. V. Raman <raman@cs.cornell.edu>
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
;;; This module provides Feeds support for Emacspeak

;;}}}
;;{{{ required modules

;;; Code:
(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'emacspeak-webutils)
(require 'url)
(require 'gfeeds)
(require 'browse-url)

;;}}}
;;{{{  feed cache

;;;###autoload
(defgroup emacspeak-feeds nil
  "RSS Feeds for the Emacspeak desktop."
  :group 'emacspeak)

(defcustom emacspeak-opml-view-xsl
  (expand-file-name "opml.xsl" emacspeak-xslt-directory)
  "XSL stylesheet used for viewing OPML  Feeds."
  :type  'file
  :group 'emacspeak-xsl)

(defcustom emacspeak-rss-view-xsl
  (expand-file-name "rss.xsl" emacspeak-xslt-directory)
  "XSL stylesheet used for viewing RSS Feeds."
  :type  'file
  :group 'emacspeak-xsl)

(defcustom emacspeak-atom-view-xsl
  (expand-file-name "legacy-atom.xsl" emacspeak-xslt-directory)
  "XSL stylesheet used for viewing Atom Feeds."
  :type '(choice
          (string :tag "Legacy"  (expand-file-name "legacy-atom.xsl" emacspeak-xslt-directory))
          (string :tag "Modern" (expand-file-name "atom-view.xsl" emacspeak-xslt-directory)))
  :group 'emacspeak-xsl)

;;;###autoload
(defcustom emacspeak-feeds
  '(
    ("Wired News" "http://www.wired.com/news_drop/netcenter/netcenter.rdf"  rss)
    ("BBC Podcast Directory" "http://www.bbc.co.uk/podcasts.opml" opml)
    ("BBC News"  "http://www.bbc.co.uk/syndication/feeds/news/ukfs_news/front_page/rss091.xml"  rss)
    ("CNet Tech News"  "http://feeds.feedburner.com/cnet/tcoc"  rss)
    )
  "Table of RSS feeds."
  :type '(repeat
          (list :tag "Feed"
                (string :tag "Title")
                (string :tag "URI")
                (choice :tag "Type"
                        (const :tag "RSS" rss)
                        (const :tag "opml" opml)
                        (const :tag "Atom" atom))))
  :initialize  'custom-initialize-reset
  :set
  #'(lambda (sym val)
      (set-default
       sym
       (sort val #'(lambda (a b)
                     (string-lessp (first a) (first b))))))
  :group 'emacspeak-feeds)

(defun emacspeak-feeds-add-feed (title url type)
  "Add specified feed to our feed store."
  (interactive
   (list
    (read-from-minibuffer "Title: ")
    (read-from-minibuffer "URL: ")
    (ecase (read-char-exclusive "a Atom, o OPML, r RSS: ")
      (?a 'atom)
      (?o 'opml)
      (?r 'rss))))
  (declare (special emacspeak-feeds))
  (let ((found
         (find-if #'(lambda (f) (string= url (second f))) emacspeak-feeds)))
    (cond
     (found
      (message "Feed already present  as %s" (first found)))
     (t (pushnew
         (list title url type)
         emacspeak-feeds
         :test #'(lambda (a b) (string= (second a) (second b))))
        (let ((dtk-quiet t))
          (customize-save-variable 'emacspeak-feeds emacspeak-feeds))
        (message "Added feed as %s" title)))))
(defvar emacspeak-feeds-archive-file
  (expand-file-name "feeds.el" emacspeak-resource-directory)
  "Feed archive.")

;;;###autoload
(defun emacspeak-feeds-archive-feeds ()
  "Archive list of subscribed fees to personal resource directory.
Archiving is useful when synchronizing feeds across multiple machines."
  (interactive)
  (declare (special emacspeak-feeds-archive-file
                    emacspeak-speak-messages emacspeak-feeds))
  (let ((buffer (find-file-noselect emacspeak-feeds-archive-file))
        (emacspeak-speak-messages nil)
        (print-level nil)
        (print-length nil))
    (with-current-buffer buffer
      (erase-buffer)
      (cl-prettyprint emacspeak-feeds)
      (save-buffer)
      (emacspeak-auditory-icon 'save-object)))
  (message "Archived emacspeak-feeds containing %d feeds in %s"
           (length emacspeak-feeds)
           emacspeak-feeds-archive-file))

;;;###autoload
(defun emacspeak-feeds-restoere-feeds ()
  "Restore list of subscribed fees from  personal resource directory.
Archiving is useful when synchronizing feeds across multiple machines."
  (interactive)
  (declare (special emacspeak-feeds-archive-file
                    emacspeak-feeds))
  (unless (file-exists-p emacspeak-feeds-archive-file)
    (error "No archived feeds to restore. "))
  (let ((buffer (find-file-noselect emacspeak-feeds-archive-file))
        (feeds  nil)
        (emacspeak-speak-messages nil))
    (with-current-buffer buffer
      (goto-char (point-min))
      (setq feeds (read buffer)))
    (kill-buffer buffer)
    (loop for f in feeds
          do
          (apply #'emacspeak-feeds-add-feed f))
    (when
        (y-or-n-p
         (format "After restoring %d feeds, we have a total of %d feeds. Save? "
                 (length feeds) (length emacspeak-feeds)))
      (customize-save-variable 'emacspeak-feeds emacspeak-feeds))))

;;}}}
;;{{{ display  feeds:

(defun emacspeak-feeds-feed-display(feed-url style &optional speak)
  "Fetch feed via Emacs and display using xsltproc."
  (declare (special emacspeak-eww-buffer-hash))
  (cond
   ((and (eq browse-url-browser-function 'eww-browse-url)
         (boundp 'emacspeak-eww-buffer-hash)
         emacspeak-eww-buffer-hash
         (gethash   feed-url emacspeak-eww-buffer-hash)
         (buffer-live-p (gethash   feed-url emacspeak-eww-buffer-hash)))
    (switch-to-buffer (gethash feed-url emacspeak-eww-buffer-hash))
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-rest-of-buffer))
   (t
    (let ((buffer (url-retrieve-synchronously feed-url))
          (coding-system-for-read 'utf-8)
          (coding-system-for-write 'utf-8)
          (emacspeak-xslt-options nil))
      (cond
       ((null buffer) (message "Nothing to display."))
       (t
        (when speak (emacspeak-webutils-autospeak))
        (add-hook
         'emacspeak-web-post-process-hook
         #'(lambda ()
             (declare (special eww-current-url
                               emacspeak-eww-feed
                               emacspeak-eww-style))
             (lexical-let ((u feed-url)
                           (s style))
               (setq eww-current-url u
                     emacspeak-eww-feed t 
                     emacspeak-eww-style s))))
        (with-current-buffer buffer
          (emacspeak-webutils-without-xsl
           (goto-char (point-min))
           (search-forward "\n\n")
           (delete-region (point-min) (point))
           (decode-coding-region (point-min) (point-max) 'utf-8)
           (emacspeak-xslt-region
            style (point-min) (point-max)
            (list (cons "base" (format "\"'%s'\"" feed-url)))))
          (browse-url-of-buffer))))))))

;;;###autoload
(defun emacspeak-feeds-rss-display (feed-url )
  "Display RSS feed."
  (interactive
   (list
    (emacspeak-webutils-read-this-url)))
  (declare (special emacspeak-rss-view-xsl))
  (emacspeak-feeds-feed-display feed-url emacspeak-rss-view-xsl 'speak))
;;;###autoload
(defun emacspeak-feeds-opml-display (feed-url )
  "Display OPML feed."
  (interactive (list (emacspeak-webutils-read-this-url)))
  (declare (special emacspeak-opml-view-xsl))
  (emacspeak-feeds-feed-display feed-url emacspeak-opml-view-xsl 'speak))

;;;###autoload
(defun emacspeak-feeds-atom-display (feed-url )
  "Display ATOM feed."
  (interactive (list (emacspeak-webutils-read-this-url)))
  (declare (special emacspeak-atom-view-xsl))
  (emacspeak-feeds-feed-display feed-url emacspeak-atom-view-xsl 'speak))

;;}}}
;;{{{  view feed

;;; Helper:
(defun emacspeak-feeds-browse-feed (feed &optional speak)
  "Display specified feed.
Argument `feed' is a feed structure (label url type)."
  (let ((uri (second feed))
        (type  (third feed))
        (style nil))
    (setq style
          (cond
           ((eq type 'rss)emacspeak-rss-view-xsl)
           ((eq type 'opml) emacspeak-opml-view-xsl)
           ((eq type 'atom) emacspeak-atom-view-xsl)
           (t (error "Unknown feed type %s" type))))
    (emacspeak-feeds-feed-display uri style speak)))

;;;###autoload
(defun emacspeak-feeds-browse (feed)
  "Browse specified  feed."
  (interactive
   (list
    (let ((completion-ignore-case t))
      (completing-read "Feed:"
                       emacspeak-feeds))))
  (emacspeak-feeds-browse-feed
   (assoc feed emacspeak-feeds)
   'speak))

;;;###autoload
(defun emacspeak-feeds-lookup-and-view  (site)
  "Lookup feed URL for a site and browse result."
  (interactive
   (list
    (read-from-minibuffer "Site: " (browse-url-url-at-point))))
  (emacspeak-webutils-autospeak)
  (gfeeds-view site 'lookup))

;;}}}
;;{{{ Finding Feeds:

(define-button-type 'emacspeak-feeds-feed-button
  'follow-link t
  'link nil)

;;;###autoload
(defun emacspeak-feeds-find-feeds (query)
  "Browse feeds matching specified query."
  (interactive "sFind Feeds Matching: ")
  (with-current-buffer (get-buffer-create "*Feeds*")
    (setq buffer-undo-list t)
    (let ((inhibit-read-only t)
          (start (point-min))
          (results (gfeeds-find query)))
      (message "Found %s feeds" (length results))
      (unless results (error "No feeds found matching %s" query))
      (erase-buffer)
      (goto-char start)
      (insert (format "Feeds Matching %s\n\n" query))
      (put-text-property start (point) 'face font-lock-doc-face)
      (loop
       for f across results
       and position  from 1
       do
       (insert (format "%d\t" position))
       (insert-text-button
        (emacspeak-webutils-html-string (cdr (assoc 'title f)))
        'link  (cdr (assoc 'url f))
        'type 'emacspeak-feeds-feed-button)
       (insert "\n")
       (setq start (point))
       (insert (emacspeak-webutils-html-string (cdr (assoc 'contentSnippet f))))
       (fill-region start (point))
       (insert "\n")
       (emacspeak-webspace-mode))))
  (switch-to-buffer "*Feeds*")
  (goto-char (point-min))
  (emacspeak-auditory-icon 'open-object)
  (emacspeak-speak-mode-line))

;;}}}
(provide 'emacspeak-feeds)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
