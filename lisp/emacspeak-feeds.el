;;; emacspeak-feeds.el --- Feeds Support (Atom, RSS) For Emacspeak  -*- lexical-binding: t; -*-
;;; $Id:$
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak Feeds Support 
;;; Keywords: Emacspeak, RSS, Atom
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2008-08-14 11:23:31 -0700 (Thu, 14 Aug 2008) $ |
;;;  $Revision: 4634 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1995 -- 2021, T. V. Raman
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
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  Introduction:

;;; Commentary:
;;; This module provides Feeds support for Emacspeak

;;}}}
;;{{{ required modules

;;; Code:
(require 'cl-lib)
(require 'cl-extra)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'emacspeak-xslt)
(require 'url)
(require 'eww)
(require 'browse-url)

;;}}}
;;{{{  feed cache


(defgroup emacspeak-feeds nil
  "RSS Feeds for the Emacspeak desktop."
  :group 'emacspeak)

(defvar emacspeak-feeds-feeds-table (make-hash-table :test #'equal)
  "Hash table to enable efficient feed look up when adding feeds.")

(defun emacspeak-feeds-cache-feeds ()
  "Cache feeds in emacspeak-feeds in a hash table."
  (cl-declare (special emacspeak-feeds))
  (cl-loop
   for f in emacspeak-feeds
   do
   (set-text-properties 0 (length (cl-second f)) nil (cl-second f))
   (puthash
    (cl-second f); strip props 
    f emacspeak-feeds-feeds-table)))

(defcustom emacspeak-feeds
  '(
    ("Wired News" "http://www.wired.com/news_drop/netcenter/netcenter.rdf"  rss)
    ("BBC Podcast Directory" "http://www.bbc.co.uk/podcasts.opml" opml)
    ("BBC News"
     "http://www.bbc.co.uk/syndication/feeds/news/ukfs_news/front_page/rss091.xml"
     rss)
    ("CNet Tech News"  "http://feeds.feedburner.com/cnet/tcoc"  rss)
    )
  "Table of RSS/Atom feeds.
The feed list is persisted to file saved-feeds on exit."
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
                     (string-lessp (cl-first a) (cl-first b)))))
      (emacspeak-feeds-cache-feeds))
  :group 'emacspeak-feeds)

(add-hook
 'kill-emacs-hook
 #'(lambda nil
     (when (bound-and-true-p emacspeak-feeds)
       (emacspeak--persist-variable
        'emacspeak-feeds
        (expand-file-name "saved-feeds"
                          emacspeak-user-directory)))))

(defun emacspeak-feeds-added-p (feed-url)
  "Check if this feed has been added before."
  (cl-declare (special emacspeak-feeds-feeds-table))
  (gethash feed-url emacspeak-feeds-feeds-table))
;;;###autoload
(defun emacspeak-feeds-add-feed (title url type)
  "Add specified feed to our feed store."
  (interactive
   (list
    (read-from-minibuffer "Title: ")
    (read-from-minibuffer "URL: ")
    (cl-ecase (read-char-exclusive "a Atom, o OPML, r RSS: ")
      (?a 'atom)
      (?o 'opml)
      (?r 'rss))))
  (cl-declare (special emacspeak-feeds))
  (let ((found (emacspeak-feeds-added-p url)))
    (cond
     (found
      (message "Feed already present  as %s" (cl-first found)))
     (t (push (list title url type) emacspeak-feeds)
        (let ((dtk-quiet t))
          (customize-save-variable 'emacspeak-feeds emacspeak-feeds))
        (ems-with-messages-silenced
            (message "Added feed as %s" title))))))

(defvar emacspeak-feeds-archive-file
  (expand-file-name "feeds.el" emacspeak-user-directory)
  "Feed archive.")


(defun emacspeak-feeds-archive-feeds ()
  "Archive list of subscribed fees to personal resource directory.
Archiving is useful when synchronizing feeds across multiple machines."
  (interactive)
  (cl-declare (special emacspeak-feeds-archive-file
                       emacspeak-feeds))
  (let ((buffer (find-file-noselect emacspeak-feeds-archive-file))
        (print-level nil)
        (print-length nil))
    (with-current-buffer buffer
      (erase-buffer)
      (ems-with-messages-silenced (cl-prettyprint emacspeak-feeds))
      (save-buffer)
      (emacspeak-auditory-icon 'save-object)
      (message "Archived emacspeak-feeds containing %d feeds in %s"
               (length emacspeak-feeds)
               emacspeak-feeds-archive-file))))


(defun emacspeak-feeds-restore-feeds ()
  "Restore list of subscribed fees from  personal resource directory.
Archiving is useful when synchronizing feeds across multiple machines."
  (interactive)
  (cl-declare (special emacspeak-feeds-archive-file
                       emacspeak-feeds))
  (unless (file-exists-p emacspeak-feeds-archive-file)
    (error "No archived feeds to restore. "))
  (let ((buffer (find-file-noselect emacspeak-feeds-archive-file))
        (feeds  nil))
    (ems-with-messages-silenced
     (with-current-buffer buffer
       (goto-char (point-min))
       (setq feeds (read buffer))))
    (kill-buffer buffer)
    (cl-loop for f in feeds
             do
             (apply #'emacspeak-feeds-add-feed f))
    (when
        (y-or-n-p
         (format "After restoring %d feeds, we have a total of %d feeds. Save? "
                 (length feeds) (length emacspeak-feeds)))
      (customize-save-variable 'emacspeak-feeds emacspeak-feeds))))


(defun emacspeak-feeds-fastload-feeds ()
  "Fast load list of feeds from archive.
This directly updates emacspeak-feeds from the archive, rather
than adding those entries to the current set of subscribed
feeds."
  (interactive)
  (cl-declare (special emacspeak-feeds-archive-file emacspeak-feeds))
  (unless (file-exists-p emacspeak-feeds-archive-file)
    (error "No archived feeds to restore. "))
  (let ((buffer (find-file-noselect emacspeak-feeds-archive-file)))
    (setq emacspeak-feeds (read buffer))
    (kill-buffer buffer)
    (when
        (y-or-n-p
         (format "After restoring  we have a total of %d feeds. Save? "
                 (length emacspeak-feeds)))
      (customize-save-variable 'emacspeak-feeds emacspeak-feeds))))

;;}}}
;;{{{ display  feeds:

(defun emacspeak-feeds-feed-display(feed-url style &optional speak)
  "Fetch feed asynchronously via Emacs and display using xsltproc."
  (let ((read-process-output-max  (* 1024 1024)))
    (url-retrieve feed-url #'emacspeak-feeds-render (list feed-url  style  speak)))
  (message "pulling feed.")
  (emacspeak-auditory-icon 'item))

(defun emacspeak-feeds-render  (_status feed-url style   speak)
  "Render the result of asynchronously retrieving feed-url."
  (cl-declare (special  eww-data  eww-current-url
                        emacspeak-eww-feed emacspeak-eww-style))
  (let ((inhibit-read-only t)
        (browse-url-browser-function  'eww-browse-url)
        (data-buffer (current-buffer))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        (emacspeak-xslt-options "--nonet --novalid"))
    (with-current-buffer data-buffer
      (when speak (emacspeak-eww-autospeak))
      (add-hook
       'emacspeak-eww-post-process-hook
       #'(lambda ()
           (setq eww-current-url feed-url
                 emacspeak-eww-feed t 
                 emacspeak-eww-style style)
           (plist-put eww-data :url feed-url)))
      (goto-char (point-min))
      (search-forward "\n\n")
      (delete-region (point-min) (point))
      (decode-coding-region (point-min) (point-max) 'utf-8)
      (emacspeak-xslt-region
       style (point-min) (point-max)
       (list (cons "base" (format "\"'%s'\"" feed-url))))
      (setq eww-current-url feed-url
            emacspeak-eww-feed t 
            emacspeak-eww-style style)
      (emacspeak-xslt-without-xsl (browse-url-of-buffer)))))

;;;###autoload
(defun emacspeak-feeds-rss-display (feed-url)
  "Display RSS feed."
  (interactive
   (list
    (emacspeak-eww-read-url)))
  (cl-declare (special emacspeak-rss-view-xsl))
  (emacspeak-feeds-feed-display feed-url emacspeak-rss-view-xsl 'speak))

;;;###autoload
(defun emacspeak-feeds-opml-display (feed-url)
  "Display OPML feed."
  (interactive (list (emacspeak-eww-read-url)))
  (cl-declare (special emacspeak-opml-view-xsl))
  (emacspeak-feeds-feed-display feed-url emacspeak-opml-view-xsl 'speak))

;;;###autoload
(defun emacspeak-feeds-atom-display (feed-url)
  "Display ATOM feed."
  (interactive (list (emacspeak-eww-read-url)))
  (cl-declare (special emacspeak-atom-view-xsl))
  (emacspeak-feeds-feed-display feed-url emacspeak-atom-view-xsl 'speak))

;;}}}
;;{{{ Validate Feed:

;;}}}
;;{{{  view feed

;;; Helper:
(defun emacspeak-feeds-browse-feed (feed &optional speak)
  "Display specified feed.
Argument `feed' is a feed structure (label url type)."
  (let ((uri (cl-second feed))
        (type  (cl-third feed))
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
  "Browse   feed."
  (interactive
   (list
    (let ((completion-ignore-case t))
      (completing-read "Feed:" emacspeak-feeds nil 'must-match))))
  (emacspeak-feeds-browse-feed (assoc feed emacspeak-feeds) 'speak))

;;}}}
;;{{{ Finding Feeds:

(define-button-type 'emacspeak-feeds-feed-button
  'follow-link t
  'action 'emacspeak-feeds-feed-button-action 
  'link nil ;site url 
  'url nil; site url
  )

(defun emacspeak-feeds-feed-button-action (button)
  "Open feed associated with this button."
  (let ((browse-url-browser-function  'eww-browse-url)
        (url (button-get button 'url))
        (link (button-get button 'link)))
    (cond
     ((zerop (length url)) ; missing feed url 
      (browse-url link))
     ((string-match "atom" url)
      (emacspeak-feeds-atom-display url))
     ((string-match "blogspot" url)
      (emacspeak-feeds-atom-display url))
     ((string-match "rss" url)
      (emacspeak-feeds-rss-display url))
     (t (emacspeak-feeds-rss-display url)))))

;;}}}
;;{{{Awesome RSS

(defcustom emacspeak-feeds-awesome-rss
  (expand-file-name "~/sourceforge/awesome-rss-feeds/")
  "Location of Awesome RSS"
  :type 'directory
  :group 'emacspeak-feeds)

(defvar emacspeak-feeds-awesome-rss-map nil
  "Hash table that holds OPML Names->Files map.")

;;;###autoload
(defun emacspeak-feeds-awesome-rss ()
  "Display Awesome RSS OPML file read with completion.
See etc/fixup-awesome-rss  for first-time  for instructions."
  (interactive)
  (cl-declare (special emacspeak-feeds-awesome-rss-map
                       emacspeak-feeds-awesome-rss
                       emacspeak-opml-view-xsl))
  (unless (file-exists-p emacspeak-feeds-awesome-rss) 
    (error
     "Download awesome-rss from Github, and run the awesome-rss-fixup.sh script found  in %s"
     emacspeak-etc-directory))
  (unless emacspeak-feeds-awesome-rss-map ;;; first time
    (setq emacspeak-feeds-awesome-rss-map (make-hash-table :test #'equal))
    (cl-loop
     for f in
     (directory-files-recursively emacspeak-feeds-awesome-rss "\\.opml\\'")
     do
     (puthash
      (substring (file-name-nondirectory f) 0 -5)
      f emacspeak-feeds-awesome-rss-map)))
  (let ((feed
         (gethash 
          (completing-read "OPML: " emacspeak-feeds-awesome-rss-map nil t)
          emacspeak-feeds-awesome-rss-map)))
    (emacspeak-eww-autospeak)
    (emacspeak-xslt-view-file emacspeak-opml-view-xsl feed)))

;;}}}
(provide 'emacspeak-feeds)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
