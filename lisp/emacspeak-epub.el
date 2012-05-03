;;; emacspeak-epub.el --- epubs Front-end for emacspeak desktop
;;; $Id: emacspeak-epub.el 5798 2008-08-22 17:35:01Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak front-end for EPUBS Talking Books
;;; Keywords: Emacspeak, epubs Digital Talking Books
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2008-06-21 10:50:41 -0700 (Sat, 21 Jun 2008) $ |
;;;  $Revision: 4541 $ |
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
;;; In celebration of a million books and more to read from
;;; Google Books
;;; The EPubs format is slightly simpler than full Daisy ---
;;; (see) emacspeak-daisy.el
;;; Since it only needs one level of indirection (no audio,
;;; therefore no smil). This module is consequently simpler than
;;; emacspeak-daisy.el.
;;; This module will eventually  implement the Google Books  API
;;; --- probably by invoking the yet-to-be-written gbooks.el in emacs-g-client
;;; As we move to epub-3, this module will bring back audio layers etc., perhaps via a simplified smil implementation.
;;; Code:

;;}}}
;;{{{ Required Modules:

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'emacspeak-webutils)
(require 'emacspeak-xslt)
(require 'derived)

;;}}}
;;{{{  Customizations, Variables:

(defgroup emacspeak-epub nil
  "Epubs Digital  Books  for the Emacspeak desktop."
  :group 'emacspeak)

(defcustom emacspeak-epub-library-directory
  (expand-file-name "~/epubs/")
  "Directory under which we store Epubs."
  :type 'directory
  :group 'emacspeak-epub)
;;;###autoload
(defcustom emacspeak-epub-html-to-text-command
  "lynx -dump -stdin"
  "Command to convert html to text on stdin."

  :type '(choice
          (const :tag "lynx"  "lynx -dump -stdin")
          (const "html2text" "html2text"))
  :group 'emacspeak-epub)

(defvar emacspeak-epub-zip-extract
  (cond ((executable-find "unzip") "unzip")
        (t (message "unzip not found.")))
  "Program to extract a zip file member.")

(defvar emacspeak-epub-wget
  (executable-find "wget")
  "WGet program.")

(defvar emacspeak-epub-zip-info
  (cond ((executable-find "zipinfo") "zipinfo")
        (t (message "zipinfo not found.")))
  "Program to examine a zip file.")

;;}}}
;;{{{ EPub Implementation:

(defvar emacspeak-epub-toc-path-pattern
  ".ncx$"
  "Pattern match for path component  to table of contents in an Epub.")

(defvar emacspeak-epub-toc-command
  (format "%s -1 %%s | grep %s"
          emacspeak-epub-zip-info
          emacspeak-epub-toc-path-pattern)
  "Command that returns location of .ncx file in an epub archive.")
(defsubst emacspeak-epub-do-toc (file)
  "Return location of .ncx file within epub archive."
  (declare (special emacspeak-epub-toc-command))
  (substring
   (shell-command-to-string (format emacspeak-epub-toc-command file ))
   0 -1))

(defvar emacspeak-epub-opf-path-pattern
  ".opf$"
  "Pattern match for path component  to table of contents in an Epub.")

(defvar emacspeak-epub-opf-command
  (format "%s -1 %%s | grep %s"
          emacspeak-epub-zip-info
          emacspeak-epub-opf-path-pattern)
  "Command that returns location of .ncx file in an epub archive.")

(defsubst emacspeak-epub-do-opf (file)
  "Return location of .opf file within epub archive."
  (declare (special emacspeak-epub-opf-command))
  (substring
   (shell-command-to-string (format emacspeak-epub-opf-command file ))
   0 -1))

(defvar emacspeak-epub-ls-command
  (format "%s -1 %%s " emacspeak-epub-zip-info)
  "Shell command that returns list of files in an epub archive.")

(defsubst emacspeak-epub-do-ls (file)
  "Return list of files in an epub archive."
  (declare (special emacspeak-epub-ls-command))
  (split-string
   (shell-command-to-string (format emacspeak-epub-ls-command file ))))

(defstruct emacspeak-epub
  path ; path to .epub file
  toc ; path to .ncx file in archive
  base ; directory in archive that holds toc.ncx
  opf ; path to content.opf
  ls ; list of files in archive
  )

(defun emacspeak-epub-make-epub  (epub-file)
  "Construct an epub object given an epub filename."
  (let ((ls (emacspeak-epub-do-ls epub-file))
        (toc (emacspeak-epub-do-toc epub-file))
        (opf (emacspeak-epub-do-opf epub-file)))
    (unless (> (length toc) 0) (error "No TOC --- Not a valid EPub?"))
    (unless (> (length opf) 0) (error "No Package --- Not a valid EPub?"))
    (make-emacspeak-epub
     :path (expand-file-name epub-file)
     :toc toc
     :base (file-name-directory toc)
     :opf opf
     :ls ls)))
(defvar emacspeak-epub-scratch " *epub-scratch*"
  "Scratch buffer used to process epub.")

(defun emacspeak-epub-get-contents (epub element)
  "Return buffer containing contents of element from epub."
  (declare (special emacspeak-epub-scratch))
  (unless   (emacspeak-epub-p epub) (error "Not an EPub object."))
  (unless (member element (emacspeak-epub-ls epub)) (error "Element not found in EPub. "))
  (let ((buffer (get-buffer-create emacspeak-epub-scratch)))
    (with-current-buffer buffer
      (setq buffer-undo-list t)
      (erase-buffer)
      (call-process emacspeak-epub-zip-extract
                    nil t nil
                    "-c" "-qq"
                    (emacspeak-epub-path epub) element))
    buffer))

(defvar emacspeak-epub-metadata-xsl
  (expand-file-name "epub-metadata.xsl" emacspeak-xslt-directory)
  "XSL to extract Author/Title information.")

(defvar emacspeak-epub-opf-xsl
  (expand-file-name "epub-opf.xsl" emacspeak-xslt-directory)
  "XSL to extract Author/Title information from content.opf.")

(defsubst emacspeak-epub-get-metadata (epub)
  "Return list containing title/author metadata."
  (declare (special emacspeak-epub-zip-extract emacspeak-xslt-program
                    emacspeak-epub-opf-xsl))
  (unless   (emacspeak-epub-p epub) (error "Not an EPub object."))
  (split-string
   (shell-command-to-string
    (format "%s -c -qq '%s' '%s' |  %s --nonet --novalid %s -"
            emacspeak-epub-zip-extract
            (emacspeak-epub-path epub) (emacspeak-epub-opf epub)
            emacspeak-xslt-program emacspeak-epub-opf-xsl))
   "\n" 'omit-nulls))

(defvar emacspeak-epub-this-epub nil
  "EPub associated with current buffer.")

(make-variable-buffer-local 'emacspeak-epub-this-epub)

(defun emacspeak-epub-browse-content (epub element fragment &optional style )
  "Browse content in specified element of EPub."
  (unless   (emacspeak-epub-p epub) (error "Invalid epub"))
  (let ((base (emacspeak-epub-base epub))
        (content nil))
    (unless (string-match (format "^%s" base) element)
      (setq element (concat base element)))
    (setq content (emacspeak-epub-get-contents epub element))
    (add-hook
     'emacspeak-web-post-process-hook
     #'(lambda nil
         (declare (special emacspeak-we-url-executor emacspeak-epub-this-epub))
         (setq emacspeak-epub-this-epub epub
               emacspeak-we-url-executor 'emacspeak-epub-url-executor)
         (when fragment (w3-fetch fragment))
         (emacspeak-speak-rest-of-buffer))
     'at-end)
    (with-current-buffer content
      (emacspeak-webutils-with-xsl-environment
       style nil
       "--nonet --novalid"; options
       (browse-url-of-buffer)))))
(defvar emacspeak-epub-files-command
  (format "%s -1 %%s | grep html$ | sort" emacspeak-epub-zip-info)
  "Command to list out HTML files.")

(defun emacspeak-epub-browse-files (epub)
  "Browse list of HTML files in an EPub.
Useful if table of contents in toc.ncx is empty."
  (interactive
   (list
    (emacspeak-epub-make-epub
     (or
      (get-text-property (point) 'epub)
      (read-file-name "EPub File: ")))))
  (declare (special emacspeak-epub-scratch
                    emacspeak-epub-files-command))
  (let ((files
         (split-string
          (shell-command-to-string
           (format  emacspeak-epub-files-command (emacspeak-epub-path epub)))
          "\n" 'omit-nulls)))
    (with-current-buffer (get-buffer-create emacspeak-epub-scratch)
      (erase-buffer)
      (insert  "<ol>\n")
      (loop for f in files
            do
            (insert
             (format "<li><a href=\"%s\">%s</a></li>\n" f f)))
      (insert "</ol>\n")
      (add-hook
       'emacspeak-web-post-process-hook
       #'(lambda nil
           (declare (special emacspeak-we-url-executor emacspeak-epub-this-epub))
           (setq emacspeak-epub-this-epub epub
                 emacspeak-we-url-executor 'emacspeak-epub-url-executor)
           (emacspeak-speak-buffer))
       'at-end)
      (browse-url-of-buffer))))

(defvar epub-toc-xsl (expand-file-name "epub-toc.xsl" emacspeak-xslt-directory)
  "XSL to process .ncx file.")

(defun emacspeak-epub-browse-toc (epub)
  "Browse table of contents from an EPub."
  (declare (special epub-toc-xsl))
  (unless   (emacspeak-epub-p epub) (error "Invalid epub"))
  (let ((toc (emacspeak-epub-toc epub)))
    (emacspeak-epub-browse-content epub toc nil epub-toc-xsl)))

;;; Fragment identifiers handled only in W3

(defun emacspeak-epub-url-executor (url)
  "Custom URL executor for use in EPub Mode."
  (interactive "sURL: ")
  (declare (special emacspeak-epub-this-epub))
  (unless emacspeak-epub-this-epub (error "Not an EPub document."))
  (cond
   ((not (string-match "^http://" url)) ; relative url
    (when (string-match "^cid:" url) (setq url (substring url 4)))
    (when (string-match "^file:" url) (setq url  (second (split-string url  "/tmp/"))))
    (let* ((fields (split-string url "#"))
           (locator (first fields))
           (fragment (second fields)))
      (when fragment (setq fragment (format "#%s" fragment)))
      (emacspeak-epub-browse-content emacspeak-epub-this-epub locator fragment)))
   (t (browse-url url))))

;;}}}
;;{{{ Bookshelf Implementation:

(defvar emacspeak-epub-db-file
  (expand-file-name ".bookshelf" emacspeak-epub-library-directory)
  "Cache of bookshelf metadata.")

(defvar emacspeak-epub-db (make-hash-table :test  #'equal)
  "In memory cache of epub bookshelf.")

(defstruct emacspeak-epub-metadata
  title
  author)

(defun emacspeak-epub-bookshelf-update ()
  "Update bookshelf metadata."
  (declare (special emacspeak-epub-db-file emacspeak-epub-db))
  (let ((updated nil))
    (loop for f in
          (directory-files  emacspeak-epub-library-directory 'full "epub")
          do
          (unless (gethash f emacspeak-epub-db)
            (setq updated t)
            (let* ((fields (emacspeak-epub-get-metadata
                            (emacspeak-epub-make-epub f)))
                   (title (first fields))
                   (author  (second fields)))
              (when (zerop (length title)) (setq title "Untitled"))
              (when (zerop (length author)) (setq author "Unknown"))
              (setf (gethash f emacspeak-epub-db)
                    (make-emacspeak-epub-metadata
                     :title title
                     :author author)))))
    (loop for f being the hash-keys of emacspeak-epub-db
          do
          (unless (file-exists-p f) (remhash f emacspeak-epub-db)))
    (when updated (emacspeak-epub-bookshelf-save))))

;;;###autoload
(defun emacspeak-epub-bookshelf-save ()
  "Save bookshelf metadata."
  (interactive)
  (declare (special emacspeak-epub-db-file))
  (let ((buff (find-file-noselect emacspeak-epub-db-file)))
    (save-excursion
      (set-buffer buff)
      (setq buffer-undo-list t)
      (erase-buffer)
      (print  emacspeak-epub-db  buff)
      (save-buffer buff)
      (kill-buffer buff)
      (when (interactive-p)
        (message "Saved EPub bookshelf.")
        (emacspeak-auditory-icon 'save-object)))))

(defun emacspeak-epub-bookshelf-load ()
  "Load bookshelf metadata from disk."
  (interactive)
  (declare (special emacspeak-epub-db
                    emacspeak-epub-db-file))
  (when (file-exists-p emacspeak-epub-db-file)
    (let ((buffer (find-file-noselect emacspeak-epub-db-file)))
      (with-current-buffer buffer
        (goto-char (point-min))
        (setq emacspeak-epub-db (read buffer)))
      (kill-buffer buffer))))

;;}}}
;;{{{ Interactive Commands:

(defvar emacspeak-epub-interaction-buffer "*EPub*"
  "Buffer for EPub interaction.")

;;;###autoload
(defun emacspeak-epub ()
  "EPub  Interaction.
For detailed documentation, see \\[emacspeak-epub-mode]"
  (interactive)
  (declare (special emacspeak-epub-interaction-buffer
                    emacspeak-epub-zip-info
                    emacspeak-epub-zip-extract))
  (unless emacspeak-epub-zip-extract
    (error "Please install unzip."))
  (unless emacspeak-epub-zip-info
    (error "Please install zipinfo. "))
  (let ((buffer (get-buffer emacspeak-epub-interaction-buffer)))
    (unless (buffer-live-p buffer)
      (with-current-buffer (get-buffer-create emacspeak-epub-interaction-buffer) (emacspeak-epub-mode)))
    (pop-to-buffer emacspeak-epub-interaction-buffer)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

;;;###autoload
(defun emacspeak-epub-open (epub-file)
  "Open specified Epub."
  (interactive
   (list
    (or
     (get-text-property (point) 'epub)
     (read-file-name "EPub: " emacspeak-epub-library-directory))))
  (let ((e (emacspeak-epub-make-epub epub-file)))
    (emacspeak-epub-browse-toc e)))

(defun emacspeak-epub-fulltext (epub-file)
  "Display fulltext from EPub in a buffer.
Suitable for text searches."
  (interactive
   (list
    (or
     (get-text-property (point) 'epub)
     (read-file-name "EPub: " emacspeak-epub-library-directory))))
  (declare (special emacspeak-epub-files-command))
  (let ((buffer (get-buffer-create "FullText EPub"))
        (files
         (split-string
          (shell-command-to-string
           (format  emacspeak-epub-files-command epub-file))
          "\n" 'omit-nulls))
        (inhibit-read-only t)
        (command nil))
    (with-current-buffer buffer
      (erase-buffer)
      (setq buffer-undo-list t)
      (loop for f in files
            do
            (setq command
                  (format "unzip -c -qq '%s' '%s' | %s"
                          epub-file f
                          emacspeak-epub-html-to-text-command))
            (insert (shell-command-to-string command ))
            (goto-char (point-max)))
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (switch-to-buffer buffer)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'open-object)))

(defvar emacspeak-epub-google-search-template
  "http://books.google.com/books/feeds/volumes?min-viewability=full&epub=epub&q=%s"
  "REST  end-point for performing Google Books Search to find Epubs  having full viewability.")

;;;###autoload
(defun emacspeak-epub-google (query)
  "Search for Epubs from Gooble Books."
  (interactive "sGoogle Books Query: ")
  (declare (special emacspeak-epub-google-search-template))
  (emacspeak-webutils-atom-display
   (format emacspeak-epub-google-search-template
           (emacspeak-url-encode query))))

(defun emacspeak-epub-next ()
  "Move to next book."
  (interactive)
  (end-of-line)
  (goto-char (next-single-property-change (point) 'epub))
  (beginning-of-line)
  (emacspeak-speak-line)
  (emacspeak-auditory-icon 'select-obect))

(defun emacspeak-epub-previous ()
  "Move to previous book."
  (interactive)
  (beginning-of-line)
  (goto-char (previous-single-property-change (point) 'epub))
  (beginning-of-line)
  (emacspeak-speak-line)
  (emacspeak-auditory-icon 'select-obect))

(defun emacspeak-epub-delete ()
  "Delete EPub under point."
  (interactive)
  (let ((file (get-text-property (point) 'epub)))
    (cond
     ((null file) (error "No EPub under point."))
     (t (when (y-or-n-p
               (format "Delete %s" file))
          (delete-file file)
          (emacspeak-epub-bookshelf-refresh)
          (emacspeak-auditory-icon 'delete-object))))))

;;}}}
;;{{{ Epub Mode:
(defsubst emacspeak-epub-format-author (name)
  "Format author name, abbreviating if needed."
  (let ((len (length name))
        (fields nil))
    (cond
     ((< len 16))
     (t (setq  fields (split-string name))
        (let ((count (length fields))
              (result nil))
          (cond
           ((= 1 count))
           (t
            (setq result
                  (loop for i from 0 to(- count 2)
                        collect
                        (upcase (aref  (nth i fields) 0))))
            (setq result
                  (mapconcat
                   #'(lambda (c) (format "%c" c))
                   result ". "))
            (setq name (format "%s. %s"
                               result
                               (nth (1- count) fields))))))))
    (propertize name 'face 'font-lock-type-face)))

(defsubst emacspeak-epub-bookshelf-redraw ()
  "Redraw Bookshelf."
  (declare (special  emacspeak-epub-db))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (loop for f being the hash-keys  of  emacspeak-epub-db
          do
          (let ((start (point)))
            (insert
             (format "%-20s\t%s"
                     (emacspeak-epub-format-author (emacspeak-epub-metadata-author (gethash f emacspeak-epub-db)) )
                     (propertize
                      (emacspeak-epub-metadata-title (gethash f emacspeak-epub-db))
                      'face 'font-lock-string-face)))
            (put-text-property start (point) 'epub f)
            (insert "\n")))
    (sort-lines nil (point-min) (point-max))
    (goto-char (point-min))))

;;;###autoload
(defun emacspeak-epub-bookshelf-refresh ()
  "Refresh and redraw bookshelf."
  (interactive)
  (unless (eq major-mode 'emacspeak-epub-mode)
    (error "Not in the EPub Bookshelf."))
  (emacspeak-epub-bookshelf-load)
  (emacspeak-epub-bookshelf-update)
  (emacspeak-epub-bookshelf-redraw)
  ( emacspeak-epub-bookshelf-save)
  (emacspeak-auditory-icon 'task-done))

(define-derived-mode emacspeak-epub-mode special-mode
  "EPub Interaction On The Emacspeak Audio Desktop"
  "An EPub Front-end.
Letters do not insert themselves; instead, they are commands.
\\<emacspeak-epub-mode-map>
\\{emacspeak-epub-mode-map}"
  (setq buffer-undo-list t)
  (setq header-line-format
        (propertize "EPub Bookshelf" 'face 'bold))
  (goto-char (point-min))
  (cd-absolute emacspeak-epub-library-directory)
  (emacspeak-epub-bookshelf-refresh))

(declaim (special emacspeak-epub-mode-map))
(loop for k in
      '(
        ("\M-s" emacspeak-epub-bookshelf-save)
        ("\C-x\C-s" emacspeak-epub-bookshelf-save)
        ("\C-x\C-q" emacspeak-epub-bookshelf-refresh)
        ("d" emacspeak-epub-delete)
        ("f" emacspeak-epub-browse-files)
        ("o" emacspeak-epub-open)
        ("t" emacspeak-epub-fulltext)
        ("n" next-line)
        ("p" previous-line)
        ([return] emacspeak-epub-open)
        ("\C-m" emacspeak-epub-open)
        ("G" emacspeak-epub-gutenberg-download)
        ("C" emacspeak-epub-gutenberg-catalog)
        ("g" emacspeak-epub-google)
        )
      do
      (emacspeak-keymap-update emacspeak-epub-mode-map k))

;;}}}
;;{{{ Gutenberg Hookup:
;;; Offline Catalog:
;;; http://www.gutenberg.org/wiki/Gutenberg:Offline_Catalogs
;;; Goal:
;;; Snapshot catalog, enable local searches, and pull desired book to local cache
;;; using appropriate recipe.
;;; http://www.gutenberg.org/ebooks/<bookid>.epub.?noimages?
(defcustom emacspeak-epub-gutenberg-mirror
  "http://www.gutenberg.org/ebooks/"
  "Base URL  for Gutenberg mirror."
  :type 'string
  :group 'emacspeak-epub)

(defcustom emacspeak-epub-gutenberg-suffix ".epub.noimages"
  "Suffix of book type we retrieve."
  :type 'string
  :group 'emacspeak-epub)

(defsubst emacspeak-epub-gutenberg-download-uri (book-id)
  "Return URL  for downloading Gutenberg EBook."
  (declare (special emacspeak-epub-gutenberg-suffix
                    emacspeak-epub-gutenberg-mirror))
  (format "%s%s%s"
          emacspeak-epub-gutenberg-mirror
          book-id
          emacspeak-epub-gutenberg-suffix))

;;;###autoload
(defun emacspeak-epub-gutenberg-download (book-id)
  "Download specified EBook to local cache"
  (interactive "sBook-Id: ")
  (let ((file
         (expand-file-name
          (format "%s%s" book-id emacspeak-epub-gutenberg-suffix)
          emacspeak-epub-library-directory))
        (url (emacspeak-epub-gutenberg-download-uri book-id)))
    (unless (file-exists-p file)
      (call-process
       emacspeak-epub-wget
       nil nil nil
       "-O"
       file url))
    (emacspeak-epub-bookshelf-refresh)
    (when (y-or-n-p
           (format "Open %s?"file))
      (emacspeak-epub-open file))))

(defvar emacspeak-epub-gutenberg-catalog-url
  "http://www.gutenberg.org/dirs/GUTINDEX.ALL"
  "URL to Gutenberg index.")

(defvar emacspeak-epub-gutenberg-catalog-file
  (expand-file-name "catalog/GUTINDEX.ALL" emacspeak-epub-library-directory)
  "Local filename of catalog.")

(defun emacspeak-epub-gutenberg-catalog (&optional refresh)
  "Open Gutenberg catalog.
Fetch if needed, or if refresh is T."
  (interactive "P")
  (declare (special emacspeak-epub-gutenberg-catalog-url
                    emacspeak-epub-gutenberg-catalog-file
                    emacspeak-epub-wget))
  (unless emacspeak-epub-wget
    (error "Please install wget. "))
  (unless (file-exists-p (file-name-directory emacspeak-epub-gutenberg-catalog-file))
    (make-directory (file-name-directory emacspeak-epub-gutenberg-catalog-file) 'parents))
  (when (or refresh
            (not (file-exists-p emacspeak-epub-gutenberg-catalog-file)))
    (call-process
     emacspeak-epub-wget
     nil nil nil
     "-O"
     emacspeak-epub-gutenberg-catalog-file
     emacspeak-epub-gutenberg-catalog-url))
  (view-file-other-window emacspeak-epub-gutenberg-catalog-file)
  (emacspeak-auditory-icon 'task-done))

;;}}}
(provide 'emacspeak-epub)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
