;;; emacspeak-epub.el --- epubs On emacspeak desktop  -*- lexical-binding: t; -*-
;;; $Id: emacspeak-epub.el 5798 2008-08-22 17:35:01Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak front-end for EPUBS Talking Books
;;; Keywords: Emacspeak, epubs Digital Talking Books
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2008-06-21 10:60:41 -0700 (Sat, 21 Jun 2008) $ |
;;;  $Revision: 4541 $ |
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

;;; @subsection Introduction
;;;
;;; This module implements the Emacspeak EPub
;;; Bookshelf --- a unified interface for organizing, locating and
;;; reading EPub EBooks on the emacspeak Audio Desktop. The epub
;;; reader is built using the Emacs Web Browser (EWW), and all of
;;; emacspeak's EWW conveniences are available when reading EBooks ---
;;; see @xref{emacspeak-eww} for useful tools including bookmarking
;;; and structured navigation. For now it supports epub2 --- it will
;;; support epub3 some time in the future.

;;; The main entry point is command @command{emacspeak-epub} bound to
;;; @kbd{C-e g}. This command opens a new bookshelf buffer unless the
;;; user has previously opened a specific bookshelf. A
;;; @emph{bookshelf} is a buffer that lists books placed on a given
;;; bookshelf --- these are listed by @emph{title} and
;;; @emph{author}. The bookshelf buffer is in a special mode that
;;; provides single-key commands for adding, removing and finding
;;; books, as well as for opening the selected book using Emacs'
;;; built-in Web browser (@command{eww}).
;;;
;;; The next few sections give a high-level overview of the emacspeak
;;; Bookshelf and EPub interaction, followed by detailed documentation
;;; on the various commands and user options.

;;; @subsection Organizing EBooks On The Emacspeak Desktop
;;;
;;; In the simplest case, EBooks can be placed under a specific
;;; directory (with sub-directories as needed).
;;; Customize   user option @code{emacspeak-epub-library-directory}
;;; to point to this location.
;;; Here is  a quick summary of commands for
;;; organizing, saving and opening  a bookshelf:
;;;
;;; @table @kbd
;;; @item a
;;; emacspeak-epub-bookshelf-add-epub
;;; @item b
;;; emacspeak-epub-bookshelf-open
;;; @item c
;;; emacspeak-epub-bookshelf-clear
;;; @item d
;;; emacspeak-epub-bookshelf-remove-this-book
;;; @item r
;;; emacspeak-epub-bookshelf-rename
;;; @item l
;;; emacspeak-epub-locate-epubs
;;; @item C-a
;;; emacspeak-epub-bookshelf-add-directory
;;; @item C-d
;;; emacspeak-epub-bookshelf-remove-directory
;;; @item C-l
;;; emacspeak-epub-bookshelf-redraw
;;; @item C-o
;;; emacspeak-epub-bookshelf-open-epub
;;; @item M-s
;;; emacspeak-epub-bookshelf-save
;;; @item C-x C-q
;;; emacspeak-epub-bookshelf-refresh
;;; @item C-x C-s
;;; emacspeak-epub-bookshelf-save
;;; @end table
;;;
;;; @subsection Integrating With Project Gutenberg
;;;
;;; Gutenberg integration provides one-shot commands for downloading
;;; the latest copy of the Gutenberg catalog and  finding and downloading
;;; the desired epub for offline reading.
;;;
;;; @table @kbd
;;; @item C
;;; emacspeak-epub-gutenberg-catalog
;;; @item g
;;; emacspeak-epub-gutenberg-download
;;; @end table
;;; Once downloaded, these EBooks can be
;;; organized under  @code{emacspeak-epub-library-directory}
;;; For  more advanced usage, see the next section
;;; on integrating with Calibre catalogs.
;;;
;;; @subsection Calibre Integration
;;;
;;; Project Calibre  enables the indexing and searching of
;;; large EBook collections.
;;; Read the Calibre documentation for organizing and indexing your EBook library.
;;; See user options named @code{emacspeak-epub-calibre-*} for
;;; customizing  emacspeak to work with Calibre.
;;; Once set up, Calibre integration provides
;;; the following commands from the @emph{bookshelf} buffer:
;;;
;;; @table @kbd
;;; @item /
;;; emacspeak-epub-calibre-results
;;; @item A
;;; emacspeak-epub-bookshelf-calibre-author
;;; @item S
;;; emacspeak-epub-bookshelf-calibre-search
;;; @item T
;;; emacspeak-epub-bookshelf-calibre-title
;;; @end table
;;;
;;; @subsection Reading EBooks From The Bookshelf
;;;
;;; The most efficient means to read an EBook is to have EWW render
;;; the entire book --- this works well even for very large EBooks
;;; given that EWW is efficient at rendering HTML. Rendering the
;;; entire book means that all of the contents are available for
;;; searching. To view an EBook in its entirety, use command
;;; @code{emacspeak-epub-eww}. You can open the EPub table of contents
;;; with command @code{emacspeak-epub-open}; for a
;;; well-constructed epub, this TOC should provide hyperlinks to each
;;; section listed in the table of contents.
;;;
;;; @table @kbd
;;; @item RET
;;; emacspeak-epub-eww
;;; @item e
;;; emacspeak-epub-eww
;;; @item f
;;; emacspeak-epub-browse-files
;;; @item o
;;; emacspeak-epub-open
;;; @end table
;;}}}
;;{{{ Required Modules:

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'emacspeak-xslt)

(eval-when-compile
  (require 'derived)
  (require 'subr-x))
(require 'dom)
;;}}}
;;{{{  Customizations, Variables:

(defgroup emacspeak-epub nil
  "Epubs Digital  Books  for the Emacspeak desktop."
  :group 'emacspeak)

(defcustom emacspeak-epub-library-directory
  (expand-file-name "~/EBooks/")
  "Directory under which we store Epubs."
  :type 'directory
  :group 'emacspeak-epub)

(defvar emacspeak-epub-zip-extract
  (cond ((executable-find "unzip") (executable-find "unzip") )
        (t (message "unzip not found.")
           nil))
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
;;; Helper: dom from file in archive
(defsubst emacspeak-epub-dom-from-archive (epub-file file &optional xml-p)
  "Return DOM from specified file in epub archive."
  (cl-declare (special emacspeak-epub-zip-extract))
  (with-temp-buffer
    (setq buffer-undo-list t)
    (shell-command
     (format
      "%s -c -qq %s %s "
      emacspeak-epub-zip-extract
      epub-file
      (shell-quote-argument file))
     (current-buffer))
    (cond
     (xml-p (libxml-parse-xml-region (point-min) (point-max)))
     (t (libxml-parse-html-region (point-min) (point-max))))))

(defvar emacspeak-epub-toc-path-pattern
  ".ncx$"
  "Pattern match for path component  to table of contents in an Epub.")

(defvar emacspeak-epub-toc-command
  (format "%s -1 %%s | grep %s"
          emacspeak-epub-zip-info
          emacspeak-epub-toc-path-pattern)
  "Command that returns location of .ncx file in an epub archive.")

(defun emacspeak-epub-do-toc (file)
  "Return location of .ncx file within epub archive."
  (cl-declare (special emacspeak-epub-toc-command))
  (let ((result
         (shell-command-to-string (format emacspeak-epub-toc-command  file))))
    (cond
     ((= 0 (length result)) nil)
     (t (substring result 0 -1)))))

(defun emacspeak-epub-get-contents (epub element)
  "Return buffer containing contents of element from epub."
  (cl-declare (special emacspeak-epub-scratch))
  (unless   (emacspeak-epub-p epub) (error "Not an EPub object."))
  (unless (member element (emacspeak-epub-ls epub))
    (error "Element not found in EPub. "))
  (let ((buffer (get-buffer-create emacspeak-epub-scratch)))
    (with-current-buffer buffer
      (setq buffer-undo-list t)
      (erase-buffer)
      (call-process emacspeak-epub-zip-extract
                    nil t nil
                    "-c" "-qq"
                    (emacspeak-epub-shell-unquote (emacspeak-epub-path epub))
                    element))
    buffer))

(defun emacspeak-epub-nav-files (this-epub)
  "Return ordered list of content files from navMap."
  (let* ((navs nil)
         (hash (make-hash-table :test 'equal))
         (value nil)
         (toc (emacspeak-epub-toc this-epub))
         (base (emacspeak-epub-base this-epub))
         (ncx nil))
    (with-current-buffer (emacspeak-epub-get-contents this-epub toc)
      (setq ncx (libxml-parse-xml-region (point-min) (point-max)))
      (kill-buffer))
    (cl-loop
     for n in (dom-by-tag ncx 'content)
     do
     (setq value
           (concat base (cl-first (split-string (dom-attr n 'src) "#"))))
     (unless (gethash value hash)
       (puthash value 1 hash)
       (push value navs)))
    (nreverse navs)))

(defvar emacspeak-epub-opf-path-pattern
  ".opf$"
  "Pattern match for path component  to table of contents in an Epub.")

(defvar emacspeak-epub-opf-command
  (format "%s -1 %%s | grep %s"
          emacspeak-epub-zip-info
          emacspeak-epub-opf-path-pattern)
  "Command that returns location of .opf file in an epub archive.")

(defun emacspeak-epub-do-opf (file)
  "Return location of .opf file within epub archive."
  (cl-declare (special emacspeak-epub-opf-command))
  (substring
   (shell-command-to-string (format emacspeak-epub-opf-command file))
   0 -1))

(defvar emacspeak-epub-ls-command
  (format "%s -1 %%s | sort" emacspeak-epub-zip-info)
  "Shell command that returns sorted list of files in an epub archive.")

(defun emacspeak-epub-do-ls (file)
  "Return sorted list of files in an epub archive."
  (cl-declare (special emacspeak-epub-ls-command))
  (split-string
   (shell-command-to-string (format emacspeak-epub-ls-command file))))

(cl-defstruct emacspeak-epub
  path                       ; path to .epub file
  toc                        ; path to .ncx file in archive
  base                       ; directory in archive that holds toc.ncx
  opf                        ; path to content.opf
  opf-dom ; parsed content of content.opf
  ls                                 ; list of files in archive
  html                               ; html files in archive
  navs                               ; content files found from navMap
  title  author
  )

(defun emacspeak-epub-make-epub  (epub-file)
  "Construct an epub object given an epub filename."
  (let ((path (expand-file-name epub-file))
        (this nil)
        (ls (emacspeak-epub-do-ls epub-file))
        (toc (emacspeak-epub-do-toc epub-file))
        (opf (emacspeak-epub-do-opf epub-file))
        (opf-dom nil)
        (title nil)
        (author nil))
    (unless (> (length opf) 0) (error "No Package --- Not a valid EPub?"))
    (unless (> (length toc) 0) (error "No TOC --- Not a valid EPub?"))
    (setq opf-dom (emacspeak-epub-dom-from-archive path opf 'xml))
    (setq title (dom-text (dom-by-tag  opf-dom 'title))
          author (dom-text (dom-by-tag  opf-dom 'creator)))
    (when (zerop (length author)) (setq author "Unknown"))
    (when (zerop (length title)) (setq title "Untitled"))
    (setq this 
          (make-emacspeak-epub
           :path path
           :title title
           :author author
           :toc toc
           :base (file-name-directory toc)
           :opf opf
           :opf-dom opf-dom
           :ls ls
           :html
           (cl-remove-if-not #'(lambda (s) (string-match "\\.x?html$" s)) ls)))
    (setf (emacspeak-epub-navs this)  (emacspeak-epub-nav-files this))
    this))

(defvar emacspeak-epub-scratch " *epub-scratch*"
  "Scratch buffer used to process epub.")

(defun emacspeak-epub-shell-unquote (f)
  "Reverse effect of shell-quote-argument."
  (shell-command-to-string (format "echo -n %s" f)))

(defvar-local emacspeak-epub-this-epub nil
  "EPub associated with current buffer.")

(defun emacspeak-epub-browse-content (epub element _ffragment &optional style)
  "Browse content in specified element of EPub."
  (cl-declare (special emacspeak-we-xsl-p))
  (unless   (emacspeak-epub-p epub) (error "Invalid epub"))
  (let ((base (emacspeak-epub-base epub))
        (content nil)
        (emacspeak-xslt-options "--nonet --novalid")
        (emacspeak-we-xsl-p nil))
    (unless (string-match (format "^%s" base) element)
      (setq element (concat base element)))
    (setq content (emacspeak-epub-get-contents epub element))
    (add-hook
     'emacspeak-eww-post-process-hook
     #'(lambda nil
         (cl-declare (special emacspeak-we-url-executor
                              emacspeak-epub-this-epub
                              emacspeak-speak-directory-settings))
         (emacspeak-speak-load-directory-settings)
         (setq emacspeak-epub-this-epub epub
               emacspeak-we-url-executor 'emacspeak-epub-url-executor)
         (emacspeak-speak-rest-of-buffer))
     'at-end)
    (with-current-buffer content
      (when style
        (emacspeak-xslt-region style   (point-min) (point-max)))
      (browse-url-of-buffer))))

(defun emacspeak-epub-browse-files (epub)
  "Browse list of HTML files in  EPub.
Useful if table of contents in toc.ncx is empty."
  (interactive
   (list
    (emacspeak-epub-make-epub
     (or
      (get-text-property (point) 'epub)
      (read-file-name "EPub File: ")))))
  (cl-declare (special emacspeak-epub-scratch))
  (let ((files (emacspeak-epub-html epub)))
    (with-current-buffer (get-buffer-create emacspeak-epub-scratch)
      (erase-buffer)
      (insert  "<ol>\n")
      (cl-loop for f in files
               do
               (insert
                (format "<li><a href=\"%s\">%s</a></li>\n" f f)))
      (insert "</ol>\n")
      (add-hook
       'emacspeak-eww-post-process-hook
       #'(lambda nil
           (cl-declare (special emacspeak-we-url-executor
                                emacspeak-epub-this-epub))
           (setq emacspeak-epub-this-epub epub
                 emacspeak-we-url-executor 'emacspeak-epub-url-executor)
           (emacspeak-speak-buffer))
       'at-end)
      (browse-url-of-buffer))))

(defvar epub-toc-xsl (emacspeak-xslt-get "epub-toc.xsl")
  "XSL to process .ncx file.")

(defun emacspeak-epub-browse-toc (epub)
  "Browse table of contents from an EPub."
  (cl-declare (special epub-toc-xsl))
  (unless   (emacspeak-epub-p epub) (error "Invalid epub"))
  (let ((toc (emacspeak-epub-toc epub)))
    (emacspeak-epub-browse-content epub toc nil epub-toc-xsl)))

(defun emacspeak-epub-url-executor (url)
  "Custom URL executor for use in EPub Mode."
  (interactive "sURL: ")
  (cl-declare (special emacspeak-epub-this-epub
                       emacspeak-speak-directory-settings))
  (unless emacspeak-epub-this-epub (error "Not an EPub document."))
  (cond
   ((not (string-match "^http://" url)) ; relative url
    (when (string-match "^cid:" url) (setq url (substring url 4)))
    (when (string-match "^file:" url)
      (setq url  (cl-second (split-string url  "/tmp/"))))
    (let* ((fields (split-string url "#"))
           (locator (cl-first fields))
           (fragment (cl-second fields)))
      (when fragment (setq fragment (format "#%s" fragment)))
      (add-hook 'emacspeak-eww-post-process-hook
                #'(lambda nil (ems--fastload emacspeak-speak-directory-settings)))
      (emacspeak-epub-browse-content emacspeak-epub-this-epub locator fragment)))
   (t (browse-url url))))

;;}}}
;;{{{ Epub Mode:

(defun emacspeak-epub-format-author (name)
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
                  (cl-loop for i from 0 to(- count 2)
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

(defun emacspeak-epub-insert-title-author (key epub)
  "Insert a formatted line of the bookshelf of the form Title --- Author."
  (let ((start (point)))
    (insert
     (format
      "%-60s%s\n"
      (propertize (emacspeak-epub-metadata-title epub) 'face 'italic)
      (emacspeak-epub-format-author (emacspeak-epub-metadata-author epub))))
    (put-text-property start (point) 'epub key)))

(defun emacspeak-epub-insert-author-title (key epub)
  "Insert a formatted line of the bookshelf of the form Author --- Title ."
  (let ((start (point)))
    (insert
     (format
      "%-20s%s\n"
      (emacspeak-epub-format-author (emacspeak-epub-metadata-author epub))
      (propertize (emacspeak-epub-metadata-title epub) 'face 'italic)))
    (put-text-property start (point) 'epub key)))

(defun emacspeak-epub-bookshelf-redraw (&optional author-first)
  "Redraw Bookshelf.
Optional interactive prefix arg author-first prints author at the
  left."
  (interactive "P")
  (cl-declare (special  emacspeak-epub-db))
  (let ((inhibit-read-only t)
        (formatter (if author-first
                       'emacspeak-epub-insert-author-title
                     'emacspeak-epub-insert-title-author)))
    (erase-buffer)
    (maphash formatter emacspeak-epub-db)
    (sort-lines nil (point-min) (point-max))
    (goto-char (point-min)))
  (when (ems-interactive-p) (emacspeak-auditory-icon 'task-done)))


(defun emacspeak-epub-bookshelf-refresh ()
  "Refresh and redraw bookshelf."
  (interactive)
  (unless (eq major-mode 'emacspeak-epub-mode)
    (error "Not in the EPub Bookshelf."))
  (emacspeak-epub-bookshelf-load)
  (emacspeak-epub-bookshelf-update)
  (emacspeak-epub-bookshelf-redraw)
  (emacspeak-epub-bookshelf-save)
  (emacspeak-auditory-icon 'task-done))

(define-derived-mode emacspeak-epub-mode special-mode
  "EPub Bookshelf"
  "An EPub Front-end.
Letters do not insert themselves; instead, they are commands.
\\{emacspeak-epub-mode-map}"
  (setq buffer-undo-list t)
  (setq header-line-format
        (propertize "EPub Bookshelf" 'face 'bold))
  (goto-char (point-min))
  (cd-absolute emacspeak-epub-library-directory)
  (emacspeak-epub-bookshelf-refresh))

(cl-declaim (special emacspeak-epub-mode-map))
(cl-loop
 for k in
 '(
   ("/" emacspeak-epub-calibre-results)
   ("O" emacspeak-epub-open-with-nov)
   ("A" emacspeak-epub-bookshelf-calibre-author)
   ("S" emacspeak-epub-bookshelf-calibre-search)
   ("T" emacspeak-epub-bookshelf-calibre-title)
   ("C" emacspeak-epub-gutenberg-catalog)
   ("G" emacspeak-epub-google)
   ("\C-a" emacspeak-epub-bookshelf-add-directory)
   ("\C-d" emacspeak-epub-bookshelf-remove-directory)
   ("\C-k" emacspeak-epub-delete)
   ("C-l" emacspeak-epub-bookshelf-redraw)
   ("\C-m" emacspeak-epub-eww)
   ("\C-o" emacspeak-epub-bookshelf-open-epub)
   ("\C-x\C-q" emacspeak-epub-bookshelf-refresh)
   ("\C-x\C-s" emacspeak-epub-bookshelf-save)
   ("M-s" emacspeak-epub-bookshelf-save)
   ("a" emacspeak-epub-bookshelf-add-epub)
   ("b" emacspeak-epub-bookshelf-open)
   ("c" emacspeak-epub-bookshelf-clear)
   ("d" emacspeak-epub-bookshelf-remove-this-book)
   ("e" emacspeak-epub-eww)
   ("f" emacspeak-epub-browse-files)
   ("g" emacspeak-epub-gutenberg-download)
   ("l" emacspeak-epub-locate-epubs)
   ("n" next-line)
   ("o" emacspeak-epub-open)
   ("p" previous-line)
   ("r" emacspeak-epub-bookshelf-rename)
   ("RET" emacspeak-epub-eww)
   )
 do
 (emacspeak-keymap-update emacspeak-epub-mode-map k))

;;}}}
;;{{{ Bookshelf Implementation:
(defcustom emacspeak-epub-bookshelf-directory
  (file-name-as-directory
   (expand-file-name "bsf" emacspeak-epub-library-directory))
  "Directory where we keep .bsf files defining various bookshelves."
  :type 'directory
  :group 'emacspeak-epub)

(defvar emacspeak-epub-db-file
  (expand-file-name ".bookshelf" emacspeak-epub-library-directory)
  "Cache of bookshelf metadata.")

(defvar emacspeak-epub-db (make-hash-table :test  #'equal)
  "In memory cache of epub bookshelf.")

(cl-defstruct emacspeak-epub-metadata
  title
  author)

(defun emacspeak-epub-bookshelf-update ()
  "Update bookshelf metadata."
  (cl-declare (special emacspeak-epub-db-file emacspeak-epub-db
                       emacspeak-epub-library-directory))
  (let ((updated nil)
        (filename nil))
    (cl-loop
     for f in
     (directory-files emacspeak-epub-library-directory  'full "\\.epub\\'")
     do
     (setq filename (shell-quote-argument f))
     (unless
         (gethash filename emacspeak-epub-db)
       (setq updated t)
       (let* ((epub (emacspeak-epub-make-epub filename))
              (title (emacspeak-epub-title epub))
              (author  (emacspeak-epub-author epub)))
         (setf (gethash filename emacspeak-epub-db)
               (make-emacspeak-epub-metadata :title title :author author)))))
    (cl-loop for f being the hash-keys of emacspeak-epub-db
             do
             (setq filename (emacspeak-epub-shell-unquote f))
             (unless (file-exists-p filename) (remhash f emacspeak-epub-db)))
    (when updated (emacspeak-epub-bookshelf-save))))

(defvar emacspeak-epub-find-program
  (executable-find "find")
  "Name of find utility.")

(defun emacspeak-epub-find-epubs-in-directory (directory)
  "Return a list of all epub files under directory dir."
  (cl-declare (special emacspeak-epub-find-program))
  (with-temp-buffer
    (call-process emacspeak-epub-find-program
                  nil t nil
                  (expand-file-name directory)
                  "-type" "f"
                  "-name" "*.epub")
    (delete ""
            (split-string (buffer-substring (point-min)
                                            (point-max))
                          "\n"))))
(defun emacspeak-epub-bookshelf-rename (name &optional overwrite)
  "Saves current bookshelf to  specified name.
Interactive prefix arg `overwrite' will overwrite existing file."
  (interactive "sBookshelf Name: \nP")
  (cl-declare (special emacspeak-epub-bookshelf-directory))
  (setq name (format "%s.bsf" name))
  (let ((bookshelf
         (expand-file-name ".bookshelf" emacspeak-epub-library-directory))
        (bsf (expand-file-name name emacspeak-epub-bookshelf-directory)))
    (when (and overwrite (file-exists-p bsf)) (delete-file bsf))
    (copy-file bookshelf bsf)
    (message "Copied current bookshelf to %s" name)))

(defun emacspeak-epub-bookshelf-add-directory (directory &optional recursive)
  "Add EPubs found in specified directory to the bookshelf.
Interactive prefix arg searches recursively in directory."
  (interactive "DAdd books from Directory: \nP")
  (cl-declare (special emacspeak-epub-db-file emacspeak-epub-db))
  (let ((updated 0)
        (filename nil))
    (cl-loop
     for f in
     (if recursive
         (emacspeak-epub-find-epubs-in-directory directory)
       (directory-files directory  'full "epub"))
     do
     (setq filename (shell-quote-argument f))
     (unless
         (gethash filename emacspeak-epub-db)
       (cl-incf updated)
       (let* ((epub (emacspeak-epub-make-epub filename))
              (title (emacspeak-epub-title epub))
              (author  (emacspeak-epub-author epub)))
         (setf (gethash filename emacspeak-epub-db)
               (make-emacspeak-epub-metadata :title title :author author)))))
    (unless (zerop updated)
      (emacspeak-epub-bookshelf-save)
      (emacspeak-epub-bookshelf-redraw)
      (message "Added %d books. " updated))))

(defun emacspeak-epub-bookshelf-add-epub (epub-file)
  "Add epub file to current bookshelf."
  (interactive "fAdd Book: ")
  (cl-declare (special  emacspeak-epub-db))
  (let* ((filename (shell-quote-argument (expand-file-name epub-file)))
         
         (epub (emacspeak-epub-make-epub filename))
         (title (emacspeak-epub-title epub))
         (author  (emacspeak-epub-author epub)))
    (setf (gethash filename emacspeak-epub-db)
          (make-emacspeak-epub-metadata :title title :author author))
    (emacspeak-epub-bookshelf-save)
    (emacspeak-epub-bookshelf-redraw)
    (goto-char (point-min))
    (search-forward title)
    (emacspeak-speak-line)))

(defun emacspeak-epub-bookshelf-open-epub (epub-file)
  "Open epub file and add it to current bookshelf."
  (interactive "fAdd Book: ")
  (cl-declare (special  emacspeak-epub-db))
  (let* ((filename (shell-quote-argument epub-file))
         (epub (emacspeak-epub-make-epub filename))
         (title (emacspeak-epub-title epub))
         (author  (emacspeak-epub-author epub)))
    (setf (gethash filename emacspeak-epub-db)
          (make-emacspeak-epub-metadata :title title :author author))
    (emacspeak-epub-bookshelf-save)
    (emacspeak-epub-bookshelf-redraw)
    (goto-char (point-min))
    (search-forward title)
    (call-interactively 'emacspeak-epub-open)))

(defun emacspeak-epub-bookshelf-remove-directory (directory &optional recursive)
  "Remove EPubs found in specified directory from the bookshelf.
Interactive prefix arg searches recursively in directory."
  (interactive "DRemove Directory: \nP")
  (cl-declare (special emacspeak-epub-db-file emacspeak-epub-db))
  (let ((updated 0)
        (filename nil))
    (cl-loop
     for f in
     (if recursive
         (emacspeak-epub-find-epubs-in-directory directory)
       (directory-files directory  'full "epub"))
     do
     (setq filename (shell-quote-argument f))
     (when (gethash filename emacspeak-epub-db)
       (cl-incf updated)
       (remhash filename emacspeak-epub-db)))
    (unless (zerop updated)
      (emacspeak-epub-bookshelf-save)
      (emacspeak-epub-bookshelf-redraw)
      (message "Removed %d books. " updated))))

(defun emacspeak-epub-bookshelf-remove-this-book ()
  "Remove the book on current line from this bookshelf.
No book files are deleted."
  (interactive)
  (cl-declare (special  emacspeak-epub-db))
  (let ((epub (get-text-property (point) 'epub))
        (orig (point)))
    (when epub
      (remhash epub emacspeak-epub-db)
      (emacspeak-epub-bookshelf-save)
      (emacspeak-epub-bookshelf-redraw)
      (emacspeak-auditory-icon 'task-done)
      (goto-char orig)
      (emacspeak-speak-line))))

(defun emacspeak-epub-bookshelf-clear ()
  "Clear all books from bookshelf."
  (interactive)
  (cl-declare (special emacspeak-epub-db))
  (when
      (or (not (called-interactively-p 'interactive))
          (y-or-n-p "Clear bookshelf?"))
    (clrhash emacspeak-epub-db)
    (setq header-line-format
          (propertize "EPub Bookshelf" 'face 'bold))
    (emacspeak-epub-bookshelf-save)
    (emacspeak-epub-bookshelf-redraw)
    (message "Cleared bookshelf.")))


(defun emacspeak-epub-bookshelf-save ()
  "Save bookshelf metadata."
  (interactive)
  (cl-declare (special emacspeak-epub-db-file))
  (let ((buff (find-file-noselect emacspeak-epub-db-file))
        (emacspeak-speak-messages nil)
        (print-length  nil)
        (print-level nil))
    (save-current-buffer
      (set-buffer buff)
      (setq buffer-undo-list t)
      (erase-buffer)
      (print  emacspeak-epub-db  buff)
      (save-buffer buff)
      (kill-buffer buff)
      (when (called-interactively-p 'interactive)
        (emacspeak-auditory-icon 'save-object)))))

(defun emacspeak-epub-bookshelf-load ()
  "Load bookshelf metadata from disk."
  (interactive)
  (cl-declare (special emacspeak-epub-db emacspeak-epub-db-file))
  (when (file-exists-p emacspeak-epub-db-file)
    (let ((buffer (find-file-noselect emacspeak-epub-db-file)))
      (with-current-buffer buffer
        (goto-char (point-min))
        (setq emacspeak-epub-db (read buffer)))
      (kill-buffer buffer))))

(defun emacspeak-epub-bookshelf-open (bookshelf)
  "Load bookshelf metadata from specified bookshelf."
  (interactive
   (list
    (read-file-name "BookShelf: "
                    (expand-file-name emacspeak-epub-bookshelf-directory)
                    nil t nil
                    #'(lambda (s) (string-match "\\.bsf\\'" s)))))
  (cl-declare (special emacspeak-epub-db))
  (let ((buffer (find-file-noselect bookshelf))
        (bookshelf-name  (substring (file-name-nondirectory bookshelf) 0 -4)))
    (with-current-buffer buffer
      (goto-char (point-min))
      (setq emacspeak-epub-db (read buffer)))
    (kill-buffer buffer)
    (emacspeak-epub-bookshelf-redraw)
    (setq header-line-format
          (propertize
           (format "EPub Bookshelf: %s" bookshelf-name)
           'face 'bold))
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-header-line)))

;;}}}
;;{{{ Interactive Commands:

(defvar emacspeak-epub-interaction-buffer "*EPub*"
  "Buffer for EPub interaction.")

;;;###autoload
(defun emacspeak-epub ()
  "EPub  Interaction.
When opened, displays a bookshelf consisting of  epubs found at the
root directory,see \\[emacspeak-epub-mode]"
  (interactive)
  (cl-declare (special emacspeak-epub-interaction-buffer
                       emacspeak-epub-zip-info
                       emacspeak-epub-zip-extract))
  (unless emacspeak-epub-zip-extract
    (error "Please install unzip."))
  (unless emacspeak-epub-zip-info
    (error "Please install zipinfo. "))
  (let ((buffer (get-buffer emacspeak-epub-interaction-buffer)))
    (unless (buffer-live-p buffer)
      (with-current-buffer
          (get-buffer-create emacspeak-epub-interaction-buffer)
        (emacspeak-epub-mode)))
    (pop-to-buffer emacspeak-epub-interaction-buffer)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))


(defun emacspeak-epub-open (epub-file)
  "Open specified Epub.
Filename may need to  be shell-quoted when called from Lisp."
  (interactive
   (list
    (or
     (get-text-property (point) 'epub)
     (read-file-name "EPub: " emacspeak-epub-library-directory))))
  (let ((e (emacspeak-epub-make-epub epub-file)))
    (emacspeak-epub-browse-toc e)))

(defvar-local epub-this-epub nil
  "EPub handle.")

(declare-function eww-update-header-line-format "eww" nil)


(defun emacspeak-epub-eww (epub-file &optional use-ncx)
  "Display entire book  using EWW from EPub.
Use content listed in toc.ncx  if prefix-arg use-ncx is true.
Default is to  use   the sorted list of html files
in the epub file."
  (interactive
   (list
    (or
     (get-text-property (point) 'epub)
     (read-file-name "EPub: " emacspeak-epub-library-directory))
    current-prefix-arg))
  (cl-declare (special emacspeak-speak-directory-settings eww-data
                       epub-this-epub emacspeak-epub-this-epub))
  (let* ((emacspeak-speak-messages nil)
         (directory
          (string-trim
           (shell-command-to-string
            (format "cd %s; pwd"
                    (file-name-directory epub-file)))))
         (eww-epub (get-buffer-create "Full Text EPub"))
         (this-epub (emacspeak-epub-make-epub epub-file))
         (navs (emacspeak-epub-navs this-epub))
         (html (emacspeak-epub-html this-epub))
         (dom nil)
         (inhibit-read-only t))
    (cl-loop
     for f in
     (if use-ncx navs html)
     do
     (setq dom (emacspeak-epub-dom-from-archive epub-file f))
     (with-current-buffer eww-epub
       (setq buffer-undo-list t)
       (shr-insert-document (dom-by-tag dom 'body))))
    (with-current-buffer eww-epub
      (eww-mode)
      (setq
       emacspeak-epub-this-epub epub-file
       epub-this-epub this-epub
       default-directory directory)
      (emacspeak-speak-load-directory-settings directory)
      (rename-buffer
       (format
        "%s: %s"
        (emacspeak-epub-title this-epub) (emacspeak-epub-author this-epub))
       'unique)
      (plist-put eww-data :author (emacspeak-epub-author this-epub))
      (plist-put eww-data :title (emacspeak-epub-title this-epub))
      (eww-update-header-line-format)
      (when emacspeak-eww-post-process-hook
        (emacspeak-eww-run-post-process-hook))
      (goto-char (point-min))
      (emacspeak-auditory-icon 'open-object))
    (funcall-interactively #'switch-to-buffer eww-epub)))






(defvar emacspeak-epub-google-search-template
  "http://books.google.com/books/feeds/volumes?min-viewability=full&epub=epub&q=%s"
  "REST  end-point for performing Google Books Search
to find Epubs  having full viewability.")


(defun emacspeak-epub-google (query)
  "Search for Epubs from Google Books."
  (interactive "sGoogle Books Query: ")
  (cl-declare (special emacspeak-epub-google-search-template))
  (emacspeak-feeds-atom-display
   (format emacspeak-epub-google-search-template
           (url-hexify-string query))))

(defun emacspeak-epub-next ()
  "Move to next book."
  (interactive)
  (end-of-line)
  (goto-char (next-single-property-change (point) 'epub))
  (beginning-of-line)
  (emacspeak-speak-line)
  (emacspeak-auditory-icon 'select-object))

(defun emacspeak-epub-previous ()
  "Move to previous book."
  (interactive)
  (beginning-of-line)
  (goto-char (previous-single-property-change (point) 'epub))
  (beginning-of-line)
  (emacspeak-speak-line)
  (emacspeak-auditory-icon 'select-object))

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

(defun emacspeak-epub-gutenberg-download-uri (book-id)
  "Return URL  for downloading Gutenberg EBook."
  (cl-declare (special emacspeak-epub-gutenberg-suffix
                       emacspeak-epub-gutenberg-mirror))
  (format "%s%s%s"
          emacspeak-epub-gutenberg-mirror
          book-id
          emacspeak-epub-gutenberg-suffix))

(defun emacspeak-epub-gutenberg-browse-uri (book-id)
  "Return URL  for browsing Gutenberg EBook."
  (cl-declare (emacspeak-epub-gutenberg-suffix emacspeak-epub-gutenberg-mirror))
  (format "%s%s"
          emacspeak-epub-gutenberg-mirror book-id))


(defun emacspeak-epub-gutenberg-download (book-id &optional download)
  "Open web page for specified book.
Place download url for epub in kill ring.
With interactive prefix arg `download', download the epub."
  (interactive
   (list (read-from-minibuffer "Book-Id:")
         current-prefix-arg))
  (let ((file
         (expand-file-name
          (format "%s%s" book-id emacspeak-epub-gutenberg-suffix)
          emacspeak-epub-library-directory))
        (browse (emacspeak-epub-gutenberg-browse-uri book-id))
        (url (emacspeak-epub-gutenberg-download-uri book-id)))
    (cond
     ((file-exists-p file)
      (browse-url browse)
      (message "Book available locally as %s" file))
     (t (kill-new   url)
        (browse-url browse)
        (when download
          (shell-command
           (format"%s -O %s '%s'"
                  emacspeak-epub-wget file url))
          (message "Downloaded content to %s" file))))))

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
  (cl-declare (special emacspeak-epub-gutenberg-catalog-url
                       emacspeak-epub-gutenberg-catalog-file
                       emacspeak-epub-wget))
  (unless emacspeak-epub-wget
    (error "Please install wget. "))
  (unless
      (file-exists-p (file-name-directory emacspeak-epub-gutenberg-catalog-file))
    (make-directory
     (file-name-directory emacspeak-epub-gutenberg-catalog-file) 'parents))
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
;;{{{ Calibre Hookup:

;;; Inspired by https://github.com/whacked/calibre-mode.git

(defcustom emacspeak-epub-calibre-root-dir
  (expand-file-name "calibre" emacspeak-epub-library-directory)
  "Root of Calibre library."
  :type 'directory
  :group 'emacspeak-epub)

(defvar   emacspeak-epub-calibre-sqlite
  (eval-when-compile (executable-find "sqlite3"))
  "Path to sqlite3.")

(defvar emacspeak-epub-calibre-db
  (expand-file-name "metadata.db" emacspeak-epub-calibre-root-dir)
  "Calibre database.")

;;; Record returned by queries:

(cl-defstruct emacspeak-epub-calibre-record
;;; "b.title,  b.author_sort, b.path,  d.format"
  title author  path format)

;;; Helper: Construct query
(defun emacspeak-epub-calibre-build-query (where &optional limit)
  "Build a Calibre query as SQL statement.
Argument  `where' is a simple SQL where clause."
  (concat
   "select "
   "b.title,  b.author_sort, b.path,  d.format"
   " from data as d "
   "left outer join books as b on d.book = b.id "
   " where "
   where
   (when limit (format "limit %s" limit))))

(defun emacspeak-epub-calibre-query (pattern)
  "Return  search query matching `pattern'.
Searches for matches in both  Title and Author."
  (emacspeak-epub-calibre-build-query
   (format
    "lower(b.author_sort) LIKE '%%%s%%' OR lower(b.title) LIKE '%%%s%%' "
    (downcase pattern) (downcase pattern))))

(defun emacspeak-epub-calibre-title-query (pattern)
  "Return title search query matching `pattern'."
  (emacspeak-epub-calibre-build-query
   (format "lower(b.title) like '%%%s%%' "
           (downcase pattern))))

(defun emacspeak-epub-calibre-author-query (pattern)
  "Return author search query matching `pattern'."
  (emacspeak-epub-calibre-build-query
   (format "lower(b.author_sort) like '%%%s%%' "
           (downcase pattern))))

(defun emacspeak-epub-calibre-get-results (query)
  "Execute query against Calibre DB, and return parsed results."
  (cl-declare (special emacspeak-epub-calibre-db emacspeak-epub-calibre-sqlite))
  (let ((fields nil)
        (result nil)
        (calibre (get-buffer-create " *Calibre Results *")))
    (with-current-buffer  calibre
      (erase-buffer)
      (setq buffer-undo-list t)
      (shell-command
       (format
        "%s -list -separator '@@' %s \"%s\" 2>/dev/null"
        emacspeak-epub-calibre-sqlite emacspeak-epub-calibre-db query)
       calibre)
      (goto-char (point-min))
      (while (not (eobp))
        (setq fields
              (split-string
               (buffer-substring-no-properties
                (line-beginning-position)  (line-end-position))
               "@@"))
        (when (= (length fields) 4)
          (push
           (make-emacspeak-epub-calibre-record
            :title (cl-first fields)
            :author (cl-second fields)
            :path (cl-third fields)
            :format (cl-fourth fields))
           result))
        (forward-line 1)))
    result))

;;}}}
;;{{{ Add  to bookshelf using calibre search:

(defvar emacspeak-epub-calibre-results nil
  "Results from most recent Calibre search.")

(defun emacspeak-epub-bookshelf-calibre-search (pattern)
  "Add results of an title/author search to current bookshelf."
  (interactive "sSearch For: ")
  (cl-declare (special emacspeak-epub-calibre-root-dir
                       emacspeak-epub-calibre-results))
  (unless (eq major-mode 'emacspeak-epub-mode)
    (error "Not in an Emacspeak Epub Bookshelf."))
  (let ((emacspeak-speak-messages nil)
        (results
         (emacspeak-epub-calibre-get-results
          (emacspeak-epub-calibre-query pattern))))
    (when (= 0 (length results)) (error "No results found, check query."))
    (cl-loop
     for r in results
     do
     (emacspeak-epub-bookshelf-add-directory
      (expand-file-name (emacspeak-epub-calibre-record-path r)
                        emacspeak-epub-calibre-root-dir)))
    (setq emacspeak-epub-calibre-results results)
    (dtk-speak-and-echo  (format "Added %d books" (length results)))))

(defun emacspeak-epub-bookshelf-calibre-author (pattern)
  "Add results of an author search to current bookshelf."
  (interactive "sAuthor: ")
  (cl-declare (special emacspeak-epub-calibre-root-dir
                       emacspeak-epub-calibre-results))
  (unless (eq major-mode 'emacspeak-epub-mode)
    (error "Not in an Emacspeak Epub Bookshelf."))
  (let ((emacspeak-speak-messages nil)
        (results
         (emacspeak-epub-calibre-get-results
          (emacspeak-epub-calibre-author-query pattern))))
    (when (= 0 (length results)) (error "No results found, check query."))
    (cl-loop
     for r in results
     do
     (emacspeak-epub-bookshelf-add-directory
      (expand-file-name (emacspeak-epub-calibre-record-path r)
                        emacspeak-epub-calibre-root-dir)))
    (setq emacspeak-epub-calibre-results results)
    (dtk-speak-and-echo  (format "Added %d books" (length results)))))

(defun emacspeak-epub-bookshelf-calibre-title (pattern)
  "Add results of an title search to current bookshelf."
  (interactive "sTitle: ")
  (cl-declare (special emacspeak-epub-calibre-root-dir
                       emacspeak-epub-calibre-results))
  (unless (eq major-mode 'emacspeak-epub-mode)
    (error "Not in an Emacspeak Epub Bookshelf."))
  (let ((emacspeak-speak-messages nil)
        (results
         (emacspeak-epub-calibre-get-results
          (emacspeak-epub-calibre-title-query pattern))))
    (when (= 0 (length results)) (error "No results found, check query."))
    (cl-loop
     for r in results
     do
     (emacspeak-epub-bookshelf-add-directory
      (expand-file-name (emacspeak-epub-calibre-record-path r)
                        emacspeak-epub-calibre-root-dir)))
    (setq emacspeak-epub-calibre-results results)
    (dtk-speak-and-echo  (format "Added %d books" (length results)))))

(define-derived-mode emacspeak-calibre-mode special-mode
  "Calibre Interaction On The Emacspeak Audio Desktop"
  "A Calibre Front-end.
Letters do not insert themselves; instead, they are commands.
\\<emacspeak-calibre-mode-map>
\\{emacspeak-calibre-mode-map}"
  (setq buffer-undo-list t)
  (setq header-line-format
        (propertize "Calibre Results" 'face 'bold))
  (goto-char (point-min))
  (cd-absolute emacspeak-epub-library-directory))

(defun emacspeak-epub-calibre-dired-at-point ()
  "Open directory containing current result."
  (interactive)
  (unless (eq major-mode 'emacspeak-calibre-mode)
    (error "Not in a Calibre Results buffer"))
  (let ((path (get-text-property (point) 'path)))
    (unless path (error "No valid result here"))
    (dired (expand-file-name path emacspeak-epub-calibre-root-dir))
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(cl-declaim (special emacspeak-calibre-mode-map))
(define-key emacspeak-calibre-mode-map "\C-m"
  'emacspeak-epub-calibre-dired-at-point)

(defun emacspeak-epub-calibre-results ()
  "Show most recent Calibre search results."
  (interactive)
  (cl-declare (special emacspeak-epub-calibre-results))
  (let ((inhibit-read-only  t)
        (buffer (get-buffer-create "*Calibre Results*"))
        (start nil))
    (with-current-buffer buffer
      (erase-buffer)
      (setq buffer-undo-list t)
      (goto-char (point-min))
      (insert "Calibre Results\n\n")
      (cl-loop
       for r in emacspeak-epub-calibre-results
       do
       (setq start (point))
       (insert
        (format "%s\t%s\t%s"
                (emacspeak-epub-calibre-record-title r)
                (emacspeak-epub-calibre-record-author r)
                (emacspeak-epub-calibre-record-format r)))
       (put-text-property start (point)
                          'path (emacspeak-epub-calibre-record-path r))
       (insert "\n"))
      (setq buffer-read-only t)
      (emacspeak-calibre-mode)
      (goto-char (point-min))
      (forward-line 2))
    (switch-to-buffer buffer)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

;;}}}
;;{{{ Locate epub using Locate:
(defun emacspeak-epub-locate-epubs (pattern)
  "Locate epub files using locate."  (interactive "sSearch Pattern: ")
  (cl-declare (special locate-command locate-make-command-line))
  (let ((locate-make-command-line #'(lambda (s) (list locate-command "-i" s))))
    (locate-with-filter pattern "\\.epub\\'")))

;;}}}
;;{{{ nov Integration:

(defun emacspeak-epub-open-with-nov ()
  "Open ebook at point in nov-mode."
  (interactive)
  (cl-assert (eq major-mode 'emacspeak-epub-mode)  nil  "Buffer is not
in emacspeak-epub-mode")
  (let ((epub (emacspeak-epub-shell-unquote (get-text-property (point) 'epub))))
    (cl-assert epub nil "No epub  at point.")
    (cl-assert (file-exists-p epub) nil "File does not exist")
    (unless (locate-library "nov") nil "Package nov is  not installed.")
    (funcall-interactively #'find-file epub)))

;;}}}
(provide 'emacspeak-epub)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
