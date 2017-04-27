;;; w3-hot.el --- Main functions for emacs-w3 on all platforms/versions
;; Author: $Author$
;; Created: $Date$
;; Version: $Revision$
;; Keywords: faces, help, comm, news, mail, processes, mouse, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993 - 1996 by William M. Perry <wmperry@cs.indiana.edu>
;;; Copyright (c) 1996 - 1999 Free Software Foundation, Inc.
;;;
;;; This file is part of GNU Emacs.
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
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structure for hotlists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (
;;;  ("name of item1" . "http://foo.bar.com/")    ;; A single item in hotlist
;;;  ("name of item2" . (                         ;; A sublist
;;;                      ("name of item3" . "http://www.ack.com/")
;;;                     ))
;;; )  ; end of hotlist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'w3-vars)
(require 'w3-parse)
(require 'url-parse)
(eval-when-compile (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hotlist Handling Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-html-bookmarks nil)

;;;###autoload
(defun w3-read-html-bookmarks (fname)
  "Import an HTML file into the Emacs-w3 format."
  (interactive "fBookmark file: ")
  (if (not (file-readable-p fname))
      (error "Can not read %s..." fname))
  (save-excursion
    (set-buffer (get-buffer-create " *bookmark-work*"))
    (erase-buffer)
    (insert-file-contents fname)
    (let* ((w3-debug-html nil)
	   (bkmarks nil)
	   (parse (w3-parse-buffer (current-buffer))))
      (setq parse w3-last-parse-tree
	    bkmarks (nreverse (w3-grok-html-bookmarks parse))
	    w3-hotlist bkmarks))))

(eval-when-compile
  (defsubst w3-hot-push-new-menu ()
    (declare (special cur-stack))
    (setq cur-stack (cons (list "") cur-stack)))

  ;; This stores it in menu format
  '(defsubst w3-hot-push-new-item (title href)
    (declare (special cur-stack))
    (setcar cur-stack (cons (vector title (list 'w3-fetch href) t)
			    (car cur-stack))))

  ;; This stores it in alist format
  (defsubst w3-hot-push-new-item (title href)
    (declare (special cur-stack))
    (setcar cur-stack (cons (cons title href) (car cur-stack))))
  
  (defsubst w3-hot-finish-submenu ()
    (declare (special cur-stack cur-title))
    (let ((x (nreverse (car cur-stack)))
	  (y (pop cur-title)))
      (while (string= y "")
	(setq y (pop cur-title)))
      (and x (setcar x y))
      (setq cur-stack (cdr cur-stack))
      (if cur-stack
	  (setcar cur-stack (cons x (car cur-stack)))
	(setq cur-stack (list x)))))
  )

(defun w3-grok-html-bookmarks-internal (tree)
  (declare (special cur-stack cur-title))
  (let (node tag content args)
    (while tree
      (setq node (car tree)
	    tree (cdr tree)
	    tag (and (listp node) (nth 0 node))
	    args (and (listp node) (nth 1 node))
	    content (and (listp node) (nth 2 node)))
      (cond
       ((eq tag 'hr)
	(setq cur-title '("------")))
       ((eq tag 'title)
	(setq cur-title (list (w3-normalize-spaces (car content))))
	(w3-grok-html-bookmarks-internal content))
       ((memq tag '(dl ol ul))
	(w3-hot-push-new-menu)
	(w3-grok-html-bookmarks-internal content)
	(w3-hot-finish-submenu))
       ((and (memq tag '(dt li p))
	     (stringp (car content)))
	(setq cur-title (cons (w3-normalize-spaces (car content))
			      cur-title)))
       ((and (eq tag 'a)
	     (stringp (car-safe content))
	     (cdr-safe (assq 'href args)))
	(w3-hot-push-new-item (w3-normalize-spaces (car-safe content))
			      (cdr-safe (assq 'href args))))
       (content
	(w3-grok-html-bookmarks-internal content))))))

(defun w3-grok-html-bookmarks (chunk)
  (let (
	cur-title
	cur-stack
	)
    (w3-grok-html-bookmarks-internal chunk)
    (reverse (car cur-stack))))

(defun w3-hot-convert-to-alist-mapper (node)
  (declare (special prefix alist))
  (cond
   ((stringp node)
    ;; Top-level node... ignore
    )
   ((stringp (cdr node))
    ;; A real hyperlink, push it onto the alist
    (push (cons (if prefix (concat prefix " / " (car node)) (car node)) (cdr node)) alist))
   (t
    ;; A submenu, add to prefix and recurse
    (w3-hot-convert-to-alist-internal
     (cdr node) (if prefix (concat prefix " / " (car node)) (car node))))))

(defun w3-hot-convert-to-alist-internal (l &optional prefix)
  (mapc 'w3-hot-convert-to-alist-mapper l))

(defun w3-hot-convert-to-alist (l)
  (let ((alist nil))
    (w3-hot-convert-to-alist-internal l)
    alist))
       
(defun w3-delete-from-alist (x alist)
  ;; Remove X from ALIST, return new alist
  (if (eq (assoc x alist) (car alist)) (cdr alist)
    (delq (assoc x alist) alist)))

(defun w3-hotlist-parse-old-mosaic-format ()
  (let (cur-link cur-alias)
    (while (re-search-forward "^\n" nil t) (replace-match ""))
    (goto-line 3)
    (while (not (eobp))
      (re-search-forward "^[^ ]*" nil t)
      (setq cur-link (buffer-substring (match-beginning 0) (match-end 0)))
      (setq cur-alias (buffer-substring (progn
					  (forward-line 1)
					  (beginning-of-line)
					  (point))
					(progn
					  (end-of-line)
					  (point))))
      (if (not (equal cur-alias ""))
	  (setq w3-hotlist (cons (list cur-alias cur-link) w3-hotlist))))))

;;;###autoload
(defun w3-parse-hotlist (&optional fname)
  "Read in the hotlist specified by FNAME"
  (if (not fname) (setq fname w3-hotlist-file))
  (setq w3-hotlist nil)
  (if (not (file-exists-p fname))
      (message "%s does not exist!" fname)
    (let* ((old-buffer (current-buffer))
	   (buffer (get-buffer-create " *HOTW3*"))
	   (case-fold-search t))
      (set-buffer buffer)
      (erase-buffer)
      (insert-file-contents fname)
      (goto-char (point-min))
      (cond
       ((looking-at "ncsa-xmosaic-hotlist-format-1");; Old-style NCSA Mosaic
	(w3-hotlist-parse-old-mosaic-format))
       ((or (looking-at "<!DOCTYPE")	; Some HTML style, including netscape
	    (re-search-forward "<a[ \n]+href" nil t))
	(w3-read-html-bookmarks fname))
       (t
	(message "Cannot determine format of hotlist file: %s" fname)))
      (set-buffer-modified-p nil)
      (kill-buffer buffer)
      (set-buffer old-buffer))))

;;;###autoload
(defun w3-use-hotlist ()
  "Possibly go to a link in your W3/Mosaic hotlist.
This is part of the emacs World Wide Web browser.  It will prompt for
one of the items in your 'hotlist'.  A hotlist is a list of often
visited or interesting items you have found on the World Wide Web."
  (interactive)
  (if (not w3-setup-done) (w3-do-setup))
  (if (not w3-hotlist) (message "No hotlist in memory!")
    (let* ((completion-ignore-case t)
	   (hot-alist (w3-hot-convert-to-alist w3-hotlist))
	   (url (cdr (assoc
		      (completing-read "Goto Document: " hot-alist nil t)
		      hot-alist))))
      (if (string= "" url) (error "No document specified!"))
      (w3-fetch url))))

;;;###autoload
(defun w3-hotlist-add-document-at-point (pref-arg)
  "Add the document pointed to by the hyperlink under point to the hotlist."
  (interactive "P")
  (let ((url (w3-view-this-url t))
	(widget (widget-at (point)))
	(title nil))
    (or url (error "No link under point."))
    (if (and (widget-get widget :from)
	     (widget-get widget :to))
	(setq title (buffer-substring (widget-get widget :from)
				      (widget-get widget :to))))
    (w3-hotlist-add-document pref-arg (or title url) url)))

;;;###autoload
(defun w3-hotlist-add-document (pref-arg &optional the-title the-url)
  "Add this documents url to the hotlist"
  (interactive "P")
  (error "Adding to hotlist not implemented yet."))

;;;###autoload
(defun w3-hotlist-delete ()
  "Deletes a document from your hotlist file"
  (interactive)
  (error "Deleting from hotlist not implemented yet."))

;;;###autoload
(defun w3-hotlist-refresh ()
  "Reload the default hotlist file into memory"
  (interactive)
  (w3-do-setup)
  (w3-parse-hotlist))

;;;###autoload
(defun w3-hotlist-apropos (regexp)
  "Show hotlist entries matching REGEXP."
  (interactive "sW3 Hotlist Apropos (regexp): ")
  (or w3-setup-done (w3-do-setup))
  (w3-fetch (concat "hotlist:search?regexp=" (url-hexify-string regexp))))

;;;###autoload
(defun w3-hotlist-view ()
  "Show the hotlist."
  (interactive)
  (w3-fetch "hotlist:view"))

(provide 'w3-hot)
