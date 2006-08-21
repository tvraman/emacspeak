;; Hi All,

;; I thought it was about time I started offering up some of my rough and
;; ready elisp code which others might find useful. To start of, I
;; thought I'd offer my little package for converting different file
;; types into text and viewing within emacspeak. This little package
;; allows you to view .doc, .pdf, .ps, .ppt files as text with a single
;; key press. It uses fairly standard Linux utilities (see commentary in
;; file). I've also setup things so that if the file is .htm or .html, it 
;; will use the browse-url package to display the rendered version of the 
;; file, rather than the raw file contents. I'n my setup, the
;; tx-dired-view-file function is bound to f6, so when in dired, hitting
;; f6 will cause the file under point to either be converted to text and
;; displayed or if it is html, displayed with your browser - otherwise,
;; it just calls dired-view-file (same as hitting v within dired). 

;; Any feedback on improvements, style or bugs always welcome. While I've
;; been hacking away at elisp for quite some time, I don't usually get
;; more than an hour here or there and have never really done much more
;; than some semi-fancy customizations and some bug squashing. I've made
;; considerable changes over the weekend - in fact, the code doesn't look
;; anything like what I've actually been using for some months, so there
;; could be some really bad bugs - use at your own risk. 

;; Raman, if you have time, I'd value any feedback you can provide. 

;; Tim
;; -----------------------------------------------------------------
;;; Start
;;;      Filename: tx-convert.el
;;; Creation Date: Sunday, 25 July 2004 06:40 PM EST
;;; Last Modified: Sunday, 10 April 2005 07:28 PM EST
;;;        Author: Tim Cross <tcross@pobox.une.edu.au>
;;;   Description: Collection of functions to convert doc/pdf/ppt/ps files
;;;                to text from within dired
;;;
;;; Copyright (C) 2005. Tim Cross <tcross@une.edu.au>
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
;;;
;;; Commenta
;;; The simple idea wiht this very basic bits of code bits was to make
;;; life just a little bit easier and require me to convert files in a 
;;; shell prior to being able to view them within emacspeak and have them 
;;; make any sense. 
;;;
;;; You need the following packages (or at least utilities which will
;;; do the same thing). Most of these are fairly standard with many Linux 
;;; distros these days. 
;;; The wv utilities which contain wvText for converting MS Word docs
;;; The xpdf utilities which include pdftotext for converting PDF to text
;;; The Ghostscript package which contains pstotext for converting PS to text
;;; The ppthtml utility for converting MS Power Point files to html
;;; A configured and working browse-url setup. I use w3m as my browser
;;;
;;; Installation 
;;; Pretty straight forward. Place this file somewhere in your load path 
;;; and put a (require 'tx-convert) in your .emacs. You may want to byte
;;; compile this file.
;;; I bind the function tx-dired-view-file to a key in dired so that I 
;;; can view any of the 'special' files with a single key press e.g.
;;;(add-hook 'dired-mode-hook
;;;		  (function (lambda ()
;;;					  (local-set-key [f6] 'tx-dired-view-file))))
;;;
;;; This is the first bit of elisp I've allowed out into the world and 
;;; while I am really learning to love both elisp and cl lisp, I'm still 
;;; very much a novice. Therefore, there IS bugs and probably some pretty 
;;; poor style within this stuff. Feedback, bug reports and suggestions 
;;; always welcome. 
;;;
;;; Emacpseka Users Note - I've not attempted to enhance this code to provide 
;;; better spoken or audio icon feedback. When the code matures a bit and
;;; once I get some feedback, I will see if a re-worked version can be 
;;; included in emacspeak. In the meantime, feel free to use 'advice' to 
;;; improve things. 
 
(defvar tx-msword-to-text-prog "/usr/bin/wvText"
  "Program used to convert Word document to plain text.")

(defvar tx-pdf-to-text-prog "/usr/bin/pdftotext"
  "Program used to convert PDF files to plain ttext.")

(defvar tx-ps-to-text-prog "/usr/bin/pstotext"
  "Program used to convert PS files to plain text.")

(defvar tx-ppt-to-html-prog "/usr/bin/ppthtml"
  "Program used to convert PowerPoint slides to HTML.")

(defun tx-do-file-translate (cmd arg1 arg2)
  "Do the actual conversion of a word doc."
  (message (format "Cmd %s Arg1 %s Arg2 %s" cmd arg1 arg2))
  (if (= 0 (shell-command (format "%s %s %s" cmd arg1 arg2)
						  "*Translate Output*"))
	  t
	nil))

(defun tx-expand-quote-filename (filename)
  "Quote shell meta characters and expand filename."
  (shell-quote-argument (expand-file-name filename)))
 
(defun tx-doc-to-text (word-doc)
  "Convert MS Word document to text and display."
  (interactive "fView MS Word doc file: ")
  (let ((output-file (concat word-doc ".txt")))
	(if (and (file-readable-p word-doc)
			 (tx-do-file-translate tx-msword-to-text-prog
								   (tx-expand-quote-filename word-doc)
								   (tx-expand-quote-filename output-file)))
		(view-file output-file)
	  (error "Could not translate %s from MS Word format to text" word-doc))))

(defun tx-pdf-to-text (pdf-file)
  "Convert a PDF file to plain text and view it."
  (interactive "fView PDF file: ")
  (let ((output-file (concat pdf-file ".txt")))
	(if (and (file-readable-p pdf-file)
			 (tx-do-file-translate tx-pdf-to-text-prog
								   (tx-expand-quote-filename pdf-file)
								   (tx-expand-quote-filename output-file)))
		(view-file output-file)
	  (error "Could not translate %s from PDF to text" pdf-file))))

(defun tx-ps-to-text (ps-file)
  "Convert a PS file to plain text and view it."
  (interactive "fView PostScript file: ")
  (let ((output-file (concat ps-file ".txt")))
	(if (and (file-readable-p ps-file)
			 (tx-do-file-translate 
			  tx-ps-to-text-prog
			  (concat "-output " (tx-expand-quote-filename output-file))
			  (tx-expand-quote-filename ps-file)))
		(view-file output-file)
	  (error "Could not translate %s from PS to text" ps-file))))

(defun tx-ppt-to-html (ppt-file)
  "convert a PPT file to HTML and view it."
  (interactive "fView PPT file: ")
  (let ((output-file (concat ppt-file ".html")))
	(if (and (file-readable-p ppt-file)
			 (tx-do-file-translate 
			  tx-ppt-to-html-prog
			  (tx-expand-quote-filename ppt-file)
			  (concat "> " (tx-expand-quote-filename output-file))))
		(browse-url-of-file (expand-file-name output-file))
	  (error "Could not translate %s from PPT to HTML" ppt-file))))

(defun tx-dired-view-file ()
  "View files within dired, possibly requiring translation."
  (interactive)
  (let ((dired-file (dired-get-filename)))
	(cond
	 ((string-match "\\.doc$" dired-file)
	  (tx-doc-to-text dired-file))
	 ((string-match "\\.pdf$" dired-file)
	  (tx-pdf-to-text dired-file))
	 ((string-match "\\.ps$" dired-file)
	  (tx-ps-to-text dired-file))
	 ((string-match "\\.ppt$" dired-file)
	  (tx-ppt-to-html dired-file))
	 ((string-match "\\.htm$\\|\\.html$" dired-file)
	  (browse-url-of-file dired-file))
	 (t
	  (dired-view-file)))))


(provide 'tc-convert)
