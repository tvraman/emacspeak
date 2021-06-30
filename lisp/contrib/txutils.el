;;      Filename: /home/tcross/projects/emacs-convert/txutils.el  -*- lexical-binding: t; -*-
;; Creation Date: Wednesday, 20 September 2006 10:13 PM EST
;; Last Modified: Monday, 19 July 2010 04:37 PM EST
;;       Version: 2.0
;;        Author: Tim Cross <tcross@une.edu.au>
;;   Description: Convert files from doc, ps, pdf, ppt to a format
;;                which can be viewed within emacs (i.e. text or html)

 ;;; Copyright (C) 2006. Tim Cross <tcross@une.edu.au>
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
 ;;;
 ;;; Commentary
 ;;; ==========
 ;;;
 ;;; The very simple idea behind this basic utility is to make accessing
 ;;; files in .doc, .pdf, .ps and .ppt more easily accessible without
 ;;; having to leave emacs or manually convert the file format prior
 ;;; to being able to view the contents in emacs.
 ;;;
 ;;; There are packages which will enable calls to external viewers
 ;;; for files of specific formats, such as xpdf for pdf etc. However,
 ;;; I wanted to have everything within emacs as this makes integration,
 ;;; cutting/pasting etc a lot easier, plus as a blind user, most
 ;;; external utilities are of little use because they don't also include
 ;;; speech support.
 ;;;
 ;;; The objective here is to have things setup so that when browsing
 ;;; a directory with dired, you can just hit 'v' for any file you want to
 ;;; view and you will be presented with a text or html version without
 ;;; needing to do any manual conversion - or even careing about what
 ;;; would need to be done.
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
 ;;; Customizing 
 ;;; ===========
 ;;;
 ;;; The easiest way to customize the conversion utility settings is to
 ;;; use M-x customize-group <RET> txutils <RET>
 ;;;
 ;;; In the txutils customize group, you will find just one setting, which
 ;;; is an alist of values indexed by a regular expression that matches against
 ;;; file extensions - a crude way of determining the source filetype
 ;;; (i.e. *.doc, *.pdf, *.ps, *.ppt, *.xls, *.html etc). See the
 ;;; documentation for more details.
 ;;;
 ;;; Installation 
 ;;; ============
 ;;; 
 ;;; Pretty straight forward. Place this file somewhere in your load path 
 ;;; and put a (require 'txutils) in your .emacs. You may want to byte
 ;;; compile this file.
 ;;;
 ;;; Thanks
 ;;; ======
 ;;;
 ;;; A number of people provided suggestions on how to improve both my elisp
 ;;; and the program itself. In particular, thanks goes to
 ;;; Lukas Loehrer
 ;;; Vinicius Jose Latorre
 ;;; Andreas Roehler
 ;;;
 ;;; plus a few others who gave general suggestions, feedback and
 ;;; encouragement. Thanks to everyone for taking the time and putting
 ;;; in the effort, its greatly appreciated.
 ;;; 
 ;;; Reporting Bugs
 ;;; ==============
 ;;; 
 ;;; This is the first bit of elisp I've allowed out into the world and 
 ;;; while I am really learning to love both elisp and cl lisp, I'm still 
 ;;; very much a novice. Therefore, there IS bugs and probably some pretty 
 ;;; poor style within this stuff. Feedback, bug reports and suggestions 
 ;;; always welcome. Send e-mail to tcross@une.edu.au
 ;;;
 ;;; Emacspeak Users Note - I've not attempted to enhance this code to provide 
 ;;; better spoken or audio icon feedback. When the code matures a bit and
 ;;; once I get some feedback, I will see if a re-worked version can be 
 ;;; included in emacspeak. In the meantime, feel free to use 'advice' to 
 ;;; improve things. 

(require 'custom)
(require 'browse-url)

                                        ; make-temp-file is part of apel prior to emacs 22
                                        ;(static-when (= emacs-major-version 21)
                                        ;  (require 'poe))

(defgroup txutils nil
  "Customize group for txutils."
  :prefix "txutils-"
  :group 'External)

(defcustom txutils-convert-alist
  '( ;; MS Word
    ("\\.\\(?:DOC\\|doc\\)$"     doc  "/usr/bin/wvText"    nil nil nil nil nil)
    ;; PDF
    ("\\.\\(?:PDF\\|pdf\\)$"     pdf  "/usr/bin/pdftotext" nil nil nil nil nil)
    ;; PostScript
    ("\\.\\(?:PS\\|ps\\)$"       ps   "/usr/bin/pstotext"  "-output" t nil nil nil)
    ;; MS PowerPoint
    ("\\.\\(?:PPT\\|ppt\\)$"     ppt  "/usr/bin/ppthtml"   nil nil nil t t))
  
  "*Association for program convertion.
 
 Each element has the following form:
 
 (REGEXP SYMBOL CONVERTER SWITCHES INVERT REDIRECT-INPUT REDIRECT-OUTPUT HTML-OUTPUT)
 
 Where:
 
 REGEXP		   is a regexp to match file type to convert.
 
 SYMBOL		   is a symbol to designate the fyle type.
 
 CONVERTER	   is a program to convert the fyle type to text or HTML.
 
 SWITCHES	   is a string which gives command line switches for the conversion 
                program. Nil means there are no switches needed.
 
 INVERT		   indicates if input and output program option is to be 
                inverted or not.  Non-nil means to invert, that is, output 
                option first then input option.  Nil means do not invert, 
                that is, input option first then output option.
 
 REDIRECT-INPUT indicates to use < to direct input from the input
                file. This is useful for utilities which accept input
                from stdin rather than a file.
 
 REDIRECT-OUTPUT indicates to use > to direct output to the output
                file. This is useful for utilities that only send output to 
                stdout.
 
 HTML-OUTPUT    Indicates the conversion program creates HTML output 
                rather than plain text."
  
  :type '(repeat
          (list :tag "Convertion"
                (regexp  :tag "File Type Regexp")
                (symbol  :tag "File Type Symbol")
                (string  :tag "Converter")
                (choice  :menu-tag "Output Option"
                         :tag "Output Option"
                         (const :tag "None" nil)
                         string)
                (boolean :tag "Invert I/O Option")
                (boolean :tag "Redirect Standard Input")
                (boolean :tag "Redirect Standard Output")
                (boolean :tag "HTML Output")))
  :group 'txutils)

(defun txutils-run-command (cmd &optional output-buffer)
  "Execute shell command with arguments, putting output in buffer."
  (= 0 (shell-command cmd (if output-buffer
                              output-buffer
                            "*txutils-output*")
                      (if output-buffer
                          "*txutils-output*"))))

(defun txutils-quote-expand-file-name (file-name)
  "Expand file name and quote special chars if required."
  (shell-quote-argument (expand-file-name file-name)))

(defun txutils-file-alist (file-name)
  "Return alist associated with file of this type."
  (let ((al txutils-convert-alist))
    (while (and al 
                (not (string-match (caar al) file-name)))
      (setq al (cdr al)))
    (if al
        (cdar al)
      nil)))

(defun txutils-make-temp-name (orig-name type-alist)
  "Create a temp file name from original file name"
  (make-temp-file (file-name-sans-extension 
                   (file-name-nondirectory orig-name)) nil
                   (if (nth 7 type-alist)
                       ".html"
                     ".txt")))

(defun txutils-build-cmd (input-file output-file type-alist)
  "Create the command string from conversion alist."
  (let ((f1 (if (nth 3 type-alist)
                output-file
              input-file))
        (f2 (if (nth 3 type-alist)
                input-file
              output-file)))
    (concat
     (nth 1 type-alist)
     (if (nth 2 type-alist)               ; Add cmd line switches
         (concat " " (nth 2 type-alist)))
     (if (nth 4 type-alist)          ; redirect input (which may be output
         (concat " < " f1)           ; if arguments are inverted!)
       (concat " " f1))
     (if (nth 5 type-alist)          ; redirect output (see above comment)
         (concat " > " f2)
       (concat " " f2)))))

(defun txutils-do-file-conversion (file-name)
  "Based on file extension, convert file to text. Return name of text file"
  (interactive "fFile to convert: ")
  (let ((f-alist (txutils-file-alist file-name))
        output-file)
    (when f-alist
      (message "Performing file conversion for %s." file-name)
      (setq output-file (txutils-make-temp-name file-name f-alist))
      (message "Command: %s" (txutils-build-cmd file-name output-file f-alist))
      (if (txutils-run-command 
           (txutils-build-cmd (txutils-quote-expand-file-name file-name)
                              (txutils-quote-expand-file-name 
                               output-file) f-alist))
          output-file
        file-name))))

(defadvice view-file (around txutils pre act comp)
  "Perform file conversion or call web browser to view contents of file."
  (let ((file-arg (ad-get-arg 0)))
    (if (txutils-file-alist file-arg)
        (ad-set-arg 0 (txutils-do-file-conversion file-arg)))
    (if (string-match "\\.\\(?:HTML?\\|html?\\)$" (ad-get-arg 0))
        (browse-url-of-file (ad-get-arg 0))
      ad-do-it)))

(provide 'txutils)



