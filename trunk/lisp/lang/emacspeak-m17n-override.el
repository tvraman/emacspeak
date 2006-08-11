;;; emacspeak-m17n-speak.el --- Overriding extended functions for emacspeak-speak

;; Copyright (C) 2002  Free Software Foundation, Inc.

;; Author: Koichi INOUE <inoue@argv.org>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file contains modified version of functions in emacspeak-speak.el.

;;; Code:

(defun emacspeak-speak-char (&optional prefix)
  "Speak character under point.
Pronounces character phonetically unless  called with a PREFIX arg."
  (interactive "P")
  (let ((dtk-stop-immediately t )
        (char  (following-char ))
	(lang (or (get-text-property (point) 'emacspeak-language)
		  dtk-default-language
		  'en)))
    (when char
      (emacspeak-handle-action-at-point)
      (cond
       ((and (not (eq lang 'en)) 	; chara with language property
	     (featurep 'emacspeak-m17n))
	(if (not prefix)
	    (dtk-speak (emacspeak-m17n-get-phonetic-string char lang) nil lang)
	  (dtk-speak (emacspeak-m17n-get-cursor-string char lang) nil lang)))
       ((and (not prefix)
             (emacspeak-is-alpha-p char))
        (dtk-speak (emacspeak-get-phonetic-string char )))
       ((emacspeak-is-alpha-p char) (dtk-letter (char-to-string char )))
       (t (dtk-dispatch
           (dtk-char-to-speech char )))))))

(defun emacspeak-speak-buffer (&optional arg)
  "Speak current buffer  contents.
With prefix ARG, speaks the rest of the buffer from point.
Negative prefix arg speaks from start of buffer to point.
 If voice lock mode is on, the paragraphs in the buffer are
voice annotated first,  see command `emacspeak-speak-voice-annotate-paragraphs'."
  (interactive "P" )
  (declare (special emacspeak-speak-voice-annotated-paragraphs))
  (when (and voice-lock-mode
             (not emacspeak-speak-voice-annotated-paragraphs))
    (emacspeak-speak-voice-annotate-paragraphs))
  (when (listp arg) (setq arg (car arg )))
  (let ((start nil )
        (end nil))
    (cond
     ((null arg)
      (setq start (point-min)
            end (point-max)))
     ((> arg 0)
      (setq start (point)
            end (point-max)))
     (t (setq start (point-min)
              end (point))))
    (when (featurep 'emacspeak-m17n)
      (emacspeak-m17n-put-language-region start end))
    (dtk-speak (buffer-substring start end ))))

;;{{ From emacspeak-redefine
(defun emacspeak-self-insert-command (arg)
  "Insert a character.
Speaks the character if emacspeak-character-echo is true.
See  command emacspeak-toggle-word-echo bound to
\\[emacspeak-toggle-word-echo].
Toggle variable dtk-stop-immediately-while-typing if you want to have
speech flush as you type."
  (interactive "p")
  (declare (special last-input-char
                    dtk-stop-immediately-while-typing dtk-program 
                    buffer-undo-list  buffer-read-only
                    emacspeak-character-echo
                    emacspeak-word-echo))
  (when buffer-read-only
    (signal 'buffer-read-only
            (list (current-buffer))))
  (unless (car buffer-undo-list)
    (pop buffer-undo-list ))
  (self-insert-command  arg )
  (cond
   ((and emacspeak-word-echo
         (interactive-p)
	 (= last-input-char 32 ))
    (save-excursion
      (condition-case nil
          (forward-word -1)
        (error nil))
      (emacspeak-speak-word)))
   ((and emacspeak-character-echo
         (interactive-p ))
    (when dtk-stop-immediately-while-typing (dtk-stop))
    (if (featurep 'emacspeak-m17n)
	(and (or emacspeak-character-echo-non-ascii
		 (< last-input-char 255))
	     (emacspeak-m17n-speak-this-char last-input-char
					     (emacspeak-m17n-maybe-last-input-language)))
      (emacspeak-speak-this-char last-input-char))))
  (and
   (= (char-syntax  last-input-char) 32)
   (>= (current-column) fill-column)
   auto-fill-function
   (funcall auto-fill-function)))
;;}}

(provide 'emacspeak-m17n-override)
;;; emacspeak-m17n-overrice.el ends here
