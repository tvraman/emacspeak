;;; emacspeak-egg.el --- Fluent spoken access to Egg4 Input Methods
;;; $Id$
;;; $Author$ 
;;; Description:  Emacspeak extension to speech enable mew
;;; Keywords: Emacspeak, Mew, IM, mail, Advice, Spoken Output
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
;;;Copyright (C) 1995 -- 2000, T. V. Raman 
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
(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-m17n-ja)
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice egg-convert-region (after emacspeak pre act)
  "Speak first candidate in the first bunsetsu"
  (let* ((inserted-string
	  (egg-get-major-bunsetsu-converted
	   (egg-get-major-bunsetsu (point))))
	 (inserted-string-phonetic
	  (emacspeak-ja-convert-string-to-phonetic inserted-string))
	 )
    (dtk-speak inserted-string-phonetic nil 'ja)
    )
)

(defadvice egg-forward-bunsetsu (after emacspeak pre act)
  "Speak current bunsetsu after movement"
  (let* ((inserted-string
	  (egg-get-major-bunsetsu-converted
	   (egg-get-major-bunsetsu (point))))
	 (inserted-string-phonetic
	  (emacspeak-ja-convert-string-to-phonetic inserted-string))
	 )
    (dtk-speak inserted-string-phonetic nil 'ja)
    )
)

(defadvice egg-backward-bunsetsu (after emacspeak pre act)
  "Speak current bunsetsu after movement"
  (let* ((inserted-string
	  (egg-get-major-bunsetsu-converted
	   (egg-get-major-bunsetsu (point))))
	 (inserted-string-phonetic
	  (emacspeak-ja-convert-string-to-phonetic inserted-string))
	 )
    (dtk-speak inserted-string-phonetic nil 'ja)
    )
)

(defadvice egg-insert-new-bunsetsu (around emacspeak (b tail new-b) pre act)
  "Speak inserted candidate"
  (let ((inserted-string-phonetic
	(emacspeak-ja-convert-string-to-phonetic
	(egg-get-major-bunsetsu-converted (car new-b)))
	))
    ad-do-it
    (dtk-speak inserted-string-phonetic nil 'ja)
    ad-return-value
    )
)

(defadvice egg-abort-conversion (after emacspeak pre act)
  "speak after aborting conversion"
  (dtk-speak "fence")
)

(defadvice its-cancel-input (after emacspeak pre act)
  "speak after aborting conversion"
  (dtk-speak "cancel")
)

(defadvice its-buffer-ins/del-SYL (after   emacspeak pre act )
  "read the  SYL(about what means SYL, please refer to its.el" 
	(if ad-return-value
	    (dtk-speak (emacspeak-ja-convert-string-to-cursor output) nil 'ja)))

(defadvice mlh-space-bar-backward-henkan (around emacspeak pre act)
  "Disable speak-messages for avoiding to speak Converting... every time"
  (let ((emacspeak-speak-messages nil))
    ad-do-it
     ad-return-value)
  )



(defconst its-mode-map
  (let ((map (make-sparse-keymap))
	(i 33))
    (define-key map "\C-a" 'its-beginning-of-input-buffer)
    (define-key map "\C-b" 'its-backward-SYL)
    (define-key map "\C-c" 'its-cancel-input)
    (define-key map "\C-d" 'its-delete-SYL)
;    (define-key map "\C-e" 'its-end-of-input-buffer)
    (define-key map emacspeak-prefix 'emacspeak-prefix-command)
    (define-key map "\C-f" 'its-forward-SYL)
    (define-key map "\C-g" 'its-select-previous-mode)
    (define-key map "\C-]" 'its-cancel-input)
    (define-key map "\C-h" 'its-mode-help-command)
    (define-key map "\C-k" 'its-kill-line)
;;    (define-key map "\C-l" 'its-exit-mode)
    (define-key map "\C-m" 'its-exit-mode)	; RET
    (define-key map [return] 'its-exit-mode)
    (define-key map "\C-t" 'its-transpose-chars)
    (define-key map "\C-w" 'its-kick-convert-region)
    (define-key map "\C-y" 'its-yank)
    (define-key map "\M-y" 'its-yank-pop)
    (define-key map [backspace] 'its-delete-backward-SYL)
    (define-key map [delete] 'its-delete-backward-SYL)
    (define-key map [M-backspace] 'its-delete-backward-SYL-by-keystroke)
    (define-key map [M-delete] 'its-delete-backward-SYL-by-keystroke)
    (define-key map [right] 'its-forward-SYL)
    (define-key map [left] 'its-backward-SYL)
    (while (< i 127)
      (define-key map (vector i) 'its-self-insert-char)
      (setq i (1+ i)))
    (define-key map " "    'its-kick-convert-region-or-self-insert)
    (define-key map "\177" 'its-delete-backward-SYL)
    ;;
    (define-key map "\M-p" 'its-previous-map)
    (define-key map "\M-n" 'its-next-map)
    (define-key map "\M-h" 'its-hiragana) ; hiragana-region for input-buffer
    (define-key map "\M-k" 'its-katakana)
    (define-key map "\M-<" 'its-half-width)
    (define-key map "\M->" 'its-full-width)
    map)
  "Keymap for ITS mode.")
(fset 'its-mode-map its-mode-map)

(defvar its-fence-mode nil)
(make-variable-buffer-local 'its-fence-mode)
(put 'its-fence-mode 'permanent-local t)


(provide 'emacspeak-egg)

