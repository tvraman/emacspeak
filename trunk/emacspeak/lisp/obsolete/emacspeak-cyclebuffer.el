;; emacspeak-cyclebuffer.el --- Speech-enable cyclebuffer.el 
;;;$Id$
;; Copyright  (C)  2005  Sergei V. Fleytin <fleytin@mail.ru>
;; Version: 1.0
;; Author: Sergei V. Fleytin <fleytin@mail.ru>
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.
;; Commentary: 
;;This program speech-enables `cyclebuffer.el' package which provides much faster and more convenient switching between buffers.
;; Code:
(require 'emacspeak-preamble)
(defadvice cyclebuffer-forward (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

(defadvice cyclebuffer-backward (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

(provide 'emacspeak-cyclebuffer)
;;; local variables:
;;; byte-compile-dynamic: t
;;; end: 
