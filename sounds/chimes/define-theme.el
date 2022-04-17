;;; Wave icons using chimes as a theme  -*- lexical-binding: t; -*- 

(require 'emacspeak-sounds)
(emacspeak-sounds-define-theme
 (expand-file-name "chimes/" emacspeak-sounds-directory)
 ".wav")
