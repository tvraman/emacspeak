;;; Wave icons using chimes as a theme  -*- lexical-binding: t; -*-
;;; Icons from theme Chimes with effect autopan applied using mplayer and ladspa

(require 'emacspeak-sounds)
(emacspeak-sounds-define-theme
 (expand-file-name "ogg-chimes/" emacspeak-sounds-directory)
 ".ogg")
