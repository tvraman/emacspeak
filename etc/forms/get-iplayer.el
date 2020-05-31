;;; get-iplayer.el --- Speech friendly interface to get_iplayer cache   -*- lexical-binding: t; -*-
;;; Description:  Use forms-mode to look at iplayer cache

(setq forms-read-only t)
(setq forms-file (expand-file-name "~/.get_iplayer/radio.cache"))
(setq forms-number-of-fields 16)
(setq forms-field-sep "|")
(setq forms-format-list
      (list
       "Id: "  1 "\t"
       "Name: " 3 "\n"
       "Episode: " 4 "\t"
       "Description: "  12 "\n"))
