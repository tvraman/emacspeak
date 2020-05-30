;;; get-iplayer.el --- Speech friendly interface to get_iplayer cache   -*- lexical-binding: t; -*-
;;; Description:  Use forms-mode to look at iplayer cache

(setq forms-read-only t)
(setq forms-file (expand-file-name "~/.get_iplayer/radio.cache"))
(setq forms-number-of-fields 16)
(setq forms-field-sep "|")
(setq forms-format-list
      (list
       "Id: "  1
       "Name: " 3
       "Episode: " 4
       "Description: "  12 "\n"))
