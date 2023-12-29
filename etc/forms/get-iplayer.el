;;; get-iplayer.el --- Speech friendly interface to get_iplayer cache   -*- lexical-binding: t; -*-
;;; Description:  Use forms-mode to look at iplayer cache

(setq forms-read-only t)
(setq forms-file (expand-file-name "~/.get_iplayer/radio.cache"))
(setq forms-number-of-fields 16)
(setq forms-field-sep "|")
(setq forms-format-list
      (list
       "C-c p: Play, C-c d: Download\n"
       "Id: "  1 "\t"
       "Name: " 3 "\n"
       "Episode: " 4 "\t"
       "Description: "  12 "\n"))

(defun ems--get-iplayer-play ()
  "Play using mpv"
  (interactive)
  (emacspeak-empv-play-url (nth  13 forms-fields)))

(defun ems--get-iplayer-download ()
  (interactive)
  (let ((default-directory (expand-file-name "~/Downloads")))
    (async-shell-command
     (format "youtube-dl %s " (nth  13 forms-fields)))))

(require 'forms)
(when (and forms-mode-map (keymapp forms-mode-map))
  (define-key forms-mode-map "d" 'ems--get-iplayer-download)
  (define-key forms-mode-map "p" 'ems--get-iplayer-play))
