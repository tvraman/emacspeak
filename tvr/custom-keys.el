;;; Emacspeak custom key-bindings I use.
;;; These are usually setup via M-x customize-variable
;;; I'm snapshotting it here,
;;; Both as an example of what can be done,
;;; But also as a quick way of bootstrapping oneself
;;; Load this file, then invoke
;;; M-x customize-variable on each variable set here
;;; Customize to taste and save.
;;; C-e x
(setq emacspeak-personal-keys
      '(
        ("3" . emacspeak-wizards-cycle-browser)
        ("<BACKSPACE>" . desktop-clear)
        ("=" . emacspeak-wizards-find-longest-line-in-region)
        ("e" . emacspeak-we-xsl-map)
        ("h" . emacspeak-wizards-how-many-matches)
        ("j" . emacspeak-jabber-popup-roster)
("l" . emacspeak-lynx)
        ("m" . mspools-show)
        ("n" . newsticker-show-news)
        ("o" . emacspeak-wizards-occur-header-lines)
        ("q" . emacspeak-wizards-portfolio-quotes)
        ("r" . jabber-activity-switch-to)
        ("s" . emacspeak-emergency-tts-restart)
        ("t" . emacspeak-speak-telephone-directory)
        ("u" . emacspeak-wizards-units)
        ("v" . emacspeak-wizards-vc-viewer)
        ("x" . emacspeak-xml-shell)
        ("|" . emacspeak-wizards-squeeze-blanks)
))
;;; C-e C-x
(setq emacspeak-personal-ctlx-keys nil)


;;; Hyper


(setq emacspeak-hyper-keys
      '(
              ("SPC" . emacspeak-webspace)
        ("." . hippie-expand)
        ("/" . emacspeak-webspace-google)
        (";" . emms-prefix-command)
        ("<f1>" . help)
        ("a" . ansi-term)
        ("b" . emacspeak-web-prefix)
        ("c" . browse-url-chromium)
        ("e" . gmaps)
        ("f" . emacspeak-webspace-freebase-search)
        ("i" . ido-everywhere)
        ("l" . emacspeak-remote-quick-connect-to-server)
        ("m" . vm)
        ("n" . emacspeak-npr-listing)
        ("p" . emacspeak-wizards-pdf-open)
        ("q" . emacspeak-remote-quick-connect-to-server)
        ("r" . org-capture)
        ("s" . emacspeak-wizards-shell)
        ("t" . twit)
        ("u" . browse-url)
        ("v" . emacspeak-wizards-vi-as-su-file)
        ("w" . w3m-goto-url)
  ("q" . emacspeak-remote-quick-connect-to-server)
        ))
;;; alt keymap
(setq emacspeak-alt-keys
      '(
        ("SPC " . emacspeak-wizards-cycle-to-next-buffer)
        ("T" . emacspeak-wizards-tune-in-radio-search)
        ("a" . emacspeak-feeds-atom-display)
        ("b" . emacspeak-bookshare)
        ("c" . emacspeak-wizards-view-buffers-filtered-by-this-mode)
        ("e" . eww)
        ("l" . eww-open-file)
        ("m" . magit-status)
        ("n" . emacspeak-wizards-next-shell)
        ("o" . emacspeak-feeds-opml-display)
        ("p" . emacspeak-wizards-previous-shell)
        ("r" . emacspeak-feeds-rss-display)
        ("s". emacspeak-wizards-tune-in-radio-search)
        ("t" . emacspeak-wizards-tune-in-radio-browse)
        ("u" . emacspeak-m-player-url)
))



(setq emacspeak-super-keys
      '(
        ("b" . emacspeak-bbc)
        ("e" . elfeed)
        ("f" . emacspeak-feeds-lookup-and-view)
        ("h" . emacspeak-webspace-headlines-browse)
        ("n" . gweb-news-view)
        ("p" . proced)
        ("r" . emacspeak-webspace-feed-reader)
        ("t" . emacspeak-wizards-tramp-open-location)

        ))
