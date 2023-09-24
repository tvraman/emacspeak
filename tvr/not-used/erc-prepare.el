(defun erc-w3 ()
  "Join W3C IRC server."
  (interactive)
  (erc  :server "irc.w3.org" :port 6665 :nick "raman"))

(defun erc-cricket ()
  "Join cricinfo IRC server."
  (interactive)
  (erc  :server "us.starlink.net       " :port 6667 :nick "raman"))
