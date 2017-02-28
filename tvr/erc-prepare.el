;(augment-load-path "erc" "erc")  -*- lexical-binding: t; -*-
;(load-library "erc")
(defun erc-w3 ()
  "Join W3C IRC server."
  (interactive)
  (erc  :server "irc.w3.org"
:port 6665
:nick "raman"
 ))

(defun erc-cricket ()
  "Join cricinfo IRC server."
  (interactive)
  (erc  :server "irc.cricket.org"
:port 6667
:nick "raman"))

(defun erc-bubbles ()
  "Join Bubbles"
  (interactive)
  (erc  "bubbles.almaden.ibm.com"
6667
"raman"
"T. V. Raman" t ))

(defun erc-handhelds ()
  "Join Handhelds IRC server."
  (interactive)
  (erc  "irc.openprojects.net"
6667
"raman"
"T. V. Raman" t ))
