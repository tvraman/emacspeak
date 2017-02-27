(augment-load-path "erc" "erc")
(load-library "erc")
(defun erc-w3 ()
  "Join W3C IRC server."
  (interactive)
  (erc  "irc.w3.org"
6665
"tvraman"
"T. V. Raman" t nil))


(defun erc-mm ()
  "Join MM IRC   server."
  (interactive)
  (erc  "bubbles.almaden.ibm.com"
6667
"tvraman"
"T. V. Raman" t "retsa"))

(defun erc-handhelds ()
  "Join Handhelds IRC server."
  (interactive)
  (erc  "irc.openprojects.net"
6667
"tvraman"
"T. V. Raman" t ))

