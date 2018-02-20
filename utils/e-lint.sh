#!/bin/sh
#$Iid:$
LOAD="-l advice.el -l cl-macs.el -l cl-lib.el -l emacspeak-load-path.el -l cl.elc  \
 -l emacspeak-loaddefs.el -L ./g-client"
echo "$@" | \
emacs -batch -q -f package-initialize $LOAD -f elint-file 
