#!/bin/sh
#$Iid:$
LOAD="-l advice.el -l cl-macs.el -l emacspeak-load-path.el -l emacspeak-loaddefs.el"
echo "$@" | \
emacs -batch -q $LOAD -f elint-file 
