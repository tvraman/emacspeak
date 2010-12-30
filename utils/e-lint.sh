#!/bin/sh
#$Iid:$
echo "$@" | \
emacs -batch -q -l advice.el -l cl-macs.el -l emacspeak-load-path.el -f elint-file 
