#!/bin/sh
#$Iid:$
echo "$@" | \
emacs -batch -q -l emacspeak-load-path.el -f elint-file
