#!/bin/bash
# Author: <github.com/tvraman> License: GPL
# Usage: ./bootstrap [version] 
# On a well-installed Linux system,
# This should let a user bootstrap into a talking Emacs.
# Not intended for daily use.

#  Prerequisites: espeak, libespeak libespeak-dev
# libespeak-ng-libespeak-dev libespeak-ng-dev espeak-ng-espeak
# Downloads,  builds and runs specified version.
latest=56.0
v=$1

if [ ! -n "$1" ]
then
  v=$latest
fi  

u="https://github.com/tvraman/emacspeak/releases/download/${v}/emacspeak-${v}.tar.bz2"

# Download and unpack if needed:
if [ ! -d "emacspeak-$v" ]
then
  wget $u;
  tar xfj "emacspeak-${v}.tar.bz2"
fi

# Build it:
cd "emacspeak-${v}"
make config && make
(cd servers/native-espeak &&  make )
#Run out of this directory.
# Default to using  espeak unless DTK_PROGRAM is set.
#
export DTK_PROGRAM="espeak"
emacs -q -l ./lisp/emacspeak-setup.el -l $HOME/.emacs
