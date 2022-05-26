#!/bin/sh
beep -f 600 -l 100 -d 20 -n -f 800 -l 200 &
/usr/local/bin/emsay  "$@"
read 
