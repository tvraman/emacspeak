#!/bin/sh
# Description: Convert media streams to mp3
#
function ram2mp3 () { 
    name=`basename $1 .ram`;
    mplayer -quiet -vc null -vo null  -ao pcm:file=$name.wav -playlist $1;
    lame -h  --quiet $name.wav $name.mp3;
    \rm $name.wav
} 
ram2mp3 "$@"
