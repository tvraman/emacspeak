#!/bin/sh
#This script is used to generate panned chimes.
#The only variation among icons is the frequency of the autopan effect.
#Usage pan filename frequency 
function bs2b () {
mplayer -af bs2b -ao pcm:file=tmp.wav $1
/bin/mv tmp.wav $1
}
function pan () {
    mplayer -af ladspa=tap_autopan:tap_autopan:$2:100:0  -ao pcm:file=tmp.wav ../chimes/$1
    sox tmp.wav -b 16 -r 44100  $1
    \rm  tmp.wav
}


function pan_depth () {
    mplayer -af ladspa=tap_autopan:tap_autopan:$2:$3:0  -ao pcm:file=tmp.wav ../chimes/$1
    sox tmp.wav -b 16 -r 44100  $1
    \rm  tmp.wav
}


function swapchan () {
    mv $1 tmp.wav
    sox  tmp.wav $1 remix 2 1
    \rm tmp.wav
}

# pan alarm.wav 3
#pan_depth alert-user.wav 1 30 
# pan ask-question.wav 2
# pan ask-short-question.wav 1
# pan button.wav 5
# pan center.wav 1
# pan_depth close-object.wav 2 50 
# pan complete.wav 2
# pan delete-object.wav 1 
# pan deselect-object.wav 2 
# pan ellipses.wav 2 
# pan fill-object.wav 1 
# pan help.wav 2 
# pan_depth item.wav 1  60
# pan large-movement.wav 1
# pan left.wav 1 
# pan mark-object.wav 1
# pan modified-object.wav 1
# pan n-answer.wav 1
# pan network-down.wav 1
# pan network-up.wav 1
# pan new-mail.wav 1
# pan news.wav 1
# pan no-answer.wav 1
# pan off.wav 1
# pan on.wav 1
# pan_depth open-object.wav 1 60 
# pan paragraph.wav 1
# pan_depth progress.wav 1 50
# pan right.wav 1
# pan save-object.wav 2
# pan scroll.wav 1
# pan search-hit.wav 1
# pan search-miss.wav 1
# pan section.wav 2
# pan select-object.wav 1
# pan shutdown.wav 1
# pan task-done.wav 1
# pan unmodified-object.wav 2
# pan voice-mail.wav 1
# pan warn-user.wav 8
# pan window-resize.wav 1
# pan working.wav 1
# pan yank-object.wav 5
# pan y-answer.wav 5
# pan yes-answer.wav 5

# These files below need to have channels swapped.

#swapchan right.wav
#swapchan search-miss.wav
# swapchan complete.wav
# swapchan deselect-object.wav
# swapchan item.wav
# swapchan unmodified-object.wav 
# swapchan network-down.wav 
# swapchan n-answer.wav
# swapchan no-answer.wav
# swapchan mark-object.wav
# swapchan scroll.wav
