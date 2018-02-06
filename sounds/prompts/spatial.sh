function 3d () {
    mplayer -af ladspa=ZamHeadX2-ladspa:ZamHeadX2:$2:$3:2.5  -ao pcm:file=tmp.wav $1
    sox tmp.wav -b 16 -r 44100  $1
    \rm  tmp.wav
}
