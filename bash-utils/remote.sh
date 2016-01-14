# Start a remote ssh session with reverse port forwarding
# usage: remote hostname server-script $DTK_PROGRAM  
# starts a local server, then ssh to remote-host,
# and set up reverse port forwarding.

#Examples:
# remote host.example.com  speech-server outloud 
# remote hostname  32-speech-server 32-outloud 

# Configuration:
EMACSPEAK=`pwd`/..
SDIR=$EMACSPEAK/servers 

# Primary server listens on 2222

function remote () {
    REMOTE=$1
    SERVER=$2
    ENGINE=$3
    $SDIR/$SERVER  2222 $SDIR/$ENGINE  &
#notification stream listens on 3333
    (export ALSA_DEFAULT="tts_mono_left"; $SDIR/$SERVER  3333 $SDIR/$ENGINE  &)
beep -f 800 -l 50
 ssh-agent ssh -R 2222:localhost:2222  -R 3333:localhost:3333 $REMOTE
}
