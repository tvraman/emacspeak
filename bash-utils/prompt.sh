# Set up prompts for bash:
SOUNDDIR=${EMACSPEAK_DIR}/sounds/chimes-stereo
SUCCESS=$SOUNDDIR/item.wav
FAIL=$SOUNDDIR/warn-user.wav
function prompt () {
case "$?" in
0) aplay -N -q   ${SUCCESS}  1>&- 2>&-;;
*) aplay  -N -q  ${FAIL} 1>&- 2>&-  ;;
esac
}

export PROMPT_COMMAND=prompt
