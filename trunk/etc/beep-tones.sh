#!/bin/bash
# $Id$
# Replacement for the simple functions of "beep" 
# Uses the sound card instead of the pc-speaker

# Set defaults
LEN=200
FREQ=440
REP=1
PAUSE=40
VOLUME=-10 # in db (0.3)

# Sampling rate of the generated beep
RATE=8000

usage() {
    echo "Usage: " `basename $0` " [ -f frequency ] [-l duration ] [-r repetitions ]"
    echo "           [ -d delay ] [ -a amplitude ]"
}

# Parse command line arguments
while [ $1 ]; do
    case $1 in
		-r)
			REP=$2
			shift
			;;
		-f)
			FREQ=$2
			shift
			;;
		-l)
			LEN=$2
			shift
			;;
		-d)
			PAUSE=$2
			shift
			;;
		-a)
			VOLUME=`echo "scale=3; 20*l($2)/l(10)" | bc -q -l /dev/fd/0`
			shift
			;;
		-h)
			usage
			exit 0
			;;
		*)
			echo "Illegal argument $1"
			usage
			exit 1
			;;
	esac
	shift
done
#echo "freq:$FREQ len:$LEN repeat:$REP delay:$PAUSE volume:$VOLUME"

# Generate the argument for "tones"
if [ $REP -gt "1" ]; then
    FREQ_SPEC=":$LEN  @$VOLUME "
    SPEC_PART="$FREQ 0:$PAUSE "
    for (( i=1 ; $i < $REP ; i += 1 ));  do
	FREQ_SPEC+=$SPEC_PART
    done
    FREQ_SPEC+="$FREQ"
else    
    FREQ_SPEC=":$LEN @$VOLUME $FREQ"
fi

# Decide whether to use alsa or oss
# and play the actual sound
TONES_OPTIONS="-abs -s $RATE -16 $FREQ_SPEC"
if [ -d /proc/asound ]; then
	tones -f -o /dev/fd/1 $TONES_OPTIONS | \
aplay -q -f S16_LE -c 1 -r $RATE  -t raw -
else
tones $TONES_OPTIONS # Tones plays to /dev/dsp by defaultelse
fi
