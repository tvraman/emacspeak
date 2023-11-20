#!/bin/sh
# A bs2b (binaural for headphones) sink:
pactl load-module module-ladspa-sink sink_name=binaural  sink_master=@DEFAULT_SINK@ plugin=bs2b label=bs2b control=725,4.5
# Use binaural as the default sink:
pactl #set-default-sink  binaural 
# A tts_right and tts_left device
# See https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/User/Modules/#module-remap-sink
# Emacspeak will use the first as the notification device.
pactl load-module module-remap-sink sink_name=tts_right  master=binaural channels=2 master_channel_map=front-right,front-right, channel_map=front-left,front-right remix=no
pactl load-module module-remap-sink sink_name=tts_left  master=binaural channels=2 master_channel_map=front-left,front-left, channel_map=front-left,front-right remix=no

# device: snoop -- to record and play
pactl load-module module-combine-sink sink_name=snoop  sink_properties=device.description="Record-and-Play"
