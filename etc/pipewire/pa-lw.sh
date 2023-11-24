#!/bin/sh
# A bs2b (binaural for headphones) sink:
pactl load-module module-ladspa-sink sink_name=binaural  sink_master=@DEFAULT_SINK@ plugin=bs2b label=bs2b control=725,4.5
# Use binaural as the default sink:
pactl set-default-sink  binaural 

# device: snoop -- to record and play
pactl load-module module-combine-sink sink_name=snoop  sink_properties=device.description="Record-and-Play"
