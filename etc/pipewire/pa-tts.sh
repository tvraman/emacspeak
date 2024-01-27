#!/bin/sh
#Create our tts_mono_left and tts_mono_right and avoid .asoundrc




# A tts_mono_right and tts_mono_left device
# See https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/User/Modules/#module-remap-sink
# Emacspeak will use the first as the notification device.
pactl load-module module-remap-sink sink_name=tts_mono_right  master=binaural channels=2 master_channel_map=front-right,front-right, channel_map=front-left,front-right remix=no
pactl load-module module-remap-sink sink_name=tts_mono_left  master=binaural channels=2 master_channel_map=front-left,front-left, channel_map=front-left,front-right remix=no

# Above can likely be done with pw-link:
#https://www.thegeekdiary.com/pw-link-command-examples-in-linux/
# device: snoop -- to record and play
# pactl load-module module-combine-sink sink_name=snoop  sink_properties=device.description="Record-and-Play"

# to undo, use pactl unload-module <module name>
