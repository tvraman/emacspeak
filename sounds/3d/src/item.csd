<CsoundSynthesizer>
<CsOptions>
; Select audio/midi flags here according to platform
;-odac     ;;;realtime audio out
;-iadc    ;;;uncomment -iadc if realtime audio input is needed too
; For Non-realtime ouput leave only the line below:
 -o fmbell.wav -W ;;; for file output any platform
</CsOptions>
<CsInstruments>
sr = 44100
ksmps = 10  
nchnls = 2
0dbfs  = 1

instr 1

kamp = p4
kenv linen kamp, 0.05, p3, 0.05
kfreq = 880
kc1 = p5
kc2 = p6
kvdepth = 0.5
kvrate = 8
kaz	expon 200, p3, 70		
  asig      fmbell   kenv, kfreq, kc1, kc2, kvdepth, kvrate
aleft,aright hrtfmove2 asig, kaz,-20, "hrtf-44100-left.dat","hrtf-44100-right.dat"
     outs aleft, aright
endin
</CsInstruments>
<CsScore>
; sine wave.
f 1 0 32768 10 1

i 1 0 .25 .15  1 8
e
</CsScore>
</CsoundSynthesizer>
