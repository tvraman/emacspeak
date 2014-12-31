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
ksmps = 32  
nchnls = 2
0dbfs  = 1

instr 1

kamp = p4
kfreq = 880
kc1 = p5
kc2 = p6
kvdepth = 0.5
kvrate = 8

  asig      fmbell   kamp, kfreq, kc1, kc2, kvdepth, kvrate
     outs asig, asig
endin
</CsInstruments>
<CsScore>
; sine wave.
f 1 0 32768 10 1

t 0 120
i 1 0 .25 .1  1 1 
e
</CsScore>
</CsoundSynthesizer>
