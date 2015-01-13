<CsoundSynthesizer>
<CsOptions>
-o scroll.wav 
</CsOptions>
<CsInstruments>
sr = 44100
ksmps = 10
nchnls = 8
0dbfs = 1

instr 1 
kaz	linseg -90, p3, 270
; generate pink noise
anoise pinkish 1
aleft,aright hrtfmove2 anoise, kaz, 0, "hrtf-44100-left.dat","hrtf-44100-right.dat"	
; write audio out
outs  aleft, aright
endin

</CsInstruments>
<CsScore>
t 0 120 
i 1 0 .5
e
</CsScore>
</CsoundSynthesizer>

