<CsoundSynthesizer>
<CsOptions>
-o scroll.wav 
</CsOptions>
<CsInstruments>
sr = 44100
ksmps = 10
nchnls = 2
0dbfs = 1

instr 1 
kaz1 	linseg -90, p3, 270
kaz2 	linseg 45, p3, 225
; generate pink noise
an1  pinkish 1
an2  pinkish  0.15
al1, ar1 hrtfmove2 an1, kaz1, 0, "hrtf-44100-left.dat","hrtf-44100-right.dat"	
al2, ar2 hrtfmove2 an2, kaz2, 0, "hrtf-44100-left.dat","hrtf-44100-right.dat"	
; write audio out
outs    al1+al2, ar1+ar2
endin

</CsInstruments>
<CsScore>
i 1 0 .25
i 1 0.15 0.35
e
</CsScore>
</CsoundSynthesizer>

