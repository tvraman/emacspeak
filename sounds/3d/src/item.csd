<CsoundSynthesizer>
<CsOptions>
-o item.wav
</CsOptions>
<CsInstruments>
sr = 44100
ksmps = 10
nchnls = 2
0dbfs = 1   
; p4 p5 are start and end frequency
; p6 p7 are envelope attach/decay 
instr 1
kaz	linseg 180, p3, 45		;1 half rotation 
kelev linseg 90, p3, -40
kenv  linen   1, p6, p3, p7 ; amplitude envelope 
kp line p4, p3, p5 ; frequency range
  ain       pluck     kenv,kp, 440, 0, 3
aleft,aright hrtfmove2 ain, kaz,kelev, "hrtf-44100-left.dat","hrtf-44100-right.dat"
outs aleft, aright
endin
</CsInstruments>
<CsScore>
i 1 0 0.25 17000 19000 0 0 
</CsScore>
</CsoundSynthesizer>
