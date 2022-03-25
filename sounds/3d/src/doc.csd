<CsoundSynthesizer>
<CsOptions>
-o doc.wav
</CsOptions>
<CsInstruments>
sr = 44100
ksmps = 10
nchnls = 2
0dbfs = 1   

; p4 is note
instr 1
kaz	linseg -90, p3, 180
kelev linseg 90, p3, 0
ip1 = cpspch(p4)
  ain       pluck     .6,ip1, ip1, 0, 1
aleft,aright hrtfmove2 ain, kaz,kelev, "hrtf-44100-left.dat","hrtf-44100-right.dat"
outs aleft, aright
endin
</CsInstruments>
<CsScore>
i 1 0 0.125 8.11 
</CsScore>
</CsoundSynthesizer>
