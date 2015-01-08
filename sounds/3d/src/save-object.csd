<CsoundSynthesizer>
<CsOptions>
-o modified-object.wav
</CsOptions>
<CsInstruments>sr = 44100
ksmps = 10
nchnls = 2
0dbfs = 1   

instr 1
; p4 is elevation 
kelev = p4 
kaz	expon 200, p3, 65		
  ain       pluck     .7, 440, 660, 0, 3
aleft,aright hrtfmove2 ain, kaz, kelev, "hrtf-44100-left.dat","hrtf-44100-right.dat"	
     outs aleft, aright

endin
</CsInstruments>
<CsScore>i 1 0 0.2 -40
{5 CNT 
  i1 [0.05 * $CNT] 0.25  20 -[5 * $CNT]
}
e
</CsScore>
</CsoundSynthesizer>
