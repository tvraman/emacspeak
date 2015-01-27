<CsoundSynthesizer>
<CsOptions>
-o save-object.wav
</CsOptions>
<CsInstruments>sr = 44100
ksmps = 10
nchnls = 2
0dbfs = 1   

instr 1
; p4 is elevation 
kelev = p4 
; p5, p6 are attack and decay 
kenv  linen   .7, p5, p3, p6
kaz	expon 200, p3, 65		
  ain       pluck     kenv, 440, 660, 0, 3
aleft,aright hrtfmove2 ain, kaz, kelev, "hrtf-44100-left.dat","hrtf-44100-right.dat"	
outs aleft, aright

endin
</CsInstruments>
<CsScore>
i 1 0 0.2 25 0.07 0.05
{5 CNT 
  i1 [0.05 * $CNT] 0.25  25 -[5 * $CNT]  0.07 0.07
}
e
</CsScore>
</CsoundSynthesizer>
