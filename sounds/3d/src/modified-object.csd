<CsoundSynthesizer>
<CsOptions>
-o modified-object.wav
</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 32
nchnls = 2
0dbfs = 1   



                                                             
instr 1
kaz	expon 225, p3, 45		
  ain       pluck     .7, 660, 440, 0, 3
aleft,aright hrtfmove2 ain, kaz, -30, "hrtf-44100-left.dat","hrtf-44100-right.dat"	
     outs aleft, aright

endin

                                                             

</CsInstruments>
<CsScore>
i 1 0 .25
  i 1 0.05 0.25
e
</CsScore>
</CsoundSynthesizer>
