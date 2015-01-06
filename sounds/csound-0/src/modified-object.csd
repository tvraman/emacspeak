<CsoundSynthesizer>
<CsOptions>
-o modified-object.wav
</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 32
nchnls = 2
0dbfs = 1   

;gasig  init 0   
;gidel  = 1		;delay time in seconds
                                                             
instr 1
kaz	expon 225, p3, 45		
  ain       pluck     .7, 440, 1000, 0, 1
aleft,aright hrtfmove2 ain, kaz,30, "hrtf-44100-left.dat","hrtf-44100-right.dat"	
     outs aleft, aright

endin

                                                             

</CsInstruments>
<CsScore>
i 1 0 .2
  i 1 + 0.15
e
</CsScore>
</CsoundSynthesizer>
