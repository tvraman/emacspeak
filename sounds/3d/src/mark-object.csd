<CsoundSynthesizer>
<CsOptions>
-o mark-object.wav
</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 10
nchnls = 2
0dbfs = 1   

gasig  init 0   
gidel  = 1		;delay time in seconds
     

gasrc init 0                                                        

instr 10	;uses output from instr1 as source

 kaz	expon 225, p3, 45		;1 half rotation 

 aleft,aright hrtfmove2 gasrc, kaz,-20, "hrtf-44100-left.dat","hrtf-44100-right.dat"

 outs	aleft, aright
  
endin

instr 1
kenv linen .7, 0.05, p3, 0.05
  ain       pluck     kenv, 440, 1000, 0, 1

     gasrc = ain

vincr gasig, ain	;send to global delay
endin


</CsInstruments>
<CsScore>

i 1 0 0.5
i10 0 0.5
e
</CsScore>
</CsoundSynthesizer>
