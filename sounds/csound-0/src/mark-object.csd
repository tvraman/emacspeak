<CsoundSynthesizer>
<CsOptions>
-o mark-object.wav
</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 32
nchnls = 2
0dbfs = 1   

gasig  init 0   
gidel  = 1		;delay time in seconds
     

gasrc init 0                                                        

instr 10	;uses output from instr1 as source

 kaz	linseg 0, p3, 360		;2 full rotations

 aleft,aright hrtfmove gasrc, kaz,0, "hrtf-44100-left.dat","hrtf-44100-right.dat"

 outs	aleft, aright
  
endin

instr 1
	
  ain       pluck     .7, 440, 1000, 0, 1

     gasrc = ain

vincr gasig, ain	;send to global delay
endin


</CsInstruments>
<CsScore>

i 1 0 .25
i10 0 0.25
e
</CsScore>
</CsoundSynthesizer>
