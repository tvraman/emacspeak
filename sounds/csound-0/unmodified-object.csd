<CsoundSynthesizer>
<CsOptions>
-o unmodified-object.wav
</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 32
nchnls = 2
0dbfs = 1   

gasig  init 0   
gidel  = 1		;delay time in seconds
                                                             
instr 1
	
ain  pluck .7, 880, 1000, 0, 1
     outs ain, ain

vincr gasig, ain	;send to global delay
endin
</CsInstruments>
<CsScore>
i 1 0 .25
  



e
</CsScore>
</CsoundSynthesizer>
