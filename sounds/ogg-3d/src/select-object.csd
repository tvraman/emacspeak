<CsoundSynthesizer>
<CsOptions>
-o select-object.wav
</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 1
nchnls = 2
0dbfs = 1   

gasig  init 0   
gidel  = 0.5		;delay time in seconds
                                                             
instr 1

adrp dripwater .1, 0.09, 10, .9, 0.65, 880, 1000, 1200
  ain      clip      adrp, 2, 0.5                ; avoid drips that drip too loud
     outs ain, ain
vincr gasig, ain	;send to global delay
endin

instr 2	

ifeedback = p4	

abuf2	delayr	gidel
adelL 	deltap	.1		;first tap (on left channel)
adelM 	deltap	.9		;second tap (on middle channel)
	delayw	gasig + (adelL * ifeedback)

abuf3	delayr	gidel
kdel	line    1.5, p3, 0	;vary delay time
adelR 	deltap  .15 * kdel	;one pitch changing tap (on the right chn.)
	delayw	gasig + (adelR * ifeedback)
;make a mix of all deayed signals	
	outs	adelL + adelM, adelR + adelM

clear	gasig
endin
</CsInstruments>
<CsScore>

i 1 0 0.1
i 1 + 0.11
i 2 0.0 0.15 1

e
</CsScore>
</CsoundSynthesizer>
