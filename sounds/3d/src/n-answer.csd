<CsoundSynthesizer>
<CsInstruments>
sr		=		44100
;
ksmps=10
nchnls	=		2

		instr 2
kampenv 	expseg 	.0001, .01, p4, .04, .01
asig 	rand 	kampenv
afilt 	reson 	asig, 1000, 100
aout 	balance 	afilt, asig
		outs 	aout, aout
		endin
</CsInstruments>
<CsScore>
 i2 0 0.25 8000 25 26 27 28
i2 0.1 0.25 8000 25 26 27 28
</CsScore>
</CsoundSynthesizer>
