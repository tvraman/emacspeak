<CsoundSynthesizer>
<CsInstruments>
sr		=		44100
ksmps=10
nchnls	=		2

		instr 2
kelev line 90, p3, -30
kampenv 	expseg 	.0001, .01, p4, .04, .01
asig 	rand 	kampenv
afilt 	reson 	asig, 1000, 100
aout 	balance 	afilt, asig
aleft,aright hrtfmove2 aout, 15,kelev, "hrtf-44100-left.dat","hrtf-44100-right.dat"
		outs 	aleft, aright
		endin
</CsInstruments>
<CsScore>
 i2 0 0.25 8000 25 26 27 28
i2 0.1 0.25 8000 25 26 27 28
</CsScore>
</CsoundSynthesizer>
