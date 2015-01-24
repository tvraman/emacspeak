<CsoundSynthesizer>
<CsOptions>
  -o ellipses.wav
</CsOptions>
<CsInstruments>
sr		=		44100
;
ksmps=10
nchnls	=		2
		instr 2
kelev expon 90, p3, 0.001
kampenv 	expseg 	.0001, .01, p4, .04, .01
asig 	rand 	kampenv
afilt 	reson 	asig, 1000, 100
aout 	balance 	afilt, asig
aleft,aright hrtfmove2 8*aout, 315,kelev, "hrtf-44100-left.dat","hrtf-44100-right.dat"
		outs 	aleft, aright
		endin

</CsInstruments>
<CsScore>
t 0 150 ; tempo

 i2 0 0.2 4800 25 26 27 28
i2 + 0.175 4600 25 26 27 28
i2 + 0.125 4500 25 26 27 28
i2 + 0.1 4000 25 26 27 28
i2 + 0.09 3000 
i2 + 0.06 2000 
</CsScore>
</CsoundSynthesizer>
