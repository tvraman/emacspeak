<CsoundSynthesizer>
<C  sOptions>
  -oellipses.wav
</CsOptions>
<CsInstruments>
sr		=		44100
ksmps=10
nchnls	=		2

		instr 2
kelev line -90, p3, 90
kampenv 	expseg 	.0001, .01, p4, .04, p3, 0.01,  .0001
asig 	rand 	kampenv
afilt 	reson 	asig, 780, 1
aout 	balance 	afilt, asig
aleft,aright hrtfmove2 aout, 0,kelev, "hrtf-44100-left.dat","hrtf-44100-right.dat"
		outs 	5*aleft, 5*aright
		endin
</CsInstruments>
<CsScore>

 i2 0 0.08 8000 
i2 0.02 0.06 5000 

i2 0.08 0.06 7000 
i2 0.1 0.04 4000 

i2 0.14 0.04 5000 
i2 0.16 0.02 2000 

i2 0.18 0.02 4000 

i2 0.20 0.04 2000 
i2 0.22 0.01 1000 

i2 0.20 0.08 1000
i2 0.22 0.08 500 
</CsScore>
</CsoundSynthesizer>
