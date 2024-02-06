<CsoundSynthesizer>
<C  sOptions>
-o search-miss.wav
</CsOptions>
<CsInstruments>
sr		=		44100
;
ksmps=10
nchnls	=		2

		instr 3

kelev line 90, p3, -30
kfreqenv 	expseg 	50, .01, 200, .08, 50
kampenv 	expseg 	.0001, .01, p4, .08, .01
asig 	rand 	kampenv
afilt 	reson 	asig, kfreqenv, kfreqenv/8
aout 	balance 	afilt, asig
aleft,aright hrtfmove2 3*aout, 0,kelev, "hrtf-44100-left.dat","hrtf-44100-right.dat"
		outs		aleft,aright
		endin

		instr 4
kelev line 90, p3, -30 
kampenv4 	linseg 	0, .001, 1, p3-.021, 1, .02, 0 
kptchenv 	linseg 	100, .01, 300, .2, 200, .01, 200 
asig 	pluck 	p4, kptchenv, 50, 2, 4, .8, 3
aout 	=		kampenv4*asig
aleft,aright hrtfmove2 3*aout, 0,kelev, "hrtf-44100-left.dat","hrtf-44100-right.dat"
		outs 	aleft, aright
 

		endin
 

		instr 5
 

; SORTA COOL KNOCK SWEEP DRUM

kfreqenv41 expseg 	50, .01, 200, .08, 50

kfreqenv42 linseg 	150, .01, 1000, .08, 250 

kampenv4	linseg 	0, .01, p4, .08, 0, .01, 0 

asig 	rand 	kampenv4

afilt1 	reson 	asig, kfreqenv41, kfreqenv41/8 

afilt2 	reson 	asig, kfreqenv42, kfreqenv42/4 

aout1	balance 	afilt1, asig

aout2	balance 	afilt2, asig

		outs 	(aout1+aout2)/2, (aout1+aout2)/2
 

		endin
 

		instr 6
 

; FM METAL BOINK DRUM

kampenv61 expseg 	.01, .01, p4, .2, p4/100, .1, .001 

kampenv62 linseg 	1, .1, 10, .1, .5, .01, 1 
 

asig 	foscil 	kampenv61, 30, 1, 6.726, kampenv62, 1

		outs 	asig, asig
 

		endin

</CsInstruments>
<CsScore>
f2 0 1024 7 1 1024 1
 

 i3 0 0.15 12000 
 i4 + 0.15 12000 
</CsScore>
</CsoundSynthesizer>
