<CsoundSynthesizer>
<CsInstruments>
sr		=		44100
kr		=		441
ksmps	=		100
nchnls	=		2
; scroll.csd: Extracted from Electric Drum Kit
; DRUM MACHINE

		instr 1
kstep 	init 	0
 
; SEQUENCER SECTION

;------------------------------------------------------------------------- 

loop1:

; READ ALL OF THE TABLE VALUES.

kdur 	table 	kstep, p5
kdrnum 	table 	kstep, p6
kleft 	table 	kstep, p7
kright 	table 	kstep, p8
kdur1 	=		kdur/8				; MAKE THE STEP SMALLER.

						
; ALL OF THE ENVELOPES WANT TO BE OUTSIDE OF THE IF FOR SOME REASON. 

kampenv1 	linseg 	0, .01, p4/2, .04, 0, .01, 0 

kampenv2 	expseg 	.0001, .01, p4, .04, .01 

kfreqenv 	expseg 	50, .01, 200, .08, 50

kampenv3 	expseg 	.0001, .01, p4*2, .08, .01 

kampenv4 	linseg 	0, .001, 1, i(kdur1)-.021, 1, .02, 0 

kfreqenv51 expseg 	50, .01, 200, .08, 50

kfreqenv52 linseg 	150, .01, 1000, .08, 250, .01, 250 

kampenv5 	linseg 	0, .01, p4, .08, 0, .01, 0 

kptchenv 	linseg 	100, .01, 300, .2, 200, .01, 200 
 

kampenv61	expseg 	.01, .01, p4, .2, p4/100, .1, .001 

kampenv62 linseg 	1, .1, 10, .1, .5, .01, 1 
 

; SOME OF THE SIGNAL GENERATORS MUST BE OUTSIDE OF THE IF'S 

asig4	pluck 	p4/2, kptchenv, 50, 2, 4, .8, 3

asig5	foscil 	kampenv61, 30, 1, 6.726, kampenv62, 1
 

; SWITCH BETWEEN THE DIFFERENT DRUMS

		if		(kdrnum != 0) goto next1

; HIHAT

aout 	rand 	kampenv1

		goto 	endswitch
 

next1:

		if		(kdrnum != 1) goto next2

; DUMB DRUM

asig 	rand	 	kampenv2

afilt 	reson 	asig, 1000, 100

aout 	balance 	afilt, asig

		goto 	endswitch
 

next2:

		if		(kdrnum != 2) goto next3

; DUMB BASS DRUM

asig 	rand 	kampenv3

afilt	 reson 	asig, kfreqenv, kfreqenv/8

aout 	balance 	afilt, asig

		goto 	endswitch
 

next3:

		if 		(kdrnum != 3) goto next4

; KS SNARE

aout 	=		kampenv4*asig4

		goto 	endswitch
 

next4:

		if		(kdrnum != 4) goto next5

; SORTA COOL KNOCK SWEEP DRUM

asig 	rand 	kampenv5

afilt1 	reson 	asig, kfreqenv51, kfreqenv51/8 

afilt2 	reson 	asig, kfreqenv52, kfreqenv52/4 

aout1 	balance 	afilt1, asig

aout2 	balance 	afilt2, asig

aout 	=		(aout1+aout2)/2

		goto 	endswitch
 

next5:

		if		(kdrnum != 5) goto endswitch

; FM METAL BOINK DRUM

aout 	= 		asig5
 
 

endswitch:

; WHEN THE TIME RUNS OUT GO TO THE NEXT STEP 

; OF THE SEQUENCE AND REINITIALIZE THE ENVELOPES.

		timout 	0, i(kdur1), cont1

kstep 	= 		frac((kstep + 1)/8)*8

		reinit 	loop1
 

cont1:
 

		outs 	kleft*10*aout, 10*kright*aout
 

		endin
 

; FIGURE OUT YOUR DRUMS DOWN HERE. THEN MOVE ENVELOPES INTO THE ENVELOPE 

; SECTION AND THE REST INTO THE SWITCH SECTION OF THE DRUM MACHINE. 
 

		instr 2
 

; DUMB DRUM 1

kampenv 	expseg 	.0001, .01, p4, .04, .01

asig 	rand 	kampenv

afilt 	reson 	asig, 1000, 100

aout 	balance 	afilt, asig

		outs 	aout, aout
 

		endin
 

		instr 3
 

; DUMB BASS DRUM

kfreqenv 	expseg 	50, .01, 200, .08, 50

kampenv 	expseg 	.0001, .01, p4, .08, .01

asig 	rand 	kampenv

afilt 	reson 	asig, kfreqenv, kfreqenv/8

aout 	balance 	afilt, asig

		outs		aout, aout
 

		endin
 

		instr 4
 

; KS SNARE DRUM

kampenv4 	linseg 	0, .001, 1, p3-.021, 1, .02, 0 

kptchenv 	linseg 	100, .01, 300, .2, 200, .01, 200 

asig 	pluck 	p4, kptchenv, 50, 2, 4, .8, 3

aout 	=		kampenv4*asig

		outs 	aout, aout
 

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
; Score

f1 0 8192 10 1

f2 0 1024 7 1 1024 1
 

; Duration

f21 0 8 -2 1	1	1	1	1	1	1	1

f25 0 8 -2 4	1	1	2	4	1	1	2

f29 0 8 -2 4	1	1	1	1	4	2	2
 

; Drums : 0=HiHat, 1=Tap, 2=Bass, 3=KS Snare, 4=Sweep, 5=FMBoink 

f22 0 8 -2 0	1	0	1	2	1	0	1

f26 0 8 -2 4	3	3	2	4	2	3	4

f30 0 8 -2 4	2	1	5	4	5	5	4
 

; Panning

f23 0 8 -2 1 0	1	0	1	0	1	1

f24 0 8 -2 0 1	0	1	1	1	1	0
 

f27 0 8 -2 1 0	1	1	0	1	1	0

f28 0 8 -2 0 1	0	0	1	1	0	1
 

f31 0 8 -2 0 0	0	0	0	1	1	0

f32 0 8 -2 1 1	1	1	1	0	1	1
 

; Sta Dur Amp	Tables

;	Dur Drum PanL PanR

i1 0 .25 2000 25 26 26 25
i1 + .25 2000 25 26 25 26
 ;i2 0 0.25 8000 25 26 27 28

; i1 0 6 8000 29 30 31 32

; s

; f0 2

; s

; i2 .2 .2 20000

; i2 + .2 20000

; s

; f0 2

; s

; i6 0 .1 20000

; i6 + .2 20000

; s

; f0 2

; s

; i4 0 1 10000

</CsScore>
</CsoundSynthesizer>
