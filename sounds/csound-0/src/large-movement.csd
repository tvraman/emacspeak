<CsoundSynthesizer>
<CsOptions>
-o large-movement.wav
</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 32
nchnls = 2
0dbfs = 1

instr 1 

kcps    expon p5, p3, p4
asig	oscil3 0.2, kcps, 1
krvt =  4.5
ilpt =  0.5
aleft	combinv asig, krvt, ilpt
	outs   aleft, asig

endin

</CsInstruments>
<CsScore>
f1 0 4096 10 1
i 1 0 .25  60 1000
;i 1 + .2 25 2000
e

</CsScore>
</CsoundSynthesizer>

es
