<CsoundSynthesizer>
<CsOptions>
-o progress.wav
</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 32
nchnls = 2
0dbfs  = 1

instr 1

imaxshake = p4
ifreq     = p5
ifreq1    = p6
ifreq2    = p7
kline	linseg 1, 0.5*p3, 0, 0.5*p3, 1 
;low amplitude
adrp dripwater .1, 0.09, 10, .9, imaxshake, ifreq, ifreq1, ifreq2 
asig clip adrp, 2, 0.9	; avoid drips that drip too loud
aL,aR	pan2	asig, kline   ; sent across image
     outs aL, aR

endin
</CsInstruments>
<CsScore>
i1 0 0.25 0.75 1630 1000 1200 
e
</CsScore>
</CsoundSynthesizer>
