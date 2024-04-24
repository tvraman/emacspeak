<CsoundSynthesizer>
<CsOptions>
-o more.ogg
</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 10
nchnls = 2
0dbfs  = 1
; swap channels here 
instr 1

imaxshake = p4
ifreq     = p5
ifreq1    = p6
ifreq2    = p7
kline	linseg 1, 0.4*p3, 0, 0.7*p3, 1 
;low amplitude
adrp dripwater .1, 0.09, 10, .9, imaxshake, ifreq, ifreq1, ifreq2 
asig clip adrp, 2, 0.9	; avoid drips that drip too loud
aL,aR	pan2	asig, kline   ; sent across image
     outs aR, aL

endin
</CsInstruments>
<CsScore>
i1 0 0.12 0.875 1930 2000 2500 
e
</CsScore>
</CsoundSynthesizer>
