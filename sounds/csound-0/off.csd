<CsoundSynthesizer>
<CsOptions>
-o off.wav
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

;low amplitude
adrp dripwater .1, 0.05, 12, .9, imaxshake, ifreq, ifreq1, ifreq2 
asig clip adrp, 2, 0.9	; avoid drips that drip too loud
     outs asig, asig

endin
</CsInstruments>
<CsScore>



i1 0 0.25 0.5 630 1000 1200 
i1 + 0.25 0.5 630 1000 1200 


e
</CsScore>
</CsoundSynthesizer>
