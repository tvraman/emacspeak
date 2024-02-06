<CsoundSynthesizer>
<CsOptions>
-o off.wav
</CsOptions>
<CsInstruments>
sr = 44100
ksmps = 10
nchnls = 2
0dbfs  = 1

instr 1

imaxshake = p4
ifreq     = p5
ifreq1    = p6
ifreq2    = p7
kline	line  1, p3, 0
;low amplitude
adrp dripwater .1, 0.05, 12, .9, imaxshake, ifreq, ifreq1, ifreq2 
  asig      clip      adrp, 2, 0.9                ; avoid drips that drip too loud
aL, aR pan2 asig, kline, 1
     outs aL, aR

endin
</CsInstruments>
<CsScore>
i1 0 0.25 0.5 630 1000 1200 
i1 + 0.05 0.75 630 1000 1200 
e
</CsScore>
</CsoundSynthesizer>
