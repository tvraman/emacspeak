<CsoundSynthesizer>
<CsOptions>
-o yank-object.wav
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

;low amplitude
adrp dripwater .1, 0.09, 10, .9, imaxshake, ifreq, ifreq1, ifreq2 
asig clip adrp, 2, 0.9	; avoid drips that drip too loud
     outs asig, asig

endin
</CsInstruments>
<CsScore>


t 0 120 ;
{5 CNT 
i1 [0.05 * $CNT] 0.5 .5 800 1600 2000 
} 

e
</CsScore>
</CsoundSynthesizer>
