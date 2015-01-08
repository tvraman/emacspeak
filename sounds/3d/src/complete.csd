<CsoundSynthesizer>
<CsOptions>
-o complete.wav
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
kaz	expon 200, p3, 70		
;low amplitude
adrp dripwater .1, 0.09, 10, .9, imaxshake, ifreq, ifreq1, ifreq2 
  asig      clip      adrp, 2, 0.9                ; avoid drips that drip too loud
aleft,aright hrtfmove2 asig, kaz,-20, "hrtf-44100-left.dat","hrtf-44100-right.dat"
     outs aleft, aright

endin
</CsInstruments>
<CsScore>

i1  0 0.15 0.95 1630 1000 1200 
i1  + 0.15 0.05 1630 1000 1200 
} 

e
</CsScore>
</CsoundSynthesizer>
