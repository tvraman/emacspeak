<CsoundSynthesizer>
<CsOptions>
-o search-hit.wav
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
kaz linseg 3600, p3, 0
kelev linseg -40, p3, 80
;low amplitude
adrp dripwater .1, 0.09, 15, .8, imaxshake, ifreq, ifreq1, ifreq2 
asig clip adrp, 2, 0.9	; avoid drips that drip too loud
     aleft,aright hrtfmove2 asig, kaz,kelev, "hrtf-44100-left.dat","hrtf-44100-right.dat"
     outs aleft, aright

endin
</CsInstruments>
<CsScore>

i1 0 0.15 0.1 1600 2000 2400
i1 0.1 0.2 0.8 1600 2000 2400
</CsScore>
</CsoundSynthesizer>
