<CsoundSynthesizer>
<CsOptions>
-o save-object.wav
</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 32
nchnls = 2
0dbfs  = 1

instr 1

kamp    init p4
kcpsmin init 4
kcpsmax init 8

kj2  jitter kamp, kcpsmin, kcpsmax
aout pluck 1, 200+kj2, 1000, 0, 1
     outs aout, aout

endin
</CsInstruments>
<CsScore>

i 1 0 0.25 10	

e
</CsScore>
</CsoundSynthesizer>
