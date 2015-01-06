<CsoundSynthesizer>
<CsOptions>
-o save-object.wav
</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 32
nchnls = 2
0dbfs  = 1

giSine ftgen 0, 0, 2^10, 10, 1

instr 2	; scaling to duration

kcps = cpspch(p4)
kenv linseg 0, p3*0.25,  1,  p3*0.75, 0
asig poscil kenv, kcps, giSine
     outs asig, asig

endin

</CsInstruments>
<CsScore>
i 2 0 0.3   7.00	; scales to duration

e
</CsScore>
</CsoundSynthesizer>
