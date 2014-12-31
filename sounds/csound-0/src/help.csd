<CsoundSynthesizer>
<CsOptions>
 -o help.wav
</CsOptions>
<CsInstruments>

sr = 44100
kr = 4410
ksmps = 10
nchnls = 2

; Instrument #1: An example of sleighbells.
instr 1
   a1 sleighbells 15000, 0.5, 0, 0.1
a2 sleighbells 12000, 0.1, 0, 0.1
  outs a1, a2
endin


</CsInstruments>
<CsScore>
t 0 180
i 1 0 0.12
i 1 + 0.11
i 1 + 0.1
i 1 + 0.09
i 1 + 0.08
e


</CsScore>
</CsoundSynthesizer>
