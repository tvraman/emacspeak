<CsoundSynthesizer>
<CsOptions>
 -odac 
</CsOptions>
<CsInstruments>

sr = 44100
kr = 4410
ksmps = 10
nchnls = 2

; Instrument #1: An example of sleighbells.
instr 1
   a1 sleighbells 20000, 0.1
a2 sleighbells 20000, 0.1
  outs a1, a2
endin


</CsInstruments>
<CsScore>
t 0 120
i 1 0.00 0.25
i 1 + 0.2
i 1 + 0.15
i 1 + 0.1
i 1 + 0.1
e


</CsScore>
</CsoundSynthesizer>
