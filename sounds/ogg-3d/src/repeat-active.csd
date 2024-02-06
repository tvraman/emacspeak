<CsoundSynthesizer>
<CsOptions>
 -o repeat-active.wav
</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 10
nchnls = 2

gasrc init 0

instr 1		;a plucked string
  kamp = p4
  kcps = cpspch(p5)
  icps = cpspch(p5)
  a1 pluck kamp, kcps, icps, 0, 1
  gasrc = a1
endin

instr 10	;uses output from instr1 as source
 kaz	linseg 0, p3, 360		;2 full rotation
 aleft,aright hrtfmove2 gasrc, kaz,0, "hrtf-44100-left.dat","hrtf-44100-right.dat"
 outs	aleft, aright
endin
</CsInstruments>
<CsScore>

i1 + 1 15000 9.11
i1 + 1 15000 9.07

; Play Instrument 10 for 2.5 seconds.
i10 0 0.5

</CsScore>
</CsoundSynthesizer>
