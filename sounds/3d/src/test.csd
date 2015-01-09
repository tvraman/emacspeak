<CsoundSynthesizer>
<CsOptions>
-odac -L stdin 
</CsOptions>
<CsInstruments>sr = 44100
ksmps = 10
nchnls = 2
0dbfs = 1   

instr 1
kenv  linen   .7, p6, p3, p7
  ain       pluck     kenv,p4, p5, 0, 3
outs ain, ain 
endin
</CsInstruments>
</CsoundSynthesizer>
