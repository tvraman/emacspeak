<CsoundSynthesizer>
<CsInstruments>
;rene.nyffenegger@adp-gmbh.ch

sr    = 44100
kr    = 441
nchnls=2
instr 1
  iamp     = p4
  
  kamp1 expon 1.25, .03,  .0001
  kamp2 expseg .001,.005,1, .35, .001
  
  anoise rand 1

  anoise1 = anoise*kamp1
  anoise2 = anoise*kamp2
  
  adel1     = anoise1
  adel2 delay anoise1, .01
  adel3 delay anoise1, .02

  adel4 delay anoise2, .03
  
  abp1 resonz adel1,  400,  1100
  abp2 resonz adel2,  600,  1100
  abp3 resonz adel3,  800,  1100
  abp4 resonz adel4,  1100, 1100
  
  out iamp*8*(abp1+abp3), iamp*8*(abp2+abp4)
endin
</CsInstruments>
<CsScore>

;i1 0 1 2000
;i1 1 1 1000
i1 0 1  500
i1 0 1  250
</CsScore>
</CsoundSynthesizer>
