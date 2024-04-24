<CsoundSynthesizer>
<CsOptions>
-o warn-user.ogg
</CsOptions>
<CsInstruments>
sr    =   44100
ksmps=10
nchnls  =   2
instr 1
kstep   init  0
loop1:
kdur  table   kstep, p5
kdrnum  table   kstep, p6
kleft   table   kstep, p7
kright  table   kstep, p8
kdur1   =   kdur/8        ; MAKE THE STEP SMALLER.
kampenv1  linseg  0, .01, p4/2, .04, 0, .01, 0
kampenv2  expseg  .0001, .01, p4, .04, .01
kfreqenv  expseg  50, .01, 200, .08, 50
kampenv3  expseg  .0001, .01, p4*2, .08, .01
kampenv4  linseg  0, .001, 1, i(kdur1)-.021, 1, .02, 0
kfreqenv51 expseg   50, .01, 200, .08, 50
kfreqenv52 linseg   150, .01, 1000, .08, 250, .01, 250
kampenv5  linseg  0, .01, p4, .08, 0, .01, 0
kptchenv  linseg  100, .01, 300, .2, 200, .01, 200
kampenv61 expseg  .01, .01, p4, .2, p4/100, .1, .001
kampenv62 linseg  1, .1, 10, .1, .5, .01, 1
asig4 pluck   p4/2, kptchenv, 50, 2, 4, .8, 3
asig5 foscil  kampenv61, 30, 1, 6.726, kampenv62, 1
if    (kdrnum != 0) goto next1
aout  rand  kampenv1
goto  endswitch
next1:
if    (kdrnum != 1) goto next2
asig  rand    kampenv2
afilt   reson   asig, 1000, 100
aout  balance   afilt, asig
goto  endswitch
next2:
if    (kdrnum != 2) goto next3
asig  rand  kampenv3
afilt  reson  asig, kfreqenv, kfreqenv/8
aout  balance   afilt, asig
goto  endswitch
next3:
if    (kdrnum != 3) goto next4
aout  =   kampenv4*asig4
goto  endswitch
next4:
if    (kdrnum != 4) goto next5
asig  rand  kampenv5
afilt1  reson   asig, kfreqenv51, kfreqenv51/8
afilt2  reson   asig, kfreqenv52, kfreqenv52/4
aout1   balance   afilt1, asig
aout2   balance   afilt2, asig
aout  =   (aout1+aout2)/2
goto  endswitch
next5:
if    (kdrnum != 5) goto endswitch
aout  =     asig5
endswitch:
timout  0, i(kdur1), cont1
kstep   =     frac((kstep + 1)/8)*8
reinit  loop1
cont1:
outs  aout*kleft, aout*kright
endin
instr 2
kampenv   expseg  .0001, .01, p4, .04, .01
asig  rand  kampenv
afilt   reson   asig, 1000, 100
aout  balance   afilt, asig
outs  5*aout, 5*aout
endin
instr 3
kfreqenv  expseg  50, .01, 200, .08, 50
kampenv   expseg  .0001, .01, p4, .08, .01
asig  rand  kampenv
afilt   reson   asig, kfreqenv, kfreqenv/8
aout  balance   afilt, asig
outs    aout, aout
endin
instr 4
kampenv4  linseg  0, .001, 1, p3-.021, 1, .02, 0
kptchenv  linseg  100, .01, 300, .2, 200, .01, 200
asig  pluck   p4, kptchenv, 50, 2, 4, .8, 3
aout  =   kampenv4*asig
outs  aout, aout
endin
instr 5
kfreqenv41 expseg   50, .01, 200, .08, 50
kfreqenv42 linseg   150, .01, 1000, .08, 250
kampenv4  linseg  0, .01, p4, .08, 0, .01, 0
asig  rand  kampenv4
afilt1  reson   asig, kfreqenv41, kfreqenv41/8
afilt2  reson   asig, kfreqenv42, kfreqenv42/4
aout1 balance   afilt1, asig
aout2 balance   afilt2, asig
outs  (aout1+aout2)/2, (aout1+aout2)/2
endin
instr 6
kampenv61 expseg  .01, .01, p4, .2, p4/100, .1, .001
kampenv62 linseg  1, .1, 10, .1, .5, .01, 1
asig  foscil  kampenv61, 30, 1, 6.726, kampenv62, 1
outs  asig, asig
endin
</CsInstruments>
<CsScore>
f1 0 8192 10 1
f2 0 1024 7 1 1024 1
f21 0 8 -2 1  1 1 1 1 1 1 1
f25 0 8 -2 4  1 1 2 4 1 1 2
f29 0 8 -2 4  1 1 1 1 4 2 2
f22 0 8 -2 0  1 0 1 2 1 0 1
f26 0 8 -2 4  3 3 2 4 2 3 4
f30 0 8 -2 4  2 1 5 4 5 5 4
f23 0 8 -2 1 0  1 0 1 0 1 1
f24 0 8 -2 0 1  0 1 1 1 1 0
f27 0 8 -2 1 0  1 1 0 1 1 0
f28 0 8 -2 0 1  0 0 1 1 0 1
f31 0 8 -2 0 0  0 0 0 1 1 0
f32 0 8 -2 1 1  1 1 1 0 1 1
i4 0 0.25 5000 24 25 26 28 30
</CsScore>
</CsoundSynthesizer>
