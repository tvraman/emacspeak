<CsoundSynthesizer>
<CsOptions>
-odac -L stdin  -+rtaudio=ALSA
</CsOptions>
<CsInstruments>sr = 44100
ksmps = 10
nchnls = 2
0dbfs = 1   
; p4 p5 are start and end frequency
; p6 p7 are envelope attach/decay 
instr 1
kenv  linen   .7, p6, p3, p7 ; amplitude envelope 
kp line p4, p3, p5 ; frequency range
  ain       pluck     kenv,kp, 440, 0, 3
outs ain, ain 
endin
; Same as before, but exponential for frequency 
instr 2
kenv  linen   .7, p6, p3, p7
kp expon p4, p3, p5
  ain       pluck     kenv,kp, 440, 0, 3
outs ain, ain 
endin
;;; amplitude envelope varies over time 
instr 3
kenv  linen   .7, p6, p3, p7
kp linseg p4, p3*0.5, p5,  p3, p4
  ain       pluck     kenv,kp, 440, 0, 3
outs ain, ain 
endin
; bell  
instr 4

kamp = p4
kfreq = p5
kc1 = p6
kc2 = p7
kvdepth = 0.005
kvrate = 6

asig fmbell kamp, kfreq, kc1, kc2, kvdepth, kvrate
     outs asig, asig
endin

instr 5
kcps = cpspch(p4)
icps = cpspch(p4)
kenv linen .7, 0.05, p3, 0.05
kaz	linseg  0, p3, 360
  ain       pluck     kenv, kcps, icps, 0, 1
 aleft,aright hrtfmove2 ain, kaz,-20, "hrtf-44100-left.dat","hrtf-44100-right.dat"
 outs	aleft, aright
endin

</CsInstruments>
</CsoundSynthesizer>
