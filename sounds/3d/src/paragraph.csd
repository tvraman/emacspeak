<CsoundSynthesizer>

<CsOptions>
-o paragraph.wav
</CsOptions>

<CsInstruments>
;===========
; toot04.orc
;===========

sr        =         44100
kr        =         441
ksmps     =         100
nchnls  =2

          instr 4
iamp      =         ampdb(p4)           ; convert decibels to linear amp
iscale    =         iamp * .333         ; scale the amp at initialization
inote     =         cpspch(p5)          ; convert octave.pitch to cps
kaz	expon 225, p3, 45		

k1        linen     iscale, p6, p3, p7  ; p4=amp

a3        oscil     k1, inote*.996, 1   ; p5=freq
a2        oscil     k1, inote*1.004, 1  ; p6=attack time
a1        oscil     k1, inote, 1        ; p7=release time

al1, ar1  hrtfmove2 a1, kaz,-20, "hrtf-44100-left.dat","hrtf-44100-right.dat"
al2, ar2  hrtfmove2 a2, kaz,-20, "hrtf-44100-left.dat","hrtf-44100-right.dat"
al3, ar3  hrtfmove2 a3, kaz,-20, "hrtf-44100-left.dat","hrtf-44100-right.dat"
aleft =al1+al2+al3
aright =ar1+ar2+ar3
          outs  aleft, aright
          endin

</CsInstruments>

<CsScore>
;===========
; toot04.sco
;===========

f1   0    4096 10 1      ; sine wave

;ins strt dur  amp  freq      attack    release
i4   0    1    90   8.04      0.1       0.7
;i4   1    1    70   8.02      0.07      0.6
;i4   2    1    75   8.00      0.05      0.5
;i4   3    1    70   8.02      0.05      0.4
;i4   4    1    85   8.04      0.1       0.5
;i4   5    1    80   8.04      0.05      0.5
;i4   6    2    90   8.04      0.03      1.

</CsScore>

</CsoundSynthesizer>
