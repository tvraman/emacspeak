<CsoundSynthesizer>
<CsOptions>
-o window-resize.wav 
</CsOptions>
<CsInstruments>
sr = 44100
ksmps = 10
nchnls = 8
0dbfs = 1

instr 1 
; generate pink noise
anoise pinkish 1
        
; one half  turn 
kalpha line 0, p3, 180
kbeta = 0
        
; generate B format
aw, ax, ay, az, ar, as, at, au, av bformenc1 anoise, kalpha, kbeta
        
; decode B format for 8 channel circle loudspeaker setup
a1, a2, a3, a4, a5, a6, a7, a8 bformdec1 4, aw, ax, ay, az, ar, as, at, au, av        

; write audio out
outo a1, a2, a3, a4, a5, a6, a7, a8
endin

</CsInstruments>
<CsScore>
t 0 120 
i 1 0 .5
i 1 + .25
e
</CsScore>
</CsoundSynthesizer>

