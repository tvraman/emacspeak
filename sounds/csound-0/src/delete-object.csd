<CsoundSynthesizer>
<CsOptions>
; XO
;-+rtmidi=alsa  --midi-device=hw:1,0 -+rtaudio=alsa -odac -r16000 -k160 ;-O stdout
; Mac
-odac -r44100 -k441
</CsOptions>
<CsInstruments>
;THIS ORCHESTRA IS AN ATTEMPT AT REALISING PITCH AND ONSET-TIME
;USING MARKOV PROCESSES.  THE CURRENT PROBLEM HAS TO DO
;WITH CLICKING OF ENVELOPES    A.D. HANNA
;MODIFIED ENVELOPES TO REDUCE POPPING M VIGORITO
nchnls    =         2 
ga1       init      0
          instr     1
itablesize =        32 ;PITCH TABLES 2 AND 3 ARE INDEXED FROM 0 TO 31
iratchoose =        p9 ;DETERMINES WHICH OF TWO TEMPOS ARE USED
kdclick   linseg    0,.04,1,p3-.08,1,.04,0
;A NEAT WAY TO REPRESENT MARKOV CHAINS.
;CHOOSE BETWEEN TWO SUBSETS OF THE SET "FURNITURE"
if (iratchoose == 0) igoto furniture_to_sit_on
if (iratchoose == 1) igoto furniture_to_put_stuff_in
;----------------------------------------------------------------------
furniture_to_sit_on:
        ;BEING HERE DEPENDS ON OUTCOME OF p9
kindex phasor p5*p6 
        plantatulip:
kenv      linen 1,.04,1/p5,1/p5*.5
inotechoose =       p8
        ;CHOOSE BETWEEN TWO PITCH TABLES
        ;THE SECOND OF WHICH IS A TRANSPOSITION
        ;OF THE FIRST IN THE ORDER OF 1 SEMITONE.
        if (inotechoose == 1) igoto chair
        if (inotechoose == 0) igoto sofa
        ;--------------------------------------------------
        chair:  ;BEING HERE DEPENDS ON THE OUTCOME OF p9 AND p8
                iipitch table i(kindex)*itablesize,2
                igoto resume
        ;--------------------------------------------------
        sofa:   ;BEING HERE DEPENDS ON THE OUTCOME OF p9 AND p8
                iipitch table i(kindex)*itablesize,3
                igoto resume
        resume:
        timout 0,1/p5,pluckatulip
        reinit plantatulip
;----------------------------------------------------------------------
furniture_to_put_stuff_in:
        ;BEING HERE DEPENDS ON OUTCOME OF p9
        kindex phasor (p5*p6)*1.125 ;varies phase of lfo vs beat 
        ;THUS CREATING MELODIC PATTERNS
        plantadaisy:
kenv      linen     1,.04,1/p5,1/p5*.5 
        inotechoose = p8
        if (inotechoose == 1) igoto bookcase
        if (inotechoose == 0) igoto wardrobe
        ;-----------------------------------------------
        bookcase:       ;BEING HERE DEPENDS ON THE OUTCOME OF p9 AND p8
                        iipitch table i(kindex)*itablesize,2
                        igoto continue
        ;--------------------------------------------------
        wardrobe:       ;BEING HERE DEPENDS ON THE OUTCOME OF p9 AND p8
                        iipitch table i(kindex)*itablesize,3
                        igoto continue
        continue:
        timout 0,1/p5,pluckatulip
        reinit plantadaisy 
pluckatulip: 
a2        oscili    ampdb(p4)*kenv*kdclick,cpspch(iipitch+p7),1 ;p7 = OCTAVE TRANSPOSITION     
ga1       =         a2+ga1
          outs      (1-kenv)*8*a2,kenv*8*a2
          endin
 
          instr     50                     
kfreq     expseg    0.01,p3*.2,.4,p3*.2,1,p3*.2,1.5,p3*.2,.1,p3*.2,.05
k1        oscili    .5,kfreq*p4,1 ;kfreq*p4=cps of pan
k2        =         .5+k1
k3        =         1-k2
a1        reverb2   ga1,2.1,.5
          outs      k2*a1,a1*k3*(-1)
ga1       =         0
          endin
</CsInstruments>
<CsScore> 
f1 0 8192 10 1

f2 0 64 -2 7.10 7.02 7.07 8.05 9.02 7.05 8.10 8.02 7.07 8.07 8.05 8.02 8.10
9.02 7.07 9.07 8.00 7.02 8.02 8.10 7.07 7.05 8.00 8.02 9.02 9.02 8.10 7.07
8.05 9.02 8.02 9.07 

f3 0 64 -2 7.11 7.03 7.08 8.06 9.03 7.06 8.11 8.03 7.08 8.08 8.06 8.03 8.11
9.03 7.08 9.08 8.01 7.03 8.03 8.11 7.08 7.06 8.01 8.03 9.03 9.03 8.11 7.06
8.06 9.03 8.03 9.08    
i50 0   0.5     10 
; ------- BEGIN OF FIELD 1 --- SECONDS: 0.00 - 28.00 --------
;ins    time    dur     p4      p5      p6      p7      p8      p9 
 ;i1      0       0.5    65      3.29    0.46    -1      0       1       
 ;i1      0.0    0.25   65      3.36    0.78    1       1       1       
 ;i1      0.625   1       65      6.95    0.39    1       1       1       
 ;i1 0   .9    65      3.32    0.53    -1      1       -1       
 i1      0    0.5    65      4.2     0.86    1       0       1       
; i1      3.75    1.25    65      7.36    0.15    1       0       0       
; i1      4.125   1       65      6.2     0.99    -1      0       0       
; i1      4.375   0.375   65      1.12    0.69    1       0       0       
; i1      4.75    0.25    65      4.28    0.42    -1      0       1       
; i1      5.75    0.375   65      1.69    0.18    1       0       1       
; i1      6.875   1       65      8.19    0.91    1       1       0       
; i1      7.875   1.25    65      3.28    0.34    -1      0       1       
; i1      8.25    0.25    65      3.3     0.11    1       0       0       
; i1      8.5     1.25    65      1.97    1       1       1       0       
; i1      8.875   1       65      2.82    0.11    -1      0       0       
; i1      9.875   0.375   65      3.4     0.21    1       1       0       
; i1      11      0.25    65      3.01    0.97    1       1       1       
; i1      12      0.375   65      5.54    0.18    -1      1       0       
; i1      12.375  1       65      1.44    0.98    1       1       1       
; i1      12.625  1.25    65      4.33    0.86    1       1       1       
; i1      13      0.25    65      6.15    0.77    -1      1       1       
; i1      14      1.25    65      6.48    0.24    1       1       0       
; i1      15.125  1       65      3.91    0.62    -1      1       1       
; i1      16.125  0.375   65      1.12    0.33    1       0       1       
; i1      16.5    0.25    65      7.43    0.73    1       1       1       
; i1      16.75   0.375   65      2.59    0.9     -1      0       0       
; i1      17.125  1       65      5.57    0.13    1       0       1       
; i1      18.125  1.25    65      4.08    0.77    1       0       0       
; i1      19.25   0.25    65      8.57    0.37    -1      0       1       
; i1      20.25   1.25    65      2.42    0.3     1       1       0       
; i1      20.625  1       65      4.24    0.52    1       0       1       
; i1      20.875  0.375   65      5.75    0.67    -1      1       1       
; i1      21.25   0.25    65      1.24    0.64    1       0       0       
; i1      22.25   0.375   65      8.26    0.75    1       1       0       
; i1      23.375  1       65      7.23    0.24    -1      0       0       
; i1      24.375  1.25    65      3.9     0.62    1       1       1       
; i1      24.75   0.25    65      6.14    0.58    -1      1       0       
; i1      25      1.25    65      7.11    0.96    1       0       0       
; i1      25.375  1       65      6.33    0.14    1       1       0       
; i1      26.375  0.375   65      8.53    0.92    -1      0       0       
; i1      27.5    0.25    65      7.9     0.4     1       0       1        
e
; ------- END OF FIELD 1 --- NUMBER OF EVENTS: 41 -------
</CsScore>

</CsoundSynthesizer>
