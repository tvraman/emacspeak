#!/bin/sh
#$Id$
#Generate HTML versions of Emacs reference cards
PREFIX=/usr/share/emacs/21.2/etc
REFS=${PREFIX}/survival.tex \
${PREFIX}/refcard.tex \
${PREFIX}/dired-ref 

for i in ${REFS}
do
tex $i
dvips -PWWW  `basename $i tex`dvi -o `basename $i tex`ps
ps2pdf `basename $i tex`ps
pdftohtml -noframes `basename $i tex`pdf
tidy -mc `basename $i tex`html
done
