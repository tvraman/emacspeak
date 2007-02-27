#$Id$
SF_HOME=raman@emacspeak.sf.net:~/www-emacspeak/htdocs
HTML_FILES=$(shell find . -name '*.html' -print)
TEXINDEX=texindex
FILES=*.texi
MAKEINFO = makeinfo 
TEX=tex
PDFTEX=pdftex
TEXI2HTML=texi2html
infodir =/usr/share/info#normally supplied by caller
INSTALL = install
EMACS=emacs

all: emacspeak.info 

info: *.texi
	$(MAKEINFO) emacspeak.texi
.PHONY: commands.texi 

commands.texi: 
	$(EMACS) -batch -q -l ../utils/document-commands.el
	rm -f *.info* emacspeak.html
	make
	make htm

options.texi:  
	$(EMACS) -batch -q -l ../utils/document-commands.el

install: emacspeak.info
	if test  -x /sbin/install-info ; then \
		install-info emacspeak.info /etc/info-dir; \
	fi
	$(INSTALL)  -m 644 *.info* $(infodir)

uninstall:
	if test  -e /sbin/install-info ; then \
		install-info --delete $(infodir)/*.info /etc/info-dir; \
	fi
	rm -f $(infodir)/*.info*
emacspeak.info: emacspeak.texi 

introducing-emacspeak.info: introducing-emacspeak.texi
	makeinfo $<

emacspeak.dvi:		emacspeak.texi
	$(TEX) emacspeak.texi
	$(TEX) emacspeak.texi
	$(TEXINDEX) emacspeak.cp  emacspeak.fn  emacspeak.ky  emacspeak.pg  emacspeak.tp  emacspeak.vr
	$(TEX) emacspeak.texi
	rm -f 	emacspeak.cp  emacspeak.fn  emacspeak.ky  emacspeak.pg  emacspeak.tp  emacspeak.vr 	\
		emacspeak.cps emacspeak.fns emacspeak.kys emacspeak.pgs emacspeak.tps emacspeak.vrs	\
		emacspeak.log emacspeak.toc emacspeak.aux


emacspeak.pdf:		emacspeak.texi
	$(PDFTEX) emacspeak.texi
	$(PDFTEX) emacspeak.texi	
	$(TEXINDEX) emacspeak.cp  emacspeak.fn  emacspeak.ky emacspeak.pg  emacspeak.tp  emacspeak.vr
	$(PDFTEX) emacspeak.texi
	$(PDFTEX) emacspeak.texi
	rm -f 	emacspeak.cp  emacspeak.fn  emacspeak.ky  emacspeak.pg  emacspeak.tp  emacspeak.vr 	\
		emacspeak.cps emacspeak.fns emacspeak.kys emacspeak.pgs emacspeak.tps emacspeak.vrs	\
		emacspeak.log emacspeak.toc emacspeak.aux

print: emacspeak.dvi
	dvips emacspeak.dvi

.phony: html

htm: emacspeak.texi 
	makeinfo -o html  --html  emacspeak.texi
	perl add-css.pl html/*.html
	for f in $(HTML_FILES) ; \
		do \
		tidy -q -mic -asxml  $$f 2>&1 > /dev/null  || echo tidy; \
		done


introducing-emacspeak.html: introducing-emacspeak.texi
	makeinfo --no-split --html  introducing-emacspeak.texi
	perl add-css.pl introducing-emacspeak.html

pub:  html/index.html introducing-emacspeak.html
	rsync introducing-emacspeak.html ${SF_HOME}/info/
	rsync -avz  html ${SF_HOME}/info/

%.pdf: %.ps
	ps2pdf    $<

%.ps: %.dvi
	@dvips -q   -Pwww  $< 2>&1  > /dev/null 

clean: 


spell:
	@mv .spell.words .spell.tex
	@cat ${FILES} | aspell -t -l | sort | uniq > .spell.words
	@diff .spell.tex .spell.words
