#$Id$
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
	make emacspeak.html

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

emacspeak.html: emacspeak.texi 
	makeinfo --no-split --html  emacspeak.texi
	perl add-css.pl emacspeak.html

introducing-emacspeak.html: introducing-emacspeak.texi
	makeinfo --no-split --html  introducing-emacspeak.texi
	perl add-css.pl introducing-emacspeak.html

pub:  emacspeak.html introducing-emacspeak.html
	scp *.html     raman@emacspeak.sourceforge.net:~/www-emacspeak/htdocs/info

%.pdf: %.ps
	ps2pdf    $<

%.ps: %.dvi
	@dvips -q   -Pwww  $< 2>&1  > /dev/null 

clean: 


spell:
	@mv .spell.words .spell.tex
	@cat ${FILES} | aspell -t -l | sort | uniq > .spell.words
	@diff .spell.tex .spell.words
