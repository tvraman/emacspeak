#$Id$
#http://emacspeak.sf.net/info
SF_HOME= raman,emacspeak@web.sourceforge.net:/home/groups/e/em/emacspeak/htdocs/info
#https://tvraman.github.io/emacspeak/manual
GH_PAGES=../../gh-pages-emacspeak/manual

TEXINDEX=texindex
FILES=*.texi
MAKEINFO = makeinfo 
CSS=-css-ref="http://www.w3.org/StyleSheets/Core/Modernist"
TEX=tex
PDFTEX=pdftex
infodir =/usr/share/info#normally supplied by caller
INSTALL = install
EMACS=emacs

all: info  introducing-emacspeak.info turning-twenty.info

info: emacspeak.texi introducing-emacspeak.texi turning-twenty.texi docs.texi
	$(MAKEINFO) emacspeak.texi
	$(MAKEINFO) introducing-emacspeak.texi
	$(MAKEINFO) turning-twenty.texi

.PHONY: docs.texi 

docs.texi: 
	$(EMACS) -batch -q -l ../utils/self-document.el -f self-document-all-modules

install: emacspeak.info turning-twenty.info
	if test  -x /sbin/install-info ; then \
		install-info *.info /etc/info-dir; \
	fi
	$(INSTALL)  -m 644 *.info* $(DESTDIR)$(infodir)

uninstall:
	if test  -e /sbin/install-info ; then \
		install-info --delete $(infodir)/emacspeak.info* /etc/info-dir; \
	fi
	rm -f $(infodir)/emacspeak.info*
emacspeak.info: 
	makeinfo --force -o emacspeak.info emacspeak.texi

introducing-emacspeak.info: introducing-emacspeak.texi
	makeinfo --force $<

emacspeak.dvi:		emacspeak.texi
	$(TEX) emacspeak.texi
	$(TEX) emacspeak.texi
	$(TEXINDEX) emacspeak.cp  emacspeak.fn  emacspeak.ky  emacspeak.pg  emacspeak.tp  emacspeak.vr
	$(TEX) emacspeak.texi
	rm -f 	emacspeak.cp  emacspeak.fn  emacspeak.ky  emacspeak.pg  emacspeak.tp  emacspeak.vr 	\
		emacspeak.cps emacspeak.fns emacspeak.kys emacspeak.pgs emacspeak.tps emacspeak.vrs	\
		emacspeak.log emacspeak.toc emacspeak.aux
pdf: emacspeak.pdf

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

man: emacspeak.texi 
	makeinfo -o ${GH_PAGES}  --html  ${CSS} emacspeak.texi
	tidy -q -mic -asxml  ${GH_PAGES}/*.html 2>&1 > /dev/null  || echo tidy
	echo "Commit and push updated HTML files from gh-pages!"

introducing-emacspeak.html: introducing-emacspeak.texi
	makeinfo --no-split --html  introducing-emacspeak.texi
	perl add-css.pl introducing-emacspeak.html
#For publishing to SF
pub:  html/index.html introducing-emacspeak.html
	rsync introducing-emacspeak.html ${SF_HOME}
	rsync -avz  html ${SF_HOME}/info/

%.pdf: %.ps
	ps2pdf    $<

%.ps: %.dvi
	@dvips -q   -Pwww  $< 2>&1  > /dev/null 

clean: 
	\rm *.info || echo "all clean"

spell:
	@mv .spell.words .spell.tex
	@cat ${FILES} | aspell --ignore-case -t --list --add-filter texinfo   \
 | sort | uniq > .spell.words
	@diff .spell.tex .spell.words
