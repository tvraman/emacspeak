#$Id$
TEXINDEX=texindex
MAKEINFO = makeinfo 
TEX=tex
PDFTEX=pdftex
TEXI2HTML=texi2html
infodir =/usr/share/info#normally supplied by caller
INSTALL = install
EMACS=emacs
info: *.texi
	$(MAKEINFO) emacspeak.texi

commands.texi: ../lisp/emacspeak-setup.el
	$(EMACS) -batch -q -l ../utils/document-commands.el

install: emacspeak.info
	if test  -x /sbin/install-info ; then \
		install-info emacspeak.info /etc/info-dir; \
	fi
	$(INSTALL)  -m 644 emacspeak.info* $(infodir)

uninstall:
	if test  -e /sbin/install-info ; then \
		install-info --delete $(infodir)/emacspeak.info /etc/info-dir; \
	fi
	rm -f $(infodir)/emacspeak.info*
emacspeak.info: emacspeak.texi

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
	$(TEXINDEX) emacspeak.cp  emacspeak.fn  emacspeak.ky  emacspeak.pg  emacspeak.tp  emacspeak.vr
	$(PDTEX) emacspeak.texi
	rm -f 	emacspeak.cp  emacspeak.fn  emacspeak.ky  emacspeak.pg  emacspeak.tp  emacspeak.vr 	\
		emacspeak.cps emacspeak.fns emacspeak.kys emacspeak.pgs emacspeak.tps emacspeak.vrs	\
		emacspeak.log emacspeak.toc emacspeak.aux

print: emacspeak.dvi
	dvips emacspeak.dvi

emacspeak.html: *.texi
	texi2html -monolithic emacspeak.texi
	perl add-css.pl emacspeak.html

pub:  emacspeak.html
	scp emacspeak.html     raman@emacspeak.sourceforge.net:~/www-emacspeak/htdocs/info

clean: 
