#$Id$
TEXINDEX=texindex
MAKEINFO = makeinfo 
TEXI2HTML=texi2html
infodir =/usr/info#normally supplied by caller
INSTALL = install

info: emacspeak.texi
	chmod 644 *info*
	makeinfo emacspeak.texi
	texi2html emacspeak.texi

install: emacspeak.info
	if test  -e /sbin/install-info ; then \
		install-info emacspeak.info /etc/info-dir; \
	fi
	$(INSTALL)  -m 644 emacspeak.info* $(infodir)
	$(INSTALL)  -m 644 emacspeak*.html $(infodir)

uninstall:
	if test  -e /sbin/install-info ; then \
		install-info --delete $(infodir)/emacspeak.info /etc/info-dir; \
	fi
	rm -f $(infodir)/emacspeak.info*
emacspeak.info: emacspeak.texi

emacspeak.dvi:		emacspeak.texi
	tex emacspeak.texi
	$(TEXINDEX) emacspeak.cp  emacspeak.fn  emacspeak.ky  emacspeak.pg  emacspeak.tp  emacspeak.vr
	tex emacspeak.texi
	rm -f 	emacspeak.cp  emacspeak.fn  emacspeak.ky  emacspeak.pg  emacspeak.tp  emacspeak.vr 	\
		emacspeak.cps emacspeak.fns emacspeak.kys emacspeak.pgs emacspeak.tps emacspeak.vrs	\
		emacspeak.log emacspeak.toc emacspeak.aux

print: emacspeak.dvi
	dvips emacspeak.dvi

clean: 
