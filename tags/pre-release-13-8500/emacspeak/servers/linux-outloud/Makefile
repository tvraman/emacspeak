#$Id: Makefile,v 1.1 1999/11/12 18:41:44 tvraman Exp tvraman
#$
INSTALL = install
PREFIX = /usr
libparentdir = ${PREFIX}/share/emacs/site-lisp
libdir =$(libparentdir)/emacspeak/linux-outloud
TTS=tcleci.so eci.ini NOTES

all: tcleci.so 

CPPFLAGS=  -DUNIX -fpic 

tcleci.so: tcleci.o
	$(CC) $(CFLAGS) -o $@ tcleci.o  -ltcl  -libmeci50 -shared

clean:
	rm -f tcleci.so tcleci.o

install: tcleci.so
	if test ! -d $(libdir) ; then mkdir $(libdir); fi ; \
	chmod 755 $(libdir); \
	$(INSTALL) -m 0644 $(TTS) $(libdir)
