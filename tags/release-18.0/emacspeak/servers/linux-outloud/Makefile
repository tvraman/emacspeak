#$Id$
CC=g++
INSTALL = install
PREFIX = /usr
libparentdir = ${PREFIX}/share/emacs/site-lisp
libdir =$(libparentdir)/emacspeak/linux-outloud
TTS=tcleci.so eci.ini VIAVOICE

all: tcleci.so 

CPPFLAGS=  -DUNIX -fpic  -g

tcleci.so: tcleci.o
	$(CC) $(CFLAGS) -g -o $@ tcleci.o -ltcl -ldl -shared

clean:
	rm -f tcleci.so tcleci.o

install: tcleci.so
	if test ! -d $(libdir) ; then mkdir $(libdir); fi ; \
	chmod 755 $(libdir); \
	$(INSTALL) -m 0644 $(TTS) $(libdir)
