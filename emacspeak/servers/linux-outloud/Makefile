#$Id$
CC=g++
INSTALL = install
PREFIX = /usr
libparentdir = ${PREFIX}/share/emacs/site-lisp
libdir =$(libparentdir)/emacspeak/linux-outloud
CFLAGS= -g   -O2 -fPIC  -DPIC -pedantic -ansi -Wall 
LIBS= -ltcl -ldl

TTS=tcleci.so eci.ini VIAVOICE

all: tcleci.so 


tcleci.lo: tcleci.cpp
	libtool --mode=compile $(CC) $(CFLAGS) -c $<

tcleci.so: tcleci.lo
	libtool --mode=link $(CC) $(CFLAGS) -g -o $@ tcleci.lo $(LIBS) -shared

atcleci.lo: atcleci.cpp
	libtool --mode=compile $(CC) $(CFLAGS) -c $<

atcleci.so: atcleci.lo
	libtool --mode=link $(CC) $(CFLAGS) -g -o $@ atcleci.lo -lasound $(LIBS)  -shared


clean:
	rm -f tcleci.so tcleci.o tcleci.lo

install: tcleci.so
	if test ! -d $(libdir) ; then mkdir $(libdir); fi ; \
	chmod 755 $(libdir); \
	$(INSTALL) -m 0644 $(TTS) $(libdir)
