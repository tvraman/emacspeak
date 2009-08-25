#$Id: Makefile 6198 2009-08-25 15:19:03Z tv.raman.tv $
CC=g++
INSTALL = install
PREFIX = /usr
libparentdir = ${PREFIX}/share/emacs/site-lisp
libdir =$(libparentdir)/emacspeak/servers/linux-espeak
INCLUDE= -I/usr/include/tcl8.4 -I/usr/include/espeak
CFLAGS= -g    -O2 -fPIC  -DPIC  -pedantic -ansi -Wall -Wno-long-long $(INCLUDE)

LIBS= -ltcl8.4 -ltclx8.4 -lespeak

TTS=tclespeak.cpp

all:   tclespeak.so


	

tclespeak.lo: tclespeak.cpp
	libtool --mode=compile $(CC) $(CFLAGS) -c $<

libtclespeak.la: tclespeak.lo 
	libtool --mode=link $(CC) $(CFLAGS) -g -o $@ $^  $(LIBS) -rpath /usr/local/lib -shared

.libs/libtclespeak.so: libtclespeak.la

tclespeak.so: .libs/libtclespeak.so
			cp -H $< $@

clean:
	rm -f *.so *.la *.o *.lo \
tclespeak.so tclespeak.o tclespeak.lo
	rm -rf .libs

install: tclespeak.so
	if test ! -d $(libdir) ; then mkdir $(libdir); fi ; \
	chmod 755 $(libdir); \
	$(INSTALL) -m 0644  $(TTS) $(libdir)
