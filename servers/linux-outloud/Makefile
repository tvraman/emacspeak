#$Id$
CC=g++
INSTALL = install
PREFIX = /usr
libparentdir = ${PREFIX}/share/emacs/site-lisp
libdir =$(libparentdir)/emacspeak/servers/linux-outloud

CFLAGS= -g    -O2 -fPIC  -DPIC  -pedantic -ansi -Wall  -Wno-long-long -I/usr/include/tcl8.4
LIBS= -ltcl8.4 -ltclx8.4 -ldl    

ATTS=atcleci.cpp  ALSA asoundrc
MISC=alsaparams.c
all:   atcleci.so

langswitch.lo: langswitch.cpp
	libtool --mode=compile $(CC) $(CFLAGS) -c $<

atcleci.lo: atcleci.cpp
	libtool --mode=compile $(CC) $(CFLAGS) -c $<

libatcleci.la: atcleci.lo langswitch.lo
	libtool --mode=link $(CC) $(CFLAGS) -g -o $@ $^ -lasound $(LIBS) -rpath /usr/local/lib -shared

.libs/libatcleci.so: libatcleci.la

atcleci.so: .libs/libatcleci.so
			cp -H $< $@

alsaparams.o: alsaparams.c
	libtool --mode=compile $(CC) $(CFLAGS) -c $<

alsaparams: alsaparams.o
	libtool --mode=link $(CC) $(CFLAGS) -g -o $@ $<  -lasound 
clean:
	rm -f *.so *.la *.o *.lo \
atcleci.so atcleci.o atcleci.lo
	rm -rf .libs

install: atcleci.so
	if test ! -d $(libdir) ; then mkdir $(libdir); fi ; \
	chmod 755 $(libdir); \
	$(INSTALL) -m 0644  $(ATTS) $(libdir)
