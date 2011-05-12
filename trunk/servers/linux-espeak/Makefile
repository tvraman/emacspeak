#$Id: Makefile 6198 2009-08-25 15:19:03Z tv.raman.tv $

INSTALL = install
PREFIX = /usr
LIBPARENTDIR = ${PREFIX}/share/emacs/site-lisp
LIBDIR =$(LIBPARENTDIR)/emacspeak/servers/linux-espeak
TCL_VERSION = 8.4
TCL_INCLUDE= /usr/include/tcl$(TCL_VERSION)
CXXFLAGS+= -g    -O2 -fPIC  -DPIC  -pedantic -ansi -Wall -Wno-long-long -I$(TCL_INCLUDE)

LIBS= -ltcl$(TCL_VERSION) -lespeak

TTS=tclespeak.so

all:   $(TTS)

$(TTS): tclespeak.o
	$(CXX) $(LDFLAGS) -shared -o $@ $< $(LIBS)

clean:
	rm -f *.so *.o

install: $(TTS)
	$(INSTALL) -d $(DESTDIR)$(LIBDIR)
	$(INSTALL) $< $(DESTDIR)$(LIBDIR)

