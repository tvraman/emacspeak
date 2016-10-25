#$Id: Makefile 6198 2009-08-25 15:19:03Z tv.raman.tv $

INSTALL = install
PREFIX = /usr/local
LIBPARENTDIR = ${PREFIX}/share/emacs/site-lisp
LIBDIR =$(LIBPARENTDIR)/emacspeak/servers/mac-espeak
CXXFLAGS+= -g    -O2 -fPIC  -DPIC  -pedantic -ansi -Wall -Wno-long-long

LIBS= -framework tcl -lespeak-ng


TTS=tclespeak.dylib

all:   $(TTS)

$(TTS): ../linux-espeak/tclespeak.o
	$(CXX) $(LDFLAGS) -dynamiclib -o $@ $< $(LIBS)

clean:
	rm -f *.dylib ../linux-espeak/*.o

install: $(TTS)
	$(INSTALL) -d $(DESTDIR)$(LIBDIR)
	$(INSTALL) $< $(DESTDIR)$(LIBDIR)

