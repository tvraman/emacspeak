#$Id: Makefile 6198 2009-08-25 15:19:03Z tv.raman.tv $

INSTALL = install
PREFIX = /usr
LIBPARENTDIR = ${PREFIX}/share/emacs/site-lisp
LIBDIR =$(LIBPARENTDIR)/emacspeak/servers/native-espeak

CXXFLAGS+= -g    -O2 -fPIC  -DPIC  -pedantic -ansi -Wall -Wno-long-long

ifeq ($(shell uname -s), Darwin)

	# Mac OS land.
  LIBS= -framework tcl -lespeak-ng
  TTS=tclespeak.dylib
else
  TCL_VERSION = 8.6
  TCL_INCLUDE= /usr/include/tcl$(TCL_VERSION)
  LIBS= -ltcl$(TCL_VERSION) -lespeak-ng
  TTS=tclespeak.so
  CXXFLAGS+=-I$(TCL_INCLUDE)
endif

all:   $(TTS)

$(TTS): tclespeak.o
	$(CXX) $(LDFLAGS) -shared -o $@ $< $(LIBS)

clean:
	rm -f *.so *.o

install: $(TTS)
	$(INSTALL) -d $(DESTDIR)$(LIBDIR)
	$(INSTALL) $< $(DESTDIR)$(LIBDIR)

