#$Id: Makefile 6198 2009-08-25 15:19:03Z tv.raman.tv $

INSTALL = install
PREFIX = /usr
LIBPARENTDIR = ${PREFIX}/share/emacs/site-lisp
LIBDIR =$(LIBPARENTDIR)/emacspeak/servers/native-speechd

CXXFLAGS+= -g    -O2 -fPIC  -DPIC  -pedantic -ansi \
-Wall -Wno-long-long --std=c++11

ifeq ($(shell uname -s), Darwin)

	# Mac OS land.
  LIBS= -framework tcl -lspeechd
  TTS=tclspeechd.dylib
else
  TCL_VERSION = 8.6
  TCL_INCLUDE= /usr/include/tcl$(TCL_VERSION)
  LIBS= -ltcl$(TCL_VERSION) -lspeechd
  TTS=tclspeechd.so
  CXXFLAGS+=-I$(TCL_INCLUDE)
endif

all:   $(TTS)

$(TTS): tclspeechd.o
	$(CXX) $(LDFLAGS) -shared -o $@ $< $(LIBS)

clean:
	rm -f *.so *.o

tidy:
	clang-tidy -header-filter=.*-checks='*' -fix-errors tclspeechd.cpp -- -std=c++11 -I/usr/include/tcl8.6 

install: $(TTS)
	$(INSTALL) -d $(DESTDIR)$(LIBDIR)
	$(INSTALL) $< $(DESTDIR)$(LIBDIR)

