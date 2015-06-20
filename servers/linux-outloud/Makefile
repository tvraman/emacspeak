#$Id$

INSTALL = install
PREFIX = /usr
LIBPARENTDIR = ${PREFIX}/share/emacs/site-lisp
LIBDIR =$(LIBPARENTDIR)/emacspeak/servers/linux-outloud

TCL_VERSION=8.6
TCL_INCLUDE=/usr/include/tcl$(TCL_VERSION)
#CFLAGS+= -g    -O2 -fPIC  -DPIC  -pedantic -ansi -Wall  -Wno-long-long -I$(TCL_INCLUDE)
CFLAGS+= -m32 -g    -O2 -fPIC  -DPIC  -pedantic -ansi -Wall  -Wno-long-long -I$(TCL_INCLUDE)
CXXFLAGS+= $(CFLAGS)
LIBS= -ltcl$(TCL_VERSION) -lasound -ldl    

all:   atcleci.so

atcleci.so: langswitch.o atcleci.o
	$(CXX) $(LDFLAGS) -m32 -shared -o $@ langswitch.o atcleci.o $(LIBS)

clean:
	rm -f *.so *.o 

install: atcleci.so
	$(INSTALL) -d $(DESTDIR)$(LIBDIR)
	$(INSTALL) $< $(DESTDIR)$(LIBDIR)

