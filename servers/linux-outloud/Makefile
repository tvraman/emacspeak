#$Id$
# Now setup to use Voxin 1.6 with direct 64-bit support.
# Type make, run ./servers/outloud for running the server.
INSTALL = install
PREFIX = /usr
LIBPARENTDIR = ${PREFIX}/share/emacs/site-lisp
LIBDIR =$(LIBPARENTDIR)/emacspeak/servers/linux-outloud

TCL_VERSION=8.6
TCL_INCLUDE=/usr/include/tcl$(TCL_VERSION)
CFLAGS+= -g    -O4 -fPIC  -DPIC  -pedantic -ansi -Wall  -Wno-long-long -std=c++11 -I$(TCL_INCLUDE)
CXXFLAGS+= $(CFLAGS)
LIBS= -ltcl$(TCL_VERSION) -lasound -ldl    

all:   atcleci.so

atcleci.so: langswitch.o atcleci.o
	$(CXX) $(LDFLAGS)  -shared -o $@ langswitch.o atcleci.o $(LIBS)

clean:
	rm -f *.so *.o 

tidy:
	clang-tidy -header-filter=.*-checks='*' -fix-errors langswitch.cpp -- -std=c++11 -I/usr/include/tcl8.6 
	clang-tidy -header-filter=.*-checks='*' -fix-errors atcleci.cpp -- -std=c++11 -I/usr/include/tcl8.6 

install: atcleci.so
	$(INSTALL) -d $(DESTDIR)$(LIBDIR)
	$(INSTALL) $< $(DESTDIR)$(LIBDIR)
