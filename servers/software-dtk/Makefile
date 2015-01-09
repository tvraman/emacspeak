#$Id$
#
LABEL=1.1
DIST=software-dtk-$(LABEL)
TCL_VERSION=8.6
CFLAGS+=-O3 -fpic -pedantic -ansi -Wall -I/usr/include/tcl$(TCL_VERSION)
CPPFLAGS+=-DSTDC_HEADERS=1 -Dclock_t=long -DRETSIGTYPE=void 
INSTALL=install
LIBS=-ltts -ltts_us -ltcl$(TCL_VERSION)
SERVER_DIR=/usr/share/emacs/site-lisp/emacspeak/servers/software-dtk
FILES=tcldtk.c tcldtk.so tcldtk.o Makefile

tcldtk.so: tcldtk.o
	$(CC) $(LDFLAGS) -shared -o $@ $< $(LIBS)

dist: software-dtk.tar.gz

software-dtk.tar.gz:
	mkdir $(DIST) ;
	cp $(FILES) $(DIST) ;
	tar cfz software-dtk.tar.gz $(DIST)
	/bin/rm -rf $(DIST)

clean:
	rm tcldtk.so tcldtk.o

install:
	$(INSTALL) -d $(DESTDIR)$(SERVER_DIR)
	cp  $(FILES) $(DESTDIR)$(SERVER_DIR)

rpm: software-dtk.spec software-dtk.tar.gz
	@cp software-dtk.tar.gz /usr/src/redhat/SOURCES/
	@cp software-dtk.spec /usr/src/redhat/SPECS/
	rpm  -bb --sign --clean   /usr/src/redhat/SPECS/software-dtk.spec

