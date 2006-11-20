#$Id$
#
LABEL=1.1
DIST=software-dtk-$(LABEL)
CFLAGS=-O3 -fpic -pedantic -ansi -Wall -I/usr/include/tcl8.3 -I/usr/include/tclX8.3
CPPFLAGS=-DSTDC_HEADERS=1 -Dclock_t=long -DRETSIGTYPE=void 
INSTALL=install
CC=gcc
LIBS=-ltts -ltts_us -ltclx8.3
SERVER_DIR=/usr/share/emacs/site-lisp/emacspeak/servers/software-dtk
FILES=tcldtk.c tcldtk.so tcldtk.o Makefile

tcldtk.so: tcldtk.o
	$(CC) $< $(LIBS)  -shared -o $@

dist: software-dtk.tar.gz

software-dtk.tar.gz:
	mkdir $(DIST) ;
	cp $(FILES) $(DIST) ;
	tar cfz software-dtk.tar.gz $(DIST)
	/bin/rm -rf $(DIST)

clean:
	rm tcldtk.so tcldtk.o

install:
	$(INSTALL) -d $(SERVER_DIR)
	cp  $(FILES) $(SERVER_DIR)

rpm: software-dtk.spec software-dtk.tar.gz
	@cp software-dtk.tar.gz /usr/src/redhat/SOURCES/
	@cp software-dtk.spec /usr/src/redhat/SPECS/
	rpm  -bb --sign --clean   /usr/src/redhat/SPECS/software-dtk.spec

