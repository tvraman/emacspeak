#$Id$
#
LABEL=1.0
DIST=software-dtk-$(LABEL)
CFLAGS=-g -fpic  -pedantic -ansi -Wall
INSTALL=install
CC=gcc
LIBS=-ltts -ltts_us -ltcl
SERVER_DIR=/usr/share/emacs/site-lisp/emacspeak/servers/software-dtk
FILES=tcldtk.c tcldtk.so tcldtk.o Makefile
	libparentdir = ${PREFIX}/share/emacs/site-lisp
libdir =$(libparentdir)/emacspeak/linux-outloud
all: tcldtk.so

tcldtk.lo: tcldtk.c
	libtool --mode=compile $(CC) $(CFLAGS) -c $<

tcldtk.so: tcldtk.lo
	libtool --mode=link $(CC) $(CFLAGS) -g -o $@ tcldtk.lo $(LIBS) -shared

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

