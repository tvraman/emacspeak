#$Id$
#
CFLAGS=-g -fpic
INSTALL=install
CC=gcc
LIBS=-ltts -ltts_us -ltcl
SERVER_DIR=/usr/share/emacs/site-lisp/emacspeak/servers/software-dtk
FILES=tcldtk.c tcldtk.so tcldtk.o  DTK

tcldtk.so: tcldtk.o
	$(CC) $< $(LIBS)  -shared -o $@

clean:
	rm tcldtk.so tcldtk.o

install:
	$(INSTALL) -d $(SERVER_DIR)
	cp  $(FILES) $(SERVER_DIR)
