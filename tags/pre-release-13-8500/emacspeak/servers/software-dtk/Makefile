#$Id$
#
CFLAGS=-g -fpic
CC=gcc
LIBS=-ltts -ltts_us -ltcl

tcldtk.so: tcldtk.o
	$(CC) $< $(LIBS)  -shared -o $@

clean:
	rm tcldtk.so tcldtk.o
