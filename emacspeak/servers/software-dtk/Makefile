#$Id$
#
CFLAGS=-g
CC=gcc
LIBS=-ltts -ltts_us -ltcl

tcldtk.so: tcldtk.o
	$(CC) $< $(LIBS)  -shared -o $@

