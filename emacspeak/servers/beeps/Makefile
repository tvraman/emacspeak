#$Id$
#Makefile for beeps
CC=gcc
CFLAGS = -O4 -Wall -pedantic -ansi
SRC = notes.c Makefile notes.el
notes: notes.c
	${CC} ${CFLAGS} -o $@ $<
	strip $@


clean:
	rm -f notes.o notes
