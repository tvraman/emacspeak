#$Id$
#Makefile for beeps
CC=gcc
CFLAGS = -O4 -Wall -pedantic -ansi
SRC = beeps.c Makefile beeps.el
beeps: beeps.c
	${CC} ${CFLAGS} -o $@ $<
	strip $@

dist: ${SRC}
	tar cvf beeps.tar ${SRC}
	gzip -9 beeps.tar
