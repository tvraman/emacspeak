#$Id$
#
LABEL=1.1
TCL_VERSION=8.6
TCL_INCLUDE=/usr/include/tcl$(TCL_VERSION)
CFLAGS+= -g    -O4 -fPIC  -DPIC  -pedantic -ansi -Wall  -Wno-long-long -I$(TCL_INCLUDE)
LIBS=-ltcl$(TCL_VERSION) -ltts -ltts_us  -ldl
tcldtk.so: tcldtk.o
	$(CC) $(LDFLAGS) -shared -o $@ $< $(LIBS)

clean:
	rm tcldtk.so tcldtk.o

tidy:
	clang-tidy -header-filter=.*-checks='*' -fix-errors tcldtk.c \
	-- -std=c11 -I${TCL_INCLUDE}
