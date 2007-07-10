SRC=atom-blogger.el blogger-edit.xsl \
	COPYING Makefile

all: atom-blogger.elc


%.elc: %.el
	emacs -batch -q -no-site-file -f batch-byte-compile $<
dist: $(SRC)
	tar cfj atom-blogger.tar.bz2 $(SRC)
