# $Author: tv.raman.tv $
# Description:  Makefile for Emacspeak
# Keywords: Emacspeak,  TTS,Makefile
# {{{ LCD Entry:

# LCD Archive Entry:
# emacspeak| T. V. Raman |raman@cs.cornell.edu
# A speech interface to Emacs |
# Location undetermined
#

# }}}
# {{{ Copyright:

#Copyright (C) 1995 -- 2017, T. V. Raman

# Copyright (c) 1994, 1995 by Digital Equipment Corporation.
# All Rights Reserved.
#
# This file is not part of GNU Emacs, but the same permissions apply.
#
# GNU Emacs is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Emacs; see the file COPYING.  If not, write to
# the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA..

# }}}
# {{{ Configuration
.POSIX:
MAKE=make
MAKEFLAGS=--no-print-directory
README = README

# }}}
# {{{  User level targets emacspeak info  outloud espeak 

emacspeak: config 
	@cd lisp && $(MAKE)  --no-print-directory
	@make   $(README)
	@chmod 644 $(README)
	@echo "See the NEWS file for a  summary of new features — Control e cap n in Emacs"
	@echo "See Emacspeak Customizations for customizations — control e cap C in Emacs"
	@echo  "Read the Emacspeak Manual — Control e TAB in Emacs"
	@make install

outloud: 
	@cd servers/linux-outloud && $(MAKE) --no-print-directory || echo "Cant build Outloud server!"

espeak: 
	@cd servers/native-espeak && $(MAKE) --no-print-directory  || echo "Cant build espeak server!"

# }}}
# {{{  Maintainance targets   dist

GITVERSION=$(shell git show HEAD | head -1  | cut -b 8- )
README: 
	@rm -f README
	@echo "Emacspeak  Revision $(GITVERSION)" > $(README)
	@echo "Distribution created by `whoami` at `date`" >> $(README)
	@echo "Unpack the  distribution And type make config " >> $(README)
	@echo "Then type make" >> $(README)
EXCLUDES=-X .excludes --exclude-backups
dist:
	make ${README}
	tar cvf  emacspeak.tar $(EXCLUDES) .

# }}}
# {{{ User level target--  config

config:
	@cd etc && $(MAKE) config   --no-print-directory
	@cd lisp && $(MAKE) config --no-print-directory


# }}}
# {{{  complete build

all: emacspeak

help:
	@make -s
#clean, config and build (development)
q:
	make clean
	make config 
	make
	@cd lisp && make muggles --no-print-directory
	@cd lisp && make extra-muggles --no-print-directory
	@cd	 tvr && make  --no-print-directory

qq:
	make -s q 2>&1 |grep -v Loading  

# }}}
# {{{  user level target-- clean

clean:
	@cd lisp &&  $(MAKE) --no-print-directory clean

# }}}
# {{{ labeling releases

#label  releases when ready
LABEL=#version number
MSG="Releasing ${LABEL}"
release: #supply LABEL=NN.NN
	git tag -a -s ${LABEL} -m "Tagging release with ${LABEL}"
	git push --tags
	$(MAKE) dist
	mkdir emacspeak-${LABEL}; \
cd emacspeak-${LABEL} ;\
	tar xvf ../emacspeak.tar ; \
	rm -f ../emacspeak.tar ; \
cd .. ;\
	tar cvfj emacspeak-${LABEL}.tar.bz2 emacspeak-$(LABEL); \
	/bin/rm -rf emacspeak-${LABEL} ;\
	echo "Prepared release in emacspeak-${LABEL}.tar.bz2"
	./utils/emacspeak-ghr ${LABEL} "emacspeak-${LABEL}.tar.bz2"

# }}}
# {{{Install: 

install:
	@echo "To run  this Emacspeak build, add this  line to the top of your .emacs:"
	@echo "(load-file \"`pwd`/lisp/emacspeak-setup.el\")"
	@echo "If using espeak or outloud for TTS, "
	@echo "type make <engine> to first build that speech-server."
	@echo "Package maintainers: see   etc/install.org	 for instructions."

# }}}
# {{{Worktree:
# Usage make wk TAG=tag
wk:
	git worktree add ../${TAG}-emacspeak ${TAG}

# }}}
# {{{ end of file

#local variables:
#mode: makefile
#fill-column: 90
#folded-file: t
#end:

# }}}
