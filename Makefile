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

#Copyright (C) 1995 -- 2015, T. V. Raman

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
# the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

# }}}
# {{{  Site Configuration

##### Site  Configuration #####
MAKE=make
prefix = /usr
# where executables go
bindir = ${prefix}/bin
# where info files should go
infodir = ${prefix}/share/info
# where the emacspeak library directory should go
libparentdir = ${prefix}/share/emacs/site-lisp
# where  all emacspeak  files should go
libdir =$(libparentdir)/emacspeak
#directory where we are building
SRC = $(shell pwd)
INSTALL = install
CP=cp

# }}}
############## no user serviceable parts beyond this point ###################
# {{{ setup distribution

# source files to distribute
ID = README
STUMPWM=stumpwm
TABLE_SAMPLES=etc/tables/*.tab 
FORMS =etc/forms/*.el
MEDIA=media
ECI=servers/linux-outloud
ESPEAK=servers/linux-espeak/tclespeak.cpp \
servers/linux-espeak/Makefile\
servers/linux-espeak/tclespeak.so

OUTLOUD=${ECI}/eci.ini \
${ECI}/*.h \
${ECI}/*.cpp \
${ECI}/ALSA ${ECI}/asoundrc \
${ECI}/atcleci.so ${ECI}/Makefile

NEWS = etc/NEWS*  etc/COPYRIGHT \
etc/remote.txt etc/applications.html   etc/tips.html
SOUNDS=sounds/classic sounds/emacspeak.mp3 \
sounds/pan-chimes  sounds/3d

TCL_PROGRAMS = servers/.servers \
servers/dtk-exp  servers/ssh-dtk-exp\
servers/espeak \
servers/mac \
servers/outloud  servers/ssh-outloud \
servers/tts-lib.tcl \
servers/cloud* servers/log* servers/speech-server
PHANTOM=js/phantom/*.js
ELISP = lisp/*.el lisp/g-client \
lisp/Makefile
TEMPLATES = etc/ etc/Makefile
MISC=etc/extract-table.pl etc/ocr-client.pl \
etc/emacspeak.xpm etc/emacspeak.jpg

INFO = info/Makefile info/*.texi 
XSL=xsl 
DISTFILES =${ELISP}  ${TEMPLATES}     ${TCL_PROGRAMS} ${XSL} \
${OUTLOUD}  ${ESPEAK} \
${PHANTOM} ${STUMPWM} ${INFO}  ${NEWS} ${MISC} Makefile

# }}}
# {{{  User level targets emacspeak info 

emacspeak:
	test -f  lisp/emacspeak-loaddefs.el || ${MAKE} config
	cd lisp; $(MAKE)
	touch   $(ID)
	chmod 644 $(ID)
	@echo "See the NEWS file for a  summary of new features --control e cap n in Emacs"
	@echo "See Emacspeak Customizations for customizations -- control e cap C in Emacs"
	@echo "Use C-h p in Emacs for a package overview"
	@echo "Make sure you read the Emacs info pages"

info:
	cd info; $(MAKE) -k

outloud: 
	cd servers/linux-outloud; $(MAKE) || echo "Cant build Outloud server!"

espeak: 
	cd servers/linux-espeak; $(MAKE) || echo "Cant build espeak server!"

# }}}
# {{{  Maintainance targets tar  dist
GITVERSION=$(shell git show HEAD | head -1  | cut -b 8- )
README: force
	@rm -f README
	@echo "Emacspeak  Revision $(GITVERSION)" > $(ID)
	@echo "Distribution created by `whoami` on `hostname`" >> $(ID)
	@echo "Unpack the  distribution And type make config " >> $(ID)
	@echo "Then type make" >> $(ID)
	@echo "See the Makefile for details. " >> $(ID)

force:

EXCLUDES= --exclude='.git' \
--exclude='*.elc' --exclude='*.o' --exclude='*.so' --exclude='*/.libs'

tar:
	make ${ID}
	tar cvf  emacspeak.tar $(EXCLUDES) $(DISTFILES)   $(ID) \
			  ${TABLE_SAMPLES} ${MEDIA}  ${FORMS} \
	${SOUNDS}

dist: $(DISTFILES)
	$(MAKE) tar

# }}}
# {{{ User level target--  config

config:
	cd etc &&   $(MAKE) config  
	cd lisp && $(MAKE) config
	@echo "Configured emacspeak in directory $(SRC). Now type make emacspeak"

# }}}
# {{{  complete build

#targets
#the complete build
all: emacspeak

#clean, config and build
q:
	make clean
	make config 
	make 

# }}}
# {{{  user level target-- clean

clean:
	cd lisp; $(MAKE) clean
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
# {{{Install: Not Supported

install:
	@echo "Install is not supported by the source distribution."

# }}}
# {{{list distfiles to stdout

list_dist:
	ls -1  $(DISTFILES)

# }}}
# {{{ end of file

#local variables:
#major-mode: makefile-mode
#eval:  (fold-set-marks "# {{{" "# }}}")
#fill-column: 90
#folded-file: t
#end:

# }}}
