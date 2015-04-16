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
# {{{ Installation instructions:

# If you're reading this, then you've already unpacked the tar archive
# and extracted the sources in a directory.
# cd to the  directory where you placed the sources.
# This directory is referred to henceforth as EMACSPEAK_DIR.
# and then type
#    make config
# Now type
#    make 
# to compile the files, then 
#    sudo make install
# to install them.
#
# By default, files are installed in subdirectories of /usr --
# that is, executables in /usr/bin, .info files in
# /usr/share/info, and elisp  files in /usr/share/emacs/site-lisp/emacspeak.
# If you want them somewhere else, you may add a "prefix=" parameter to the
# make install command.  For example, to place files in subdirectories of
# /usr/local instead of /usr, use this command:
#    sudo make prefix=/usr/local install
#
# emacspeak uses tclx --extended tcl-- for the speech server.
# Note:  Extended TCL  --tclx-- is *not* tclsh
# Setting up speech server:
# Emacspeak comes with two servers written in TCL:
# 1) dtk-exp for the Dectalk Express
#2 outloud --- for ViaVoice outloud
# 3 espeak for Espeak
# emacspeak uses the shell environment variable DTK_PROGRAM to determine
# which server to use, and the shell environment variable DTK_PORT
# to determine the port where the Dectalk is connected.
# Examples: If using csh or tcsh
#    setenv DTK_PROGRAM "dtk-exp"
# or if using bash
#    export DTK_PROGRAM=dtk-exp
# By default the port is /dev/tty00 on ultrix/osf1, and /dev/ttyS0 on linux.
#
# Finally, make sure that tclsh  is present in your search path by typing
#    which tclsh
# Assuming you're using dtk-exp:
# Check that the dtk-exp can be run by typing
# <emacspeak-dir>/dtk-exp
# You should hear the Dectalk speak and get a TCL prompt if everything is okay.
# Next, check that your serial port is working correctly, and that your stty
# settings are correct. You can do this by executing the following sequence
# of TCL commands in the TCL session you just started:
#q {this is a test. }; d
# should speak the text within the braces.
#    s
# The above command stops speech.
# You should see a TCL prompt when you execute it.
# If things appear to hang when you execute tts_stop
# i.e. you don't see a TCL prompt (%) then
# a) The serial cable conecting your speech device is flaky
# b) Your serial port is flaky
# c) The stty settings on the port are incorrect for your
# system
#In the case of (c) on solaris systems,
#try setting environment variable DTK_OS to solaris.
# In the case of (c) please report the problem
# quit this tcl session by typing ctrl-d
#
# To use emacspeak you can do one of the following:
# Add the line
# (load-file (expand-file-name "<EMACSPEAK_DIR>/emacspeak-setup.el"))
# to the start of your .emacs
# This will start emacspeak every time you use emacs
# or alternatively set the following alias.
# If you use csh or tcsh
# alias emacspeak "emacs -q -l <EMACSPEAK_DIR>/emacspeak-setup.el -l $HOME/.emacs"
# If you use bash (the default under linux)
# alias emacspeak="emacs -q -l <EMACSPEAK_DIR>/emacspeak-setup.el -l $HOME/.emacs"
# Note: in all of the above you should replace <EMACSPEAK_DIR> with your
# site-specific value. The distribution also creates a shell executable
# The now deprecated emacspeak.sh that does the same thing as the alias shown above.

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
# How to install files
INSTALL = install
CP=cp

# }}}
############## no user serviceable parts beyond this point ###################
# {{{ setup distribution

# source files to distribute
ID = README
STUMPWM=stumpwm
TABLE_SAMPLES=etc/tables/*.tab etc/tables/*.dat etc/tables/*.html
FORMS =etc/forms/*.el
MEDIA=media
ECI=servers/linux-outloud
ESPEAK=servers/linux-espeak/tclespeak.cpp \
servers/linux-espeak/Makefile\

DTKTTS=servers/software-dtk/tcldtk.c \
servers/software-dtk/DTK \
servers/software-dtk/Makefile
OUTLOUD=${ECI}/eci.ini \
${ECI}/*.h \
${ECI}/*.cpp \
${ECI}/VIAVOICE ${ECI}/ALSA ${ECI}/asoundrc \
${ECI}/Makefile

NEWS = etc/NEWS*  etc/COPYRIGHT \
etc/remote.txt etc/applications.html   etc/tips.html
SOUNDS=sounds/default-8k sounds/emacspeak.mp3 \
sounds/cartoon-22k-mono sounds/chimes-stereo  sounds/3d

TCL_PROGRAMS = servers/.servers \
servers/dtk-exp  servers/ssh-dtk-exp\
servers/dtk-soft \
servers/espeak \
servers/mac \
servers/outloud  servers/ssh-outloud servers/32-outloud \
servers/tts-lib.tcl \
servers/cloud* servers/log* servers/speech-server
ELISP = lisp/*.el \
lisp/g-client \
lisp/Makefile
TEMPLATES = etc/emacspeak.sh.def etc/Makefile
MISC=etc/extract-table.pl etc/last-log.pl \
etc/pdf2text etc/doc2text \
etc/xls2html etc/ppt2html  \
etc/ocr-client.pl \
etc/emacspeak.xpm etc/emacspeak.jpg

INFO = info/Makefile info/*.texi info/add-css.pl
XSL=xsl
DISTFILES =${ELISP}  ${TEMPLATES}     $(TCL_PROGRAMS) ${XSL} \
${OUTLOUD} ${DTKTTS} ${ESPEAK} \
${STUMPWM} ${INFO}  ${NEWS} ${MISC} Makefile

# }}}
# {{{  User level targets emacspeak info 

emacspeak:
	test -f  lisp/emacspeak-loaddefs.el || ${MAKE} config
	cd lisp; $(MAKE)
	touch   $(ID)
	chmod 644 $(ID)
	@echo "Now check installation of  the speech server. "
	@echo "See Makefile for instructions."
	@echo "See the NEWS file for a  summary of new features --control e cap n in Emacs"
	@echo "See Emacspeak Customizations for customizations -- control e cap C in Emacs"
	@echo "Use C-h p in Emacs for a package overview"
	@echo "Make sure you read the Emacs info pages"

info:
	cd info; $(MAKE) -k

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

EXCLUDES=--exclude='.git' --exclude='*.o' --exclude='*.so' --exclude='*/.libs'

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
# {{{  user level target-- install uninstall
# We install both  elisp sources and the resulting .elc files

install:
	$(MAKE) config 
	  $(INSTALL)  -d $(DESTDIR)$(libparentdir)
	  $(INSTALL) -d $(DESTDIR)$(libdir)
	  $(INSTALL) -d $(DESTDIR)$(libdir)/lisp
	$(INSTALL) -d $(DESTDIR)$(libdir)/lisp/g-client
	$(INSTALL) -d $(DESTDIR)$(libdir)/etc
	$(INSTALL) -d $(DESTDIR)$(libdir)/xsl
	$(INSTALL) -m 0644  ${ID} $(DESTDIR)$(libdir)
	  $(INSTALL) -m 0644  lisp/*.el lisp/*.elc  $(DESTDIR)$(libdir)/lisp
	$(INSTALL) -m 0644  lisp/g-client/*.el    $(DESTDIR)$(libdir)/lisp/g-client
	$(INSTALL) -m 0644  lisp/g-client/*.elc    $(DESTDIR)$(libdir)/lisp/g-client
	$(INSTALL) -m 0644  lisp/g-client/*.xsl    $(DESTDIR)$(libdir)/lisp/g-client
	$(INSTALL) -m 0644  xsl/*.xsl    $(DESTDIR)$(libdir)/xsl
	$(INSTALL) -d $(DESTDIR)$(libdir)/sounds
	$(INSTALL) -d $(DESTDIR)$(libdir)/servers
	$(INSTALL) -d $(DESTDIR)$(libdir)/servers/linux-outloud
	$(INSTALL)  -m 755 ${OUTLOUD}  $(DESTDIR)$(libdir)/servers/linux-outloud
	$(INSTALL) -d $(DESTDIR)$(libdir)/servers/linux-espeak
	$(INSTALL)  -m 755 ${ESPEAK}  $(DESTDIR)$(libdir)/servers/linux-espeak
	$(INSTALL) -d $(DESTDIR)$(libdir)/servers/software-dtk
	$(INSTALL)  -m 755 ${DTKTTS}  $(DESTDIR)$(libdir)/servers/software-dtk
	$(INSTALL)  -m 755 ${TCL_PROGRAMS}  $(DESTDIR)$(libdir)/servers
	$(INSTALL) -m 0644   ${NEWS}   $(DESTDIR)$(libdir)/etc
	cp   ${MISC}   $(DESTDIR)$(libdir)/etc
	$(CP) -r $(SOUNDS) $(DESTDIR)$(libdir)/sounds
	chmod -R go+rX  $(DESTDIR)$(libdir)/sounds
	$(CP) -r $(MEDIA) $(DESTDIR)$(libdir)
	chmod -R go+rX  $(DESTDIR)$(libdir)/media
	$(CP) -r $(STUMPWM) $(DESTDIR)$(libdir)
	chmod -R go+rX  $(DESTDIR)$(libdir)/stumpwm	
	$(INSTALL) -d $(DESTDIR)$(libdir)/etc/forms
	$(INSTALL)  -m 0644 $(FORMS) $(DESTDIR)$(libdir)/etc/forms
	$(INSTALL) -d $(DESTDIR)$(libdir)/etc/tables
	$(INSTALL)  -m 0644 $(TABLE_SAMPLES) $(DESTDIR)$(libdir)/etc/tables
	$(INSTALL) -d $(DESTDIR)$(bindir)
	$(INSTALL) -m 0755  etc/emacspeak.sh $(DESTDIR)$(bindir)/emacspeak
	$(INSTALL) -d $(DESTDIR)$(infodir)
	cd info; \
	$(MAKE) install DESTDIR="$(DESTDIR)" infodir="$(infodir)"

uninstall:
	rm -rf $(infodir)/emacspeak.info* $(bindir)/emacspeak $(libdir)
# }}}
# {{{  complete build

#targets
#the complete build
all: emacspeak

# }}}
# {{{  user level target-- clean

clean:
	cd lisp; $(MAKE) clean
		cd info; $(MAKE) clean

# }}}
# {{{ labeling releases

#label  releases when ready
LABEL=
MSG="Releasing ${LABEL}"
release: #supply LABEL=NN.NN
	git tag -a  ${LABEL} -m "Tagging release with ${LABEL}"
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
	./utils/ghr ${LABEL} "emacspeak-${LABEL}.tar.bz2"

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
