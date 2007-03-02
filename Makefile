# $Id$
# $Author$
# Description:  Makefile for Emacspeak
# Keywords: Emacspeak,  TTS,Makefile
# {{{ LCD Entry:

# LCD Archive Entry:
# emacspeak| T. V. Raman |raman@cs.cornell.edu
# A speech interface to Emacs |
# $Date$ |
#  $Revision$ |
# Location undetermined
#

# }}}
# {{{ Copyright:

#Copyright (C) 1995 -- 2003, T. V. Raman

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
#    make emacspeak
# to compile the files, then (as superuser)
#    make install
# to install them.
#
# By default, files are installed in subdirectories of /usr --
# that is, executables in /usr/bin, .info files in
# /usr/info, and compiled files in /usr/share/lib/emacs/site-lisp/emacspeak.
# If you want them somewhere else, you may add a "prefix=" parameter to the
# make install command.  For example, to place files in subdirectories of
# /usr/local instead of /usr, use this command:
#    make prefix=/usr/local install
#
#Note: The intent is to place emacspeak in a subdirectory of site-lisp.
#Newer emacsuns have this under /usr/local/share (or /usr/share)
#older emacsuns used /usr/local/lib/...
# emacspeak uses tclx --extended tcl-- for the synthesizer server.
# Note:  Extended TCL  --tclx-- is *not* tclsh
# Setting up synthesizer server:
# Emacspeak comes with two servers written in TCL:
# 1) dtk-exp for the Dectalk Express
# 2) dtk-mv for the MultiVoice and older Dectalk 3 synthesizers
# emacspeak uses the shell environment variable DTK_PROGRAM to determine
# which server to use, and the shell environment variable DTK_PORT
# to determine the port where the Dectalk is connected.
# Examples: If using csh or tcsh
#    setenv DTK_PROGRAM "dtk-exp"
# or if using bash
#    export DTK_PROGRAM=dtk-exp
# By default the port is /dev/tty00 on ultrix/osf1, and /dev/ttyS0 on linux.
#
# Finally, make sure that tcl  is present in your search path by typing
#    which tcl
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
# emacspeak.sh that does the same thing as the alias shown above.

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
#for older emacsuns use /usr/lib
libparentdir = ${prefix}/share/emacs/site-lisp
# where  all emacspeak  files should go
libdir =$(libparentdir)/emacspeak
#directory where we are building
SRC = $(shell pwd)
# How to install files
INSTALL = install
CP=cp

# }}}
############## no user servicable parts beyond this point ###################
# {{{ setup distribution

# source files to distribute
ID = README
SAWFISH=sawfish/*.jl sawfish/sawfishrc
UGUIDE=user-guide/*.html user-guide/*.sgml
IGUIDE=install-guide/*.html install-guide/*.sgml
TABLE_SAMPLES=etc/tables/*.tab etc/tables/*.dat etc/tables/*.html
FORMS =etc/forms/*.el
REALAUDIO=realaudio
SHOUTCAST=shoutcast
ECI=servers/linux-outloud
ESPEAK=servers/linux-espeak/tclespeak.cpp \
servers/linux-espeak/Makefile
DTKTTS=servers/software-dtk/tcldtk.c \
servers/software-dtk/DTK \
servers/software-dtk/Makefile

OUTLOUD=${ECI}/eci.ini \
${ECI}/*.cpp \
${ECI}/VIAVOICE ${ECI}/ALSA ${ECI}/asoundrc \
${ECI}/Makefile

NEWS = etc/NEWS*  etc/COPYRIGHT \
etc/remote.txt etc/FAQ etc/HELP etc/applications.html   etc/tips.html
SOUNDS=sounds/default-8k sounds/emacspeak.mp3

TCL_PROGRAMS = servers/.servers \
servers/dtk-exp  servers/ssh-dtk-exp\
servers/dtk-mv \
servers/dtk-soft \
servers/espeak \
servers/outloud  servers/ssh-outloud \
servers/dsp-outloud \
servers/tts-lib.tcl \
servers/remote-tcl \
servers/speech-server
ELISP = lisp/*.el \
lisp/atom-blogger \
lisp/g-client \
lisp/xml-forms/*.xml \
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
${SAWFISH} ${OUTLOUD} ${DTKTTS} \
${INFO} ${UGUIDE} ${IGUIDE} ${NEWS} ${MISC} Makefile

# }}}
# {{{  User level targets emacspeak info print

emacspeak:
	test -f  lisp/emacspeak-loaddefs.el || make config
	cd lisp; $(MAKE)  SRC=$(SRC)
	touch   $(ID)
	chmod 644 $(ID)
	@echo "Now check installation of  the speech server. "
	@echo "See Makefile for instructions."
	@echo "See the NEWS file for a  summary of new features --control e cap n in Emacs"
	@echo "See the FAQ for Frequently Asked Questions -- control e cap F in Emacs"
	@echo "See Emacspeak Customizations for customizations -- control e cap C in Emacs"
	@echo "Use C-h p in Emacs for a package overview"
	@echo "Make sure you read the Emacs info pages"

info:
	cd info; $(MAKE) -k

print:
	@echo "Please change to the info directory and type make print"

# }}}
# {{{  Maintainance targets tar  dist



README: force
	@rm -f README
	@echo "Emacspeak  Revision `svnversion .`" > $(ID)
	@echo "Distribution created by `whoami` on `hostname`" >> $(ID)
	@echo "Unpack the  distribution And type make config " >> $(ID)
	@echo "Then type make" >> $(ID)
	@echo "See the Makefile for details. " >> $(ID)


force:

EXCLUDES=--exclude='*/CVS' --exclude='*/.svn' \
--exclude='*.o' --exclude='*.so' --exclude='*/.libs'

tar:
	make ${ID}
	tar cvf  emacspeak.tar $(EXCLUDES) $(DISTFILES)   $(ID) \
			 ${TABLE_SAMPLES} ${REALAUDIO} ${SHOUTCAST} ${FORMS} \
	${SOUNDS}

dist: $(DISTFILES)
	$(MAKE) tar

# }}}
# {{{ User level target--  config

config:
	cd etc; $(MAKE) config  SRC=$(SRC)
	cd lisp; $(MAKE) config
	@echo "Configured emacspeak in directory $(SRC). Now type make emacspeak"

# }}}
# {{{  user level target-- install uninstall

install:
	$(MAKE) config SRC=$(libdir)
	  $(INSTALL)  -d $(libparentdir)
	  $(INSTALL) -d $(libdir)
	touch $(libdir)/.nosearch
	  $(INSTALL) -d $(libdir)/lisp
	$(INSTALL) -d $(libdir)/lisp/xml-forms
	$(INSTALL) -d $(libdir)/lisp/atom-blogger
	$(INSTALL) -d $(libdir)/etc
	$(INSTALL) -d $(libdir)/sawfish
	$(INSTALL) -d $(libdir)/xsl
	$(INSTALL) -d $(libdir)/user-guide
	$(INSTALL) -d $(libdir)/install-guide
	$(INSTALL) -m 0644  ${ID} $(libdir)
	  $(INSTALL) -m 0644  lisp/*.el lisp/*.elc  $(libdir)/lisp
	$(INSTALL) -m 0644  lisp/xml-forms/*.xml   $(libdir)/lisp/xml-forms
	$(INSTALL) -m 0644  lisp/atom-blogger/*.el    $(libdir)/lisp/atom-blogger
	$(INSTALL) -m 0644  lisp/atom-blogger/*.xsl    $(libdir)/lisp/atom-blogger
	$(INSTALL) -m 0644  sawfish/*.jl sawfish/sawfishrc   $(libdir)/sawfish
	$(INSTALL) -m 0644  xsl/*.xsl    $(libdir)/xsl
	$(INSTALL) -m 0644  ${UGUIDE}   $(libdir)/user-guide
	$(INSTALL) -m 0644  ${IGUIDE}   $(libdir)/install-guide
	$(INSTALL) -d $(libdir)/sounds
	$(INSTALL) -d $(libdir)/servers
	$(INSTALL) -d $(libdir)/servers/linux-outloud
	$(INSTALL)  -m 755 ${OUTLOUD}  $(libdir)/servers/linux-outloud
	$(INSTALL) -d $(libdir)/servers/linux-espeak
	$(INSTALL)  -m 755 ${ESPEAK}  $(libdir)/servers/linux-espeak
	$(INSTALL) -d $(libdir)/servers/software-dtk
	$(INSTALL)  -m 755 ${DTKTTS}  $(libdir)/servers/software-dtk
	$(INSTALL)  -m 755 ${TCL_PROGRAMS}  $(libdir)/servers
	$(INSTALL) -m 0644   ${NEWS}   $(libdir)/etc
	cp   ${MISC}   $(libdir)/etc
	$(CP) -r $(SOUNDS) $(libdir)/sounds
	chmod -R go+rX  $(libdir)/sounds
	$(CP) -r $(REALAUDIO) $(libdir)
	chmod -R go+rX  $(libdir)/realaudio
	$(CP) -r $(SHOUTCAST) $(libdir)
	chmod -R go+rX  $(libdir)/shoutcast
	$(INSTALL) -d $(libdir)/etc/forms
	$(INSTALL)  -m 0644 $(FORMS) $(libdir)/etc/forms
	$(INSTALL) -d $(libdir)/etc/tables
	$(INSTALL)  -m 0644 $(TABLE_SAMPLES) $(libdir)/etc/tables
	$(INSTALL) -d $(bindir)
	$(INSTALL) -m 0755  etc/emacspeak.sh $(bindir)/emacspeak
	$(INSTALL) -d $(infodir)
	cd info; \
	$(MAKE) install infodir=$(infodir)

uninstall:
	rm -rf $(infodir)/emacspeak.info* $(bindir)/emacspeak
	  rm -rf $(libdir)


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

label: $(DISTFILES)
	rm -f lisp/emacspeak-loaddefs.el lisp/cus-load.el
	svn commit  -m "${MSG}"
	svn cp https://emacspeak.googlecode.com/svn/trunk https://emacspeak.googlecode.com/svn/tags/release-${LABEL}

release: #supply LABEL=NN.NN
	$(MAKE) label LABEL=$(LABEL) MSG="Releasing version ${LABEL}"
	$(MAKE) dist
	mkdir release; \
	mv emacspeak.tar release; \
	cd release; \
	mkdir emacspeak-$(LABEL); \
	cd emacspeak-$(LABEL); \
	tar xvf ../emacspeak.tar ; \
	chmod 644 emacspeak-finder-inf.el ;\
	cd ..; \
	rm -f emacspeak.tar ; \
	tar cvf emacspeak.tar emacspeak-$(LABEL); \
	bzip2  -9 emacspeak.tar; \
	mv  emacspeak.tar.bz2 ../; \
	cd .. ; \
	/bin/rm -rf release ; \
	rm -f emacspeak.spec ; \
sed "s@<version>@$(LABEL)@g" \
	emacspeak.spec.in > emacspeak.spec
	@echo "Prepared Emacspeak-$(LABEL) in emacspeak.tar.bz2"

# }}}
# {{{ rpm

rpm: emacspeak.spec
	rpm --erase emacspeak || echo ""
	/bin/rm -rf /usr/share/emacs/site-lisp/emacspeak || echo ""
	@cp emacspeak.tar.bz2 /usr/src/redhat/SOURCES/
	@cp emacspeak.spec /usr/src/redhat/SPECS/
	rpmbuild  -ba --target noarch --sign --clean   /usr/src/redhat/SPECS/emacspeak.spec

# }}}
# {{{list distfiles to stdout

list_dist:
	ls -1  $(DISTFILES)

# }}}
# {{{upload to sourceforge

TARBALL=emacspeak-${LABEL}.tar.bz2
RPM=/usr/src/redhat/RPMS/i386/emacspeak-${LABEL}-1.i386.rpm
SF_HOME='raman@emacspeak.sf.net:~/www-emacspeak/htdocs'
sourceforge:
	mv emacspeak.tar.bz2 ${TARBALL}
	( echo 'anonymous';			     \
	  echo prompt;				  \
	  echo hash;				    \
	  echo cd incoming;			     \
	  echo put ${TARBALL};		  \
	  echo put ${RPM};	      \
	  echo quit ) | /usr/bin/ftp upload.sourceforge.net
	scp ${RPM} ${TARBALL} ${SF_HOME}

# }}}
# {{{ end of file

#local variables:
#major-mode: makefile-mode
#eval:  (fold-set-marks "# {{{" "# }}}")
#fill-column: 90
#folded-file: t
#end:

# }}}
