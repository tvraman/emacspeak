# $Id: Makefile 4373 2007-02-17 03:59:51Z tv.raman.tv $
# $Author: tv.raman.tv $
# Description:  Makefile for g-client -- An Emacs client for Google services
# Keywords: Emacs, G-CLIENT, Makefile
# {{{ LCD Entry:

# LCD Archive Entry:
# g-client| T. V. Raman |raman@cs.cornell.edu
# An emacs  interface to Google services |
# $Date: 2007-02-16 19:59:51 -0800 (Fri, 16 Feb 2007) $ |
#  $Revision: 4373 $ |
# Location undetermined
#

# }}}
# {{{ Copyright:

#Copyright (C) 2006--2007, T. V. Raman
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
# {{{make definitions

# what emacs is called on your system
EMACS = emacs
# How to run in batch mode
BATCH = -batch -q -no-site-file   
DEPS= -l ./g-load-path.el -l ./g-loaddefs.el
COMPILE =  -f batch-byte-compile
OBJECTS= g-utils.elc \
g.elc \
gweb.elc \
gmaps.elc \
gm-nnir.elc \


MODULES=\
  g-utils.el g.el gweb.el gmaps.el gm-nnir.el 

# How to compile
%.elc:  %.el
	$(EMACS) $(BATCH)  $(DEPS)  $(COMPILE) $<

# }}}
# {{{build time target: config, all

all: ${MODULES} g-loaddefs.el
	@make config 
	@make $(OBJECTS)

config: g-cus-load.el g-loaddefs.el

g-loaddefs.el:${MODULES}
	@echo ";;;Auto generated" > g-loaddefs.el
	$(EMACS) $(BATCH) -l ./g-autogen.el -f g-autogen-generate-autoloads

g-cus-load.el:${MODULES}
	$(EMACS) $(BATCH) -l ./g-autogen.el -f g-autogen-custom-make-dependencies "."

# }}}
# {{{  Make rules for the various modules

g-utils.elc: g-utils.el
g.elc: g.el  
gweb.elc: gweb.el g-utils.elc
gmaps.elc: gmaps.el g-utils.elc
gm-nnir.elc: gm-nnir.el 
# }}}
# {{{dist

dist:../g-client.tar.bz2

../g-client.tar.bz2: 
	rm -f $@
	tar  cfj $@ -X .excludes -C ..  g-client

# }}}
# {{{  user level target-- clean

clean:
	rm -f *.elc   g-cus-loads.el g-loaddefs.el

# }}}
# {{{dev targets:

indent:
	emacs -batch -q -no-site-file -l ../../utils/indent-files.el

# }}}
# {{{ end of file

#local variables:
#mode: makefile
#eval:  (fold-set-marks "# {{{" "# }}}")
#fill-column: 90
#folded-file: t
#end:

# }}}
