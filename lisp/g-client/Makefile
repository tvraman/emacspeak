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
# How to compile
%.elc:  %.el
	$(EMACS) $(BATCH)  $(DEPS)  $(COMPILE) $<

OBJECTS= g-auth.elc \
g-utils.elc \
g.elc \
gblogger.elc \
gcal.elc \
greader.elc

# }}}
# {{{  User level targets all

all: $(OBJECTS)

# }}}
# {{{build time target: config

config: g-loaddefs.el g-cus-load.el


force:

g-cus-loads.el: force
g-loaddefs.el: force

.phony: g-cus-loads.el
.phony: g-loaddefs.el

g-loaddefs.el:
	echo ";;;Auto generated" > g-loaddefs.el
	$(EMACS) $(BATCH) -l ./g-autogen.el -f g-autogen-generate-autoloads

g-cus-load.el:
	$(EMACS) $(BATCH) -l ./g-autogen.el -f g-autogen-custom-make-dependencies "."

# }}}
# {{{  Make rules for the various modules
g-utils.elc: g-utils.el g-loaddefs.el
g.elc: g.el g-loaddefs.el
g-auth.elc: g-auth.el g-utils.elc
greader.elc: greader.el g-auth.elc
gcal.elc: gcal.el g-auth.elc
gblogger.elc: gblogger.el g-auth.elc

# }}}
# {{{dist

dist:g-client.tar.bz2

g-client.tar.bz2: ${SOURCES}
	rm -f $@
	tar  cfj $@ -X .excludes -C ..  g-client

# }}}
# {{{  user level target-- clean

clean:
	rm -f *.elc   g-cus-loads.el g-loaddefs.el

# }}}
# {{{ end of file

#local variables:
#major-mode: makefile-mode
#eval:  (fold-set-marks "# {{{" "# }}}")
#fill-column: 90
#folded-file: t
#end:

# }}}
