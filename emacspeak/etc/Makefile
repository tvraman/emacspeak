# $Id$
# $Author$ 
# Description:  Makefile for Emacspeak 
# Keywords: Emacspeak, Dectalk, Makefile 
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

#Copyright (C) 1995 -- 2002, T. V. Raman 

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
# {{{xml -> html 
%.html: %.xml
	xsltproc $< > $@

# }}}
# {{{ User level target--  config 
config:
	@rm -f    emacspeak.sh
	@sed "s@<emacspeak-dir>@$(SRC)/lisp@g" \
		emacspeak.sh.def > emacspeak.sh
	@chmod 755   emacspeak.sh

# }}}
# {{{maintainer target 

pub: tips.html applications.html
	scp tips.html applications.html   raman@emacspeak.sourceforge.net:~/www-emacspeak/htdocs 
# }}}
# {{{ end of file 

#local variables: 
#major-mode: makefile-mode
#eval:  (fold-set-marks "# {{{" "# }}}")
#fill-column: 90
#folded-file: t
#end:

# }}}
