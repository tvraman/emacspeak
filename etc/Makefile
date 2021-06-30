# $Id$
# $Author: tv.raman.tv $ 
# Description:  Makefile for Emacspeak 
# Keywords: Emacspeak, Dectalk, Makefile 
# {{{ LCD Entry: 

# LCD Archive Entry:
# emacspeak| T. V. Raman |raman@cs.cornell.edu 
# A speech interface to Emacs |
# $Date: 2006-08-11 12:11:17 -0700 (Fri, 11 Aug 2006) $ |
#  $Revision: 4047 $ | 
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
# the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

# }}}
all: #fix perms
	chmod 755 *.pl
# {{{xml -> html 
tips.html: tips.xml tips.xsl
	xsltproc tips.xsl tips.xml > tips.html 
%.html: %.xml
	xsltproc    $< > $@

# }}}
# {{{ User level target--  config 
config:
	@chmod 755   *.pl

# }}}
# {{{maintainer target 

pub: tips.html applications.html
	scp tips.html applications.html    raman,emacspeak@web.sourceforge.net:/home/groups/e/em/emacspeak/htdocs
	cp applications.html tips.html ../gh-emacspeak/
# }}}
# {{{ end of file 

#local variables: 
#mode: makefile
#fill-column: 90
#folded-file: t
#end:

# }}}
