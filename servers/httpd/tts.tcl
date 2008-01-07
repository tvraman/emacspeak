#!/usr/bin/tcl
# $Id: speech-server 4725 2007-07-02 02:12:26Z tv.raman.tv $
# Description:  Speech server
#Usage: tcl <tbd>
#Example: <tbd>
# Keywords: Emacspeak, Speech, TCL HTTP 
# {{{ LCD Entry: 

# LCD Archive Entry:
# emacspeak| T. V. Raman |raman@cs.cornell.edu 
# A speech interface to Emacs |
# $Date: 2007-07-01 19:12:26 -0700 (Sun, 01 Jul 2007) $ |
#  $Revision: 4725 $ | 
# Location undetermined
#

# }}}
# {{{ Copyright:  
#Copyright (C) 1995 -- 2001, T. V. Raman 
# Incorporated.
#All Rights Reserved
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
# {{{Introduction 

# }}}
# {{{HTTP Binding 

namespace eval ::tts {}
# The URLs under / are implemented by procedures that begin with "::tts::"

Direct_Url /	::tts::

# }}}
# {{{HTTP Request Handlers  

# ::tts::/ --
#
#	This implements /
# Here for testing -- will go away.

proc ::tts::/ {{user none}} {
    return $user
}

# ::tts::/say --
#
#	This implements /tts/say

proc ::tts::/say {text} {
    return 0
}


# }}}

# {{{ Emacs local variables  

### Local variables:
### major-mode: tcl-mode 
### voice-lock-mode: t
### folded-file: t
### End:

# }}}
