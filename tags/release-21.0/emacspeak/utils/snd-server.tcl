# $Id$
# $Author$ 
# Description:A sound server in TCL 
# Keywords:Sounds, TCL
# {{{ LCD Entry: 

# LCD Archive Entry:
# emacspeak| T. V. Raman |raman@crl.dec.com 
# A speech interface to Emacs |
# $date: $ |
#  $Revision$ | 
# Location undetermined
#

# }}}
# {{{ Copyright:  

# Copyright (c) , 1995 by Adobe Systems Incorporated
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
# {{{ procedures  

proc set_sound {filename} {
    global sound
    if {[file exists $filename]} {
        set sound(filename) $filename
    } elseif {[file exists $sound(directory)/$filename]} {
        set sound(filename) $sound(directory)/$filename
    } else {
        error "$filename does not exist. "
    }
    if {[info exists sound(duration)]} {unset sound(duration) }
    set sound(duration) [get_duration]
    if {![string match $sound(count) 0]} {sound_task}
    return $sound(filename)
}

proc get_sound {} {
    global sound
    if {[info exists sound(filename)]} {
        return $sound(filename)
    } else {
        error "No sound defined. "
    }
}

proc get_duration {} {
    global sound
    if {[info exists sound(duration)] } {
        return $sound(duration)
    } else {
        set_duration
    }
}

proc set_duration {} {
    global sound
    if {[info exists sound(filename)]} {
        set size [file size $sound(filename)]
        set sound(duration) [expr $size / 8192.0]
    } else {
        error "First define a sound. "
    }
}

proc play {{count 1}} {
    global sound
    set sound(playing?) 1
    set sound(count) $count 
    sound_task
}

proc stop {} {
    global sound
    set sound(playing?) 0
    set sound(count) 0
}
proc resume {} {
    global sound
    set sound(playing?) 1
    sound_task
}

proc pause {} {
    global sound
    set sound(playing?) 0
}

#end procedure section

# }}}
# {{{sound  task 

proc sound_task {} {
    global  sound
    set sound(playing?) 1
    while {$sound(playing?) } {
        if {![string match $sound(count) 0] } {
            set duration [get_duration]
            catch "exec $sound(play_program) [get_sound] " errCode
            set status [select [list  file0]  {} {} $duration]
            if {[lsearch $status file0]   >=0} {
                set sound(playing?) 0 
                break;
            }
        }  else {
            set sound(playing?) 0
        }
        if {![string match $sound(count) inf]
            && ![string match $sound(count) 0]} {
            incr sound(count) -1
        }
    }
    return $sound(count)
}

# }}}
# {{{ globals

set sound(play_program) "play"
set sound(directory) "/usr/local/sounds"
set sound(playing?) 0
set sound(count) 0
set_sound "ding.au"

# }}}

# {{{ Emacs local variables  

### Local variables:
### major-mode: tcl-mode 
### voice-lock-mode: t
### folded-file: t
### End:

# }}}
