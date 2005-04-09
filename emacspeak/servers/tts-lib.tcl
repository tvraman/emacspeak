#$Id$
# {{{ LCD Entry: 
#x
# LCD Archive Entry:
# emacspeak| T. V. Raman |raman@cs.cornell.edu
# A speech interface to Emacs |
# $Date$ |
#  $Revision$ | 
# Location undetermined
#

# }}}
# {{{ Copyright:

#x
#Copyright (C) 1995 -- 2003, T. V. Raman 
#All Rights Reserved
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
#

# }}}
# {{{ sync state 
proc tts_sync_state {punct capitalize allcaps splitcaps rate} {
    tts_set_punctuations  $punct
    tts_capitalize  $capitalize
    tts_allcaps_beep  $allcaps
    tts_split_caps   $splitcaps
    tts_set_speech_rate  $rate
} 
# }}}
# {{{ queue:

#currently we use an inlined version of this test in speech_task

proc queue_empty? {} {
    global tts
    expr $tts(q_head) == $tts(q_tail)
}

proc queue_nonempty? {} {
    global tts
    expr $tts(q_head) != $tts(q_tail)
}

proc queue_length {} {
    global tts
    expr $tts(q_tail) - $tts(q_head)
}

proc queue_clear {} {
    global tts queue
    if {$tts(debug)} {
    puts -nonewline  $tts(write) "$tts(q_head) e\013"
    }
    if {[info exists q]} unset q
    set queue(-1) "" 
    set tts(q_head) 0
    set tts(q_tail) 0 
    return ""
}

#formerly called queue_speech --queue speech event

proc q {{element ""}} {
    global queue tts env
    if {[string length element]} {
        set queue($tts(q_tail)) [list s $element]
        incr tts(q_tail)
        if {$tts(midi) == 1} {
            set mod [expr ($tts(q_tail) - $tts(q_head)) % 50]
            if {$mod == 0}   {
                note 1 60 .5
            }
        }
        return ""
    }
}

#queue a note 
proc n {instrument note length {target 0} {step 5}} {
    global queue tts env
    set queue($tts(q_tail)) [list n $instrument $note \
                             $length $target $step]
    incr tts(q_tail)
    return ""
}


#queue a beep 
proc b {{pitch 523} {length 100} {repeat 1} {duration 50}} {
    global queue tts 
    set queue($tts(q_tail)) [list b $pitch $length $repeat $duration]
    incr tts(q_tail)
    return ""
}

#queue a sound event

proc a {sound} {
    global queue tts
    set queue($tts(q_tail)) [list a $sound]
    incr tts(q_tail)
    return ""
}

proc queue_rewind {} {
    global tts queue 
    if {$tts(q_head) == 0} {return ""}
    set tts(q_head)  0
    set element  $queue($tts(q_head))
    return $element
}

proc queue_retreat {{step 1}} {
    global tts queue 
    if {$tts(q_head) == 0} {return ""}
    incr tts(q_head) [expr - $step]
    set tts(q_head)  [expr max ($tts(q_head), 0)]
    set element  $queue($tts(q_head))
    return $element
}

proc queue_advance {{step 1}} {
    global tts queue 
    incr tts(q_head) $step
    set tts(q_head)  [expr min ($tts(q_head), $tts(q_tail))]
    set element  $queue($tts(q_head))
    return $element
}

proc queue_remove {} {
    global tts queue 
    set element  $queue($tts(q_head))
    incr tts(q_head)
    return $element
}

proc queue_backup {} {
    global tts  backup queue
    if {[queue_empty?]} {
set tts(backup_head) 0
    set tts(backup_tail) 0
        return
    }
    unset backup
    set backup(-1) ""
    set head [expr  max($tts(q_head) - 2, 0)]
    set tail $tts(q_tail)
    loop i $head $tail 1 {
        set backup($i) $queue($i)
    }
    set tts(backup_head) $head
    set tts(backup_tail) $tail
}

proc queue_restore {} {
    global tts  backup queue
    unset queue
    set queue(-1) ""
    set head $tts(backup_head)
    set tail $tts(backup_tail)
    loop i $head $tail 1 {
        set queue($i) $backup($i)
    }
    set tts(q_head) $head
    set tts(q_tail) $tail
}

# }}}
# {{{sounds: 

#play a sound over the server
proc p {sound} {
    global tts
    catch "exec $tts(play) $sound 2>&1 >   /dev/null &" errCode
    speech_task
}

# }}}
# {{{notes: 

#Simple notes player
#Uses stdio music player 

proc note {i p d {target 0} {step 5}} {
    global tts
    set f $tts(notes)
    if {$target == 0} {
        puts $f "n $i $p 127"
        select {} {} {} $d
        puts $f "x $i $p 127"
    } else {
        loop freq $p $target $step {
            puts $f "n $i $freq 127"
        }
        select {} {} {} $d
        loop freq $p $target $step {
            puts $f "x $i $freq 127"
        }
    }
    return 0
}
proc notes_shutdown  {} {
    global tts
    if {$tts(midi) == 0} return
    set notes $tts(notes)
    puts $notes "q\n"
    close $tts(notes)
    set tts(midi) 0
}

proc notes_initialize {} {
    global tts
    if {[info exists tts(midi)]
        && $tts(midi) == 1}  {
        puts stderr "Notes already initialized "
        return 1
    }
    set tts(midi) 0
    if {![file executable /usr/bin/stdiosynth]} {
        return
    }
    set result [catch {set tts(notes) [open "|stdiosynth " w]} err]
    if {$result != 0}  {
        puts stderr "$err: Notes not initialized --unable to start stdiosynth"
        return
    }
    fcntl $tts(notes) nobuf 1 
    set result [catch {note 1 60 .1 } err]
    if {$result == 0} {
        set tts(midi) 1
    } else {
        puts stderr "Error playing test note "
        set tts(midi) 0
    }
}

# }}}
# {{{beep  

#you need to have beep installed 

proc beep_initialize {} {
    global tts
    if {[file executable /usr/bin/beep]} {
        set tts(beep) 1
    }
}

proc beep {{freq 523} {length 100} {repeat 1} {delay 10}} {
    global tts
    if {[info exists tts(beep)]
        && $tts(beep) == 1}  {
        exec beep -f $freq -l $length -r $repeat -d $delay &
    }
}

# }}}
# {{{self test 

proc tts_selftest {} {
     loop i 1 10 {
     q "This is test $i. "
     }
     d
}


# }}}
# {{{guessing os   and port 

proc which_os {} {
global env
     #if env variable DTK_OS is set, use it;
     if {[info exists env(DTK_OS)] } {
     return  $env(DTK_OS)
     } 
     set machine [exec uname -a]
     #os hostname version 
     set fields [split $machine ]
     set os [lindex $fields 0]
     set host [lindex $fields 1]
     set version [lindex $fields 2]    
     switch -exact  -- $os {
     ULTRIX  -
     OSF1  {return DEC}
     SunOS {
     #are we  solaris
     if {[string match 5.* $version] }  {
     return Solaris
     } else    {
     #we are sunos 4
     return SunOS
     }
     }
     Linux -
     default    {
     return Linux
     }
     }
     }

proc which_port {{os Linux}} {
     global env
    if {[info exists env(DTK_PORT)] } {
    set port $env(DTK_PORT)
    puts stdout "Set port to $port"
    } else {
    switch -exact  -- $os {
           DEC {
           set port /dev/tty00
           }
           SunOS -
           Solaris -
           solaris {
           set port /dev/ttya
           } 
           Linux -
           default {
           set port /dev/ttyS0
           }
           }
    }
    return $port
}

# }}}
# {{{tts setserial 

proc tts_setserial {} {
    global tts
    set machine [which_os]
    set port [which_port $machine]
    set tts(read)  [open $port  r]
    set tts(write)  [open $port  w]
    #set up stty settings 
    switch -exact  -- $machine {
        DEC { #osf and ultrix
            exec stty sane 9600 raw  -echo < $port 
            exec stty ixon ixoff  <  $port 
        }
        solaris -
        Solaris {
            exec /usr/bin/stty sane 9600 raw  < $port 
            exec /usr/bin/stty -echo <  $port 
            exec /usr/bin/stty ignpar <  $port 
            exec   /usr/bin/stty ixon ixoff < $port 
        }
        SunOS   {
            exec stty sane 9600 raw  -echo -echoe -echoke echoctl  > $port 
            exec stty ixon ixoff  >  $port 
        }
        Linux -
        default   {
            exec stty sane 9600 raw  -echo <  $port 
            exec stty -echo <  $port 
            exec stty ixon ixoff  < $port 
        }
    }
    
    #set up the right kind of buffering:
    fcntl $tts(read) nobuf 1
    fcntl $tts(write) nobuf 1
}

# }}}
# {{{tts initialize  

proc tts_initialize {} {
    global tts backup  queue
    #split caps flag: 
    set tts(split_caps) 1
    # Capitalize flag
    set tts(capitalize)  0
    #allcaps beep flag
    set tts(allcaps_beep)  0
    set tts(talking?) 0
    set tts(char_factor)  1.2
    set tts(q_head)  0
    set tts(q_tail) 0
    set tts(backup_head)  0
    set tts(backup_tail) 0
    set tts(punctuations) some
    set queue(-1) ""
    set backup(-1) ""
    #play program
    if {[info exists env(EMACSPEAK_PLAY_PROGRAM)] } {
        set tts(play)  $env(EMACSPEAK_PLAY_PROGRAM)
    } else {
        set tts(play) "play"
    }
    
    #optional debuggin output
    if {[info exists env(DTK_DEBUG)] } {
        set tts(debug) 1
    } else {
        set tts(debug) 0
    }
    
    #flag to avoid multiple consecutive stops
    set tts(not_stopped) 1
}

# }}}
# {{{ Emacs local variables  

### Local variables:
### major-mode: tcl-mode 
### voice-lock-mode: t
### folded-file: t
### End:

# }}}
