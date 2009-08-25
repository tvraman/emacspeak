#$Id:$
# TVR: Pruning out cacheing etc to create HTTP TTS binding
# Cloned from minihttpd.tcl from tclhttpd 3.5.1
# Simple Sample httpd/1.0 server in 250 lines of Tcl
# Stephen Uhler / Brent Welch (c) 1996 Sun Microsystems
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

# This is a working sample httpd server written entirely in TCL with the
# CGI and imagemap capability removed.  It has been tested on the Mac, PC
# and Unix.  It is intended as sample of how to write internet servers in
# Tcl. This sample server was derived from a full-featured httpd server,
# also written entirely in Tcl.
# Comments or questions welcome (stephen.uhler@sun.com)

# Httpd is a global array containing the global server state
#  root:	the root of the document directory
# TVR: this server will not serve documents.
#  port:	The port this server is serving
# Emacspeak will use 2222 
#  listen:	the main listening socket id
#  accepts:	a count of accepted connections so far

# HTTP/1.0 error codes (the ones we use)
set httpdLog stderr
array set HttpdErrors {
    204 {No Content}
    400 {Bad Request}
    404 {Not Found}
    503 {Service Unavailable}
    504 {Service Temporarily Unavailable}
    }

# Our Url to command map 

array set httpdUrl2CmdMap {
    /say httpd_say
    /stop httpd_stop
    /isSpeaking httpd_isSpeaking
}

array set Httpd {
    bufsize	32768
    sockblock	0
}

# Start the server by listening for connections on the desired port.
# No document root,
# and possibly use default for a status page.
proc Httpd_Server {root {port 80} {default index.html}} {
    global Httpd

    array set Httpd [list root $root default $default]
    if {![info exists Httpd(port)]} {
	set Httpd(port) $port
	set Httpd(listen) [socket -server HttpdAccept $port]
	set Httpd(accepts) 0
    }
    return $Httpd(port)
}

# Accept a new connection from the server and set up a handler
# to read the request from the client.

proc HttpdAccept {newsock ipaddr port} {
    global Httpd
    upvar #0 Httpd$newsock data

    incr Httpd(accepts)
    fconfigure $newsock -blocking $Httpd(sockblock) \
	-buffersize $Httpd(bufsize) \
	-translation {auto crlf}
    Httpd_Log $newsock Connect $ipaddr $port
    set data(ipaddr) $ipaddr
    fileevent $newsock readable [list HttpdRead $newsock]
}

# read data from a client request

proc HttpdRead { sock } {
    upvar #0 Httpd$sock data

    set readCount [gets $sock line]
    puts stderr "Read $readCount bytes into <$line>"
    if {![info exists data(state)]} {
	if [regexp {(POST|GET) ([^?]+)\??([^ ]*) HTTP/1.0} \
		$line x data(proto) data(url) data(query)] {
	    puts stderr "$data(proto) $data(url) $data(query)"
	    set data(state) mime
	    Httpd_Log $sock Query $line
	} else {
	    HttpdError $sock 400
	    Httpd_Log $sock Error "bad first line:$line"
	    HttpdSockDone $sock
	}
	return
    }

    # string compare $readCount 0 maps -1 to -1, 0 to 0, and > 0 to 1
    
    set state [string compare $readCount 0],$data(state),$data(proto)
    switch -- $state {
	0,mime,GET	-
	0,query,POST	{ HttpdRespond $sock }
	0,mime,POST	{ set data(state) query }
	1,mime,POST	-
	1,mime,GET	{
	    if [regexp {([^:]+):[ 	]*(.*)}  $line dummy key value] {
		set data(mime,[string tolower $key]) $value
	    }
	}
	1,query,POST	{
	    set data(query) $line
	    HttpdRespond $sock
	}
	default {
	    if [eof $sock] {
		Httpd_Log $sock Error "unexpected eof on <$data(url)> request"
	    } else {
		Httpd_Log $sock Error "unhandled state <$state> fetching <$data(url)>"
	    }
	    HttpdError $sock 404
	    HttpdSockDone $sock
	}
    }
}

# Close a socket.
# We'll use this to implement keep-alives some day.

proc HttpdSockDone { sock } {
    upvar #0 Httpd$sock data
    unset data
    close $sock
}

# Respond to the query.
# map URLs to TTS commands.

proc HttpdRespond { sock } {
    global Httpd httpdUrl2CmdMap
    upvar #0 Httpd$sock data

    set cmd   $data(url)
    puts stderr "Handler: $cmd"
    if {[string length $cmd] == 0} {
	HttpdError $sock 400
	Httpd_Log $sock Error "$data(url) in29valid path"
	HttpdSockDone $sock
	return
    }
# handle request, and send out a 200 response on success:
    if {[info exists httpdUrl2CmdMap($cmd)]} {
	puts stderr "handling $cmd"
	puts $sock "HTTP/1.0 200 Data follows"
	puts $sock "Date: [HttpdDate [clock clicks]]"
	puts $sock "Content-Type: text/plain"
	puts $sock "Content-Length: 0"
	puts $sock ""
	flush $sock
	HttpdSockDone $sock
    } else {
	# 404 on unknown request
puts stderr "unknown cmd $cmd"
	HttpdError $sock 404
	Httpd_Log $sock Error "$data(url) "
	HttpdSockDone $sock
    }
}


# convert the file suffix into a mime type
# add your own types as needed
array set HttpdMimeType {
    {}		text/plain
}

proc HttpdContentType {path} {
    global HttpdMimeType

    set type text/plain
    catch {set type $HttpdMimeType([file extension $path])}
    return $type
}

# Generic error response.

set HttpdErrorFormat {
    <title>Error: %1$s</title>
    Got the error: <b>%2$s</b><br>
    while trying to obtain <b>%3$s</b>
}

proc HttpdError {sock code} {
    upvar #0 Httpd$sock data
    global HttpdErrors HttpdErrorFormat

    append data(url) ""
    set message [format $HttpdErrorFormat $code $HttpdErrors($code)  $data(url)]
    puts $sock "HTTP/1.0 $code $HttpdErrors($code)"
    puts $sock "Date: [HttpdDate [clock clicks]]"
    puts $sock "Content-Length: [string length $message]"
    puts $sock ""
    puts $sock $message
}

# Generate a date string in HTTP format.

proc HttpdDate {clicks} {
    return [clock format $clicks -format {%a, %d %b %Y %T %Z}]
}

# Log an Httpd transaction.
# This should be replaced as needed.

proc Httpd_Log {sock reason args} {
    global httpdLog httpClicks
    if {[info exists httpdLog]} {
	if ![info exists httpClicks] {
	    set last 0
	} else {
	    set last $httpClicks
	}
	set httpClicks [clock clicks]
	puts $httpdLog "[clock format [clock seconds]] ([expr $httpClicks - $last])\t$sock\t$reason\t[join $args { }]"
    }
}

# Convert a url into a command 

proc HttpdUrl2Cmd {root url} {
    global  Httpd  HttpdUrl2CmdMap
    	
    	# split out cmd and do the mapping
    # url decode via HttpdUrlDecode
    return $HttpdCmdMap($url)
}

# Decode url-encoded strings.

proc HttpdUrlDecode {data} {
    regsub -all {([][$\\])} $data {\\\1} data
    regsub -all {%([0-9a-fA-F][0-9a-fA-F])} $data  {[format %c 0x\1]} data
    return [subst $data]
}

proc bgerror {msg} {
    global errorInfo
    puts stderr "bgerror: $msg\n$errorInfo"
}



Httpd_Server "/tmp"  8080 index.html
puts stderr "Starting Tcl httpd server on [info hostname] port 8080"


vwait forever		;# start the Tcl event loop
