#!/bin/sh
emacsclient -e '(read-passwd "OpenSSH Password:  ")' | sed 's/^"//; s/"$//'
