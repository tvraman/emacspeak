#!/usr/bin/env perl
# Cloned from https://gist.github.com/1143204.git
# You can use this script in a pipe. It's input will become an emacs buffer
# via emacsclient (so you need server-start etc.)

# See http://mark.aufflick.com/o/886457 for more information

# Copyright (C) 2011 by Mark Aufflick <mark@aufflick.com>
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

use strict;
use warnings;

use IO::Select;

my $emacsclient = "/usr/local/bin/emacsclient";

# This script uses emacsclient, be sure to have a running server session.
# A server-session can be started by "M-x server-start".

exit 1
    if 0 != system("$emacsclient -n --eval '(emacspeak-wizards-pipe)'");

my $s = IO::Select->new;
$s->add(\*STDIN);

while (1)
{
    # block until data available
    my $data = <STDIN>;

    # exit if STDIN closed
    exit(0)
        if ! $data;

    # keep reading while data is available, or we have a bunch of lines
    my $lines = 0;
    $data .= <STDIN>
        while $lines++ < 100 && $s->can_read(.5);

    # need to escape backslashes first, otherwise we end up escaping the backslashes
    # we're using to escape the quotes...
    $data =~ s/\\/\\\\/g;
    $data =~ s/"/\\"/g;
    $data =~ s/'/'\\''/g;
    system(qq{$emacsclient -n --eval '(with-current-buffer " *piped*" (goto-char (point-max)) (insert "} . $data . qq{"))'});
}
