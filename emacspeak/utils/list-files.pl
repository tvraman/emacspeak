#!/usr/bin/perl -w
#Author: T. V. Raman <raman@cs.cornell.edu>
#list al files 

#
use strict;
use File::Find;

sub wanted {
     -e and not -d  and print "$File::Find::name\n";
}
my $dir = shift;
$dir ||=".";
find (\&wanted, $dir);


# {{{  end of file

#local variables:
#folded-file: t
#end:

 # }}}
