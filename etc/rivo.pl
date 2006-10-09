#!/usr/bin/perl -w
#record radio for later playback
#Rewritten to use mplayer

use strict;
use vars qw(%options);
use Getopt::Std;
getopts('c:d:s:o:', \%options);
die "Usage: $0 -c channel -d directory  -s stop-time -o output\n"
  unless (defined ($options{d})
          and defined($options{o})
          and defined ($options{c})
          and defined($options{s}));
chdir($options{d});
my $wav="$$.wav";
$options{o} .=".mp3" unless ($options{o} =~ m/\.mp$/);
qx(echo "pkill mplayer" | at $options{s});
qx(mplayer -quiet -vc null -vo null  -ao pcm:file=$wav -playlist $options{c} );
qx(lame --quiet $wav $options{o});

unlink($wav);
