#!/usr/bin/perl -w
#$Id$
#Description: Print out hostname wher ewe logged in from:
use strict;
#see /usr/include/utmp.h
use constant  RECORD_SIZE => 292;
use constant RECORD_FORMAT =>'l A32 A256';
my $CONSOLE_SND  = 'play /usr/share/sounds/startup3.wav';
my $user = shift;
$user ||= $ENV{LOGNAME};
my $u =qx(id -u);
die "User $user not found on this system" unless defined ($u);
chomp($u);

my $lastlog  = "/var/log/lastlog";
open(LASTL, $lastlog) or die "Cannot read lastlog $!";
my $offset = $u * RECORD_SIZE  ;
seek (LASTL, $offset, 0);
my $record;
read(LASTL, $record, RECORD_SIZE);
my ($time, $line, $host) = unpack(RECORD_FORMAT, $record);
if ( $host) {
my $home=$ENV{HOME};
open (OUT, "> $home/.emacspeak/.current-remote-hostname") or die "Cannot write output $!";
print OUT $host, "\n";
close OUT;
} else {
  qx($CONSOLE_SND);
}
close LASTL;
