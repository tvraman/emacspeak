#!/usr/bin/perl -w produce ram file for today's All Things
#Considered.  We need this gross hack because NPR runs on
#the darkside and doesn't know about symlinks (cringe)
open (ATC, ">atc.ram") 
  or die "Cannot write atc.ram $!";
my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime();
$year +=1900;
$mon += 1;
my $today = sprintf("%d%02d%02d", $year, $mon, $mday);
print ATC "pnm://audio.npr.org/atc/$today.atc.ra\n";
close (ATC);
