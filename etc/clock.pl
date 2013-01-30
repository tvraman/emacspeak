#!/usr/bin/perl -w
#$Id: clock.pl,v 1.14 2006/05/14 21:26:59 tvraman Exp $
#chime the time
#Switching to MP3
use strict;

my ($sec,$min,$hour,$mDay,$mon,$year,$wDay,$yDay,$isdst) = localtime();
my $sounds="$ENV{HOME}/cues/chimes/mp3";
my %chimes =(
             0 => [qw(Sun Mon Tue Wed Thu Fri Sat)],
             15  => [qw(gf-15.wav chime-15.wav bigben-15.wav)],
             30 =>[qw (gf-30.wav chime-30.wav bigben-30.wav)],
             45 => [qw(gf-45.wav chime-45.wav bigben-45.wav)]
            );
my $chime;
exit unless defined ($chimes{$min});
if ($min == 0 ) {
  my $c = $chimes{0}[$wDay];
  $hour %= 12;
  $hour = 12 if ($hour == 0);
  $chime = "$sounds/$c/$hour.mp3";
  qx(mplayer $chime  2>/dev/null);
} else {
  $chime = $chimes{$min}[$hour  % 3];
  qx(mplayer -af extrastereo=0  $sounds/$chime 2>/dev/null);
}
