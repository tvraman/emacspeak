#!/usr/bin/perl -w
#$Id: clock.pl,v 1.14 2006/05/14 21:26:59 tvraman Exp $
#chime the time
#Switching to MP3
use strict;
# Update ${EMACSPEAK_DIR} below to match your installation:
my $sounds="$ENV{EMACSPEAK_DIR}/sounds/clock/";
my ($sec,$min,$hour,$mDay,$mon,$year,$wDay,$yDay,$isdst) = localtime();
my %chimes =(
    0 => [qw(Sun Mon Tue Wed Thu Fri Sat)],
    15  => [qw(gf-15.mp3 wm-15.mp3 chime-15.mp3 bigben-15.mp3)],
    30 =>[qw (gf-30.mp3 wm-30.mp3 chime-30.mp3 bigben-30.mp3)],
    45 => [qw(gf-45.mp3 wm-45.mp3 chime-45.mp3 bigben-45.mp3)]
    );
my $chime;
$min=0;
exit unless defined ($chimes{$min});
if ($min == 0 ) {
    my $c = $chimes{0}[$wDay];
    $hour %= 12;
    $hour = 12 if ($hour == 0);
    $chime = "$sounds/$c/$hour.mp3";
} else {
    $chime = "$sounds/". ($chimes{$min}[$hour  % 4]);
}
print $chime,"\n";
qx(mplayer -af bs2b $chime  2>&1  >/dev/null);
