#!/usr/bin/perl -w #chime the time

use strict;
$ENV{LADSPA_PATH} = "/usr/lib/ladspa";
$ENV{XDG_RUNTIME_DIR}="/run/user/`id -u`";
# Update  sounds location  to match your installation:
my $sounds="$ENV{HOME}/emacs/lisp/emacspeak/sounds/clock";
my ($sec,$min,$hour,$mDay,$mon,$year,$wDay,$yDay,$isdst) = localtime();
my %chimes =(
    0 => [qw(Sun Mon Tue Wed Thu Fri Sat)],
    15  => [qw(gf-15.mp3 wm-15.mp3 chime-15.mp3 bigben-15.mp3)],
    30 =>[qw (gf-30.mp3 wm-30.mp3 chime-30.mp3 bigben-30.mp3)],
    45 => [qw(gf-45.mp3 wm-45.mp3 chime-45.mp3 bigben-45.mp3)]
    );
my $chime;
exit unless defined ($chimes{$min});
if ($min == 0 ) {
    my $c = $chimes{0}[$wDay];
    $hour %= 12;
    $hour = 12 if ($hour == 0);
    $chime = "$sounds/$c/$hour.mp3";
} else {
    my $c = $chimes{$min}[$hour % 4];
    $chime = "$sounds/$c";
}
# If ladspa not available, use next line.
#qx(mplayer -af bs2b $chime  2>&1  >/dev/null);
qx(mplayer -af bs2b,ladspa=tap_reverb:tap_reverb:5000:-4:-18:1:1:1:1:6 $chime  2>&1  >/dev/null);

