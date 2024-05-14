#!/usr/bin/perl -w #chime the time
# To install, first search for xxxxx and  update appropriately.

use strict;
$ENV{LADSPA_PATH} = "/usr/lib/ladspa";
# update xxxxx below for the loged-in user.

$ENV{XDG_RUNTIME_DIR}="/run/user/xxxxx";

# Update  sounds location  to match your installation:
my $sounds="$ENV{HOME}/emacs/lisp/emacspeak/sounds/clock";
my ($sec,$min,$hour,$mDay,$mon,$year,$wDay,$yDay,$isdst) = localtime();
my %chimes =(
    0 => [qw(Sun Mon Tue Wed Thu Fri Sat)],
    15  => [qw(gf-15.ogg wm-15.ogg chime-15.ogg bigben-15.ogg)],
    30 =>[qw (gf-30.ogg wm-30.ogg chime-30.ogg bigben-30.ogg)],
    45 => [qw(gf-45.ogg wm-45.ogg chime-45.ogg bigben-45.ogg)]
    );
my $chime;
exit unless defined ($chimes{$min});
if ($min == 0 ) {
    my $c = $chimes{0}[$wDay];
    $hour %= 12;
    $hour = 12 if ($hour == 0);
    $chime = "$sounds/$c/$hour.ogg";
} else {
    my $c = $chimes{$min}[$hour % 4];
    $chime = "$sounds/$c";
}
# update xxxxx below for logged-in user:
#qx(XDG_RUNTIME_DIR=/run/user/xxxxx; mplayer -volume 70 -af bs2b,ladspa=tap_reverb:tap_reverb:5000:-4:-18:1:1:1:1:6 $chime  2>&1  >/dev/null);
qx(XDG_RUNTIME_DIR=/run/user/xxxxx; mpv --af=haas $chime  2>&1  >/dev/null &);
