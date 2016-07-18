#!/usr/bin/perl -w
# Reads Emacspeak Lisp sources,
# Looks for calls to emacspeak-auditory-icon,
# Extracts icon names,
# And counts them.

use strict;
my $pattern = qq@\"\(emacspeak-auditory-icon\"@; #pattern  to match calls
my @raw = qx(cat ../lisp/*.el | grep $pattern); #matching lines 
chomp(@raw);

# Load in defined icon names.
my @icons = qx(ls ../sounds/pan-chimes/*.wav);
chomp(@icons);
my %hash;
# clean up icon names 
foreach my $w (@icons) {
  $w =qx(basename $w .wav);
  chomp($w);
  $hash{$w}=0;
}

foreach my $i  (@raw) {
  $i =~ m/\'([a-z-]+)/;
  $i = $1; #icon name 
  next unless defined ($i);
  if (defined ($hash{$i})) {
    $hash{$i}++;
  } else {
    $hash{$i} = 1;
  }
}

# Report icons sorted by usage
foreach  my $k   (sort {$hash{$a} <=>  $hash{$b} or $a cmp $b } keys %hash) {
  print "$k: $hash{$k}\n";
}
