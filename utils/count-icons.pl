#!/usr/bin/perl -w
# Reads Emacspeak Lisp sources,
# Look for calls to emacspeak-auditory-icon,
# Extract icon names,
# And count them.

use strict;
my $pattern = qq@\"\(emacspeak-auditory-icon\"@; #pattern  to match calls
my @raw = qx(cat ../lisp/*.el | grep $pattern); #matching lines 
chomp(@raw);

# Load in defined icon names.
my @icons = qx(ls ../sounds/chimes/*.ogg);
chomp(@icons);
my %hash;

foreach my $w (@icons) { # clean up icon names 
  $w =qx(basename $w .wav);
  chomp($w);
  $hash{$w}=0;
}

foreach my $i  (@raw) {
  if ($i =~ m/\(if/) {
    $hash{off}++;
    $hash{on}++;
    next;
  }
  $i =~ m/\'([a-z-]+)/;
  $i = $1 if defined ($1); #icon name
  next unless defined ($i);
  if (defined ($hash{$i})) {
    $hash{$i}++;
  } else {
    $hash{$i} = 1;
  }
}

# Report icons sorted by usage
print "Icon Name:\tCount\n";
foreach  my $k   (sort {$hash{$a} <=>  $hash{$b} or $a cmp $b } keys %hash) {
  print "$k:\t$hash{$k}\n";
}
