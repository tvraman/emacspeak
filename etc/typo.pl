#!/usr/bin/perl -i
#fix typo 
my $orig = shift || die "What should I change?";
my $new = shift || die "What should I change $orig  to?";
while ( <>) {
  s/\b$orig\b/$new/go;
  print;
}
