#!/usr/bin/perl -i
#replace old with new 
my $old=shift || die "What should I change?";
my $new = shift || die "What should I change it to?";
while ( <>) {
  s/$old/$new/go;
  print;
}
