#!/usr/bin/perl -i
#fix typo 
my $orig = shift || die "What should I change?";
print STDERR "orig: $orig\n";
my $new = shift || die "What should I change $orig  to?";
print STDERR "new: $new \n";
while ( <>) {
    #s/\b$orig\b/$new/go;
  s/$orig/$new/go;
  print;
}
