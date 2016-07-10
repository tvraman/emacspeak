#!/usr/bin/perl -w
use strict;
use Finance::Quote;
my @symbols = <>;
chomp (@symbols);
my @labels=qw(
  name ask avg_vol bid cap
  close currency date day_range
  div div_date div_yield eps
  ex_div high  last
  low   net
  open p_change pe price
  success symbol time volume year_range
    );

my $q = Finance::Quote->new;
my %quotes  = $q->fetch("usa",@symbols);

print join(",", @labels), "\n";
foreach my $sym (@symbols) {
  foreach my $l (@labels) {
my $v = $quotes{$sym, $l};
    $v ="N/A" unless defined($v);
print $v, ", ";
  }
  print "\n";
}
