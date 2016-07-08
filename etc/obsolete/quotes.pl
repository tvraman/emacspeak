#!/usr/bin/perl -w
#$Id$
use Finance::Quote;
#my @symbols = <>;
#chomp (@symbols);
my @symbols =qw/ibm msft adbe goog googl fb/;

my @labels=qw(
  ask avg_vol bid cap
close currency date day_range
div div_date div_yield eps
ex_div high isodate last
low method name net
open p_change pe price
success symbol time volume
year_range);

$q = Finance::Quote->new;
#$q->require_labels(@labels);
%quotes  = $q->fetch("usa",@symbols);
#print join (", ", @labels ),"\n";
my $sep = q(",");

my $cl='';
print join(",", @labels), "\n";
foreach $sym (@symbols) {
  foreach $l (@labels) {
my $v = $quotes{$sym.$cl.$l},;
    print $v, ","  if defined ($v);
  }
  print "\n";
}
