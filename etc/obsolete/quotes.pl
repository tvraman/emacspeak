#!/usr/bin/perl -w
#$Id$
#use Finance::YahooQuote;
use Finance::Quote;
my @symbols = <>;
chomp (@symbols);
my @labels=qw(
              Symbol
              Company
              Last-Price
              Last-Trade-Date
              Last-Trade-Time
              Change
              Percent-Change
              Volume
              Average-Daily-Vol
              Bid
              Ask
              Close
              Open
              Day-Range
              52-Week-Range
              EPS
              P/E
              Dividend-Pay-Date
              Dividend-per-Share
              Dividend-Yield
              Market-Capitalization
              Stock-Exchange
             );

#@quotes = getquote @symbols;
$q = Finance::Quote->new;
#$q->require_labels(@labels);
$quotes  = $q->fetch("usa",@symbols);
#print join (", ", @labels ),"\n";
my $sep = q(",");


  foreach $sym (@symbols) {
    print "Sym: $sym\n";
     foreach $key (keys $quotes->{$sym}) {
  print "$quotes{$key},, ";
  }
     print "\n";
}
