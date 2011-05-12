#!/usr/local/bin/perl -i.bak
#$Id$
#Let's  turn on dynamic loading 
while ( <>) {
  print;
  if ( m/^;;; folded-file: t$/) {
    print ";;; byte-compile-dynamic: t\n";
  }
}
