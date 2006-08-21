#!/usr/local/bin/perl -i.bak
#$Id$
#Turn off dynamic loading on specified file
while ( <>) {
  print;
  if (m/^;;; byte-compile-dynamic: t$/) {
    print ";;; byte-compile-dynamic: nil\n";
  }
}
