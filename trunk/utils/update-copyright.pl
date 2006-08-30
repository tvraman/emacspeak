#!/usr/bin/perl -i
#$Id$
#Update Copyright notice
#

my $old = "1995 -- 2004, T. V. Raman";
my $new = "1995 -- 2006, T. V. Raman";

while (<>) {
    s/$old/$new/o;
    print;
}
