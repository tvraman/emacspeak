#!/usr/local/bin/perl -i.bak
#$Id$
#Update Copyright notice
#

my $old = "1995 -- 2000, T. V. Raman";
my $new = "1995 -- 2002, T. V. Raman";

while (<>) {
    s/$old/$new/o;
    print;
}
