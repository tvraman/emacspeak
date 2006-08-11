#!/usr/local/bin/perl -i.bak
#$Id$
#Update my email
#
my $old =  "raman\@adobe.com";
my $new = "raman\@cs.cornell.edu";
while (<>) {
    s/$old/$new/o;
    print;
}
