#!/usr/bin/perl -w
#$Id$
#Description: Bookshare downloader for Lynx
use strict;
my $location="$ENV{HOME}/books/book-share";
my $password = 'xxxxxx';
my $grabbed = shift;
my $target = shift;
my $dir =qx(basename $target .bks);
chomp $dir;
my $where = "$location/$dir";
qx(mkdir -p $where);
qx(mv $grabbed  $where/$target);
chdir $where;
qx(echo $password | bks-unpack -q $target 1>&- 2>&- &);
