#!/usr/bin/perl -w
#$Id: ocr-client.pl 4047 2006-08-11 19:11:17Z tv.raman.tv $
#Description: Invoke ocropus client. Pipe result to stdout
#Usage: ocr-client.pl image-file
use strict;
use File::Temp qw(tempfile);
use File::Basename;

my $OCR = 'tesseract';
my $lang='eng'; #fix to taste
my ($out, $output) = tempfile(suffix=>'.txt');
my $image =shift;
my $cat = $output . ".txt";
die "No image specified" unless defined ($image);
qx($OCR  $image  $output -l $lang 2>/dev/null);
open(CAT,"cat -s $cat |");
while (<CAT>) {
  print;
}
unlink $cat;
