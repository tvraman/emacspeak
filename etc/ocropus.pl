#!/usr/bin/perl -w
#$Id: ocr-client.pl 4047 2006-08-11 19:11:17Z tv.raman.tv $
#Description: Invoke ocropus client. Pipe result to stdout
#Usage: ocr-client.pl image-file
use strict;
use File::Basename;

my $OCR = 'ocropus';
my $output = "/tmp/ocr-output-$$.html";
my $image =shift;
die "No image specified" unless defined ($image);
qx($OCR  ocr $image > $output 2>/dev/null);
open(OUTPUT, "lynx -dump $output 2>/dev/null | cat -s |");
while (<OUTPUT>) {
  print;
}
unlink $output;
