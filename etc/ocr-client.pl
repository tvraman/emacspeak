#!/usr/bin/perl -w
#$Id$
#Description: Invoke ocrxtr client. Pipe result to stdout
#Usage: ocr-client.pl image-file
use strict;
use File::Temp qw(tempfile);
use File::Basename;

my $OCR = 'tesseract';
my $image =shift;
die "No image specified" unless defined ($image);

my ($name,$path,$suffix) = fileparse($image,"\.tiff");
my ($in, $input)  = tempfile(suffix=>'.tiff');
my ($out, $output) = tempfile(suffix=>'.txt');
qx($OCR $image $output 2>/dev/null);
open (OUT, "cat -s $output |");
while ( <OUT>) {
    print;
}
unlink $output;

