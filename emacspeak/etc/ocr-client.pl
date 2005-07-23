#!/usr/bin/perl -w
#$Id$
#Description: Invoke ocrxtr client. Pipe result to stdout
#Usage: ocr-client.pl image-file [hostname]
use strict;
use File::Basename;

my $OCR = 'ocrxtr';
my $image =shift;
die "No image specified" unless defined ($image);
my $host =shift;
my ($name,$path,$suffix) = fileparse($image,"\.tiff");
my $input = "/tmp/$$-$name.tiff";
my $output = "/tmp/ocr-output-$$.txt";
$host ='localhost' unless defined ($host);
if ( $host =~ m/localhost/) {
  qx($OCR -out_text_name $output $image 2>/dev/null);
  open (OUT, "cat -s $output |");
  while ( <OUT>) {
    print;
  }
  unlink $output;
} else {
  qx(scp $image  $host:$input);
  qx(ssh $host $OCR -out_text_name $output $input 2>&1 > /dev/null);
  open (OUT, "ssh $host cat -s $output |");
  while (<OUT>) {
    print;
  }
  qx(ssh $host rm $input $output);
}
