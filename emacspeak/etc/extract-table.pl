#!/usr/bin/perl -w
#$Id$
# Accepts a URI and table spec
#returns a csv file
use strict;
use FileHandle;
use LWP::UserAgent;
use HTML::TableExtract;
use IO::File;
use Getopt::Long;
use vars qw (%options);
my %options = ();
GetOptions (\%options,
"task=s",
           "url=s",
"file=s",
           "depth=i",
"count=i");
$options{task} ||= "extract-table";
my $input;
if (defined ($options{file})) {
  $input = $options{file};
} else {
  $input="/tmp/$options{task}.html";
RetrieveURLToFile($options{url}, $input);
}
my $te = new HTML::TableExtract( depth => 2, count=>7);
$te->parse_file($input);
my $output = new FileHandle (">  /tmp/$options{task}.csv");
my $row;
foreach $row ($te->rows) {
  $output->print(  join(',', @$row),"\n");
}
$output->close();
if (defined ($options{url})) {
  unlink ($input);
}
# {{{  retrieve URL to file

sub RetrieveURLToFile {
  my ($url, $filename) = @_;
  my $ua = new LWP::UserAgent;
  # Create a request
  my $req = new HTTP::Request( 'GET' => $url);
  # Pass request to the user agent and get a response back
  my $res = $ua->request($req, $filename);
  if ($res->is_success()) {
    warn"table: Retrieved $url to $filename\n";
  } elsif ($res->is_error()) {
    exit ("Retrieval for $url failed\n");
  }
}

# }}}
1;
