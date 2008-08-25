#!/usr/bin/perl -w
#$Id$
# Accepts a URI and table spec
#returns a csv file
use strict;
use FileHandle;
use LWP::UserAgent;
use HTML::TableExtract;
use IO::File;
use File::Temp qw(tempfile);
use Getopt::Long;
use vars qw (%options);
my ($url, $file, $task, $depth, $count, $cols);

my %options = (task => \$task,
           url => \$url,
file => \$file,
           depth => \$depth,
count => \$count,
headers => \$cols);
GetOptions (\%options,
            'file=s',
            'url=s',
            'task=s',
            'depth=i',
            'count=i',
            'headers=s');
$task ||= "extract-table";
my ($input, $inputname);
if (defined ($file)) {
  $input = $file;
} else {
    ($input, $inputname) = tempfile( $task.'XXXX', SUFFIX => '.html');
  RetrieveURLToFile($url, $inputname);
}

my $te;
if ( defined ($cols)) {
  my @headers = split(',', $cols);
  $te = new HTML::TableExtract(headers=>\@headers);
} else {
 $te = new HTML::TableExtract( depth => $depth, count=>$count); 
}
$te->parse_file($input);
my ($ts,$row);
my $output =\*STDOUT;
foreach $ts ($te->table_states) {
          foreach $row ($ts->rows) {
             $output->print ( join(',', @$row), "\n");
          }
        }

$output->close();

if (defined ($url)) {
  unlink ($inputname);
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
      die ("Retrieval failed  for $url");
  }
}

# }}}
1;
