#!/usr/bin/perl-w  -i 
my $css='<link rel="http://www.w3.org/StyleSheets/Core/Modernist" type="text/css">';
while ( <>) {
  s@<head>@<head>$css@igo;
s/^\s*\@title.*$//;
s/^\s*\@kindex.*//;
print;
}
