#!/usr/bin/perl-w  -i 
my $css='<link rel="http://www.w3.org/StyleSheets/Core/Modernist" type="text/css">';
while ( <>) {
  s@<head>@<head>$css@igo;
print;
}
