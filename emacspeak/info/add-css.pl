#!/usr/bin/perl-w  -i 
my $css='<link rel="http://www.w3.org/StyleSheets/Core/Modernist" type="text/css">';
my $logo=qq(<p> <a href="http://emacspeak.sf.net"><img width="150" height="216" src="emacspeak.jpg" alt= "EMACSPEAK --Complete Audio Desktop"></a> </p>);
while ( <>) {
  s@<head>@<head>$css@igo;
s@</address>@</address>$logo@igo;
s/^\s*\@title.*$//;
s/^\s*\@kindex.*//;
print;
}
