#!/usr/bin/perl -i
#$Id$
#change voice-lock to emacspeak-personality 
my $old="require \'voice-lock";
my $new="require \'emacspeak-personality";
while ( <>) {
  s/$old/$new/go;
print;
}
