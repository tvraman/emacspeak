#!/usr/bin/perl -i -w
#
my $old=qq(http://www-4.ibm.com/software/speech/dev/ttssdk_linux.html);
my $new=qq(http://www.redhat.com/services/techsupport/accessibility/s1-access-install.html);
while ( <>) {
  s/$old/$new/go;
  print
}
