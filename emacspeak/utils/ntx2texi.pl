#!/usr/local/bin/perl -w
#$Id$
#Author: T. V. Raman <raman@adobe.com>
#
#Description: convert latex to texinfo
#
use strict;
# {{{ patterns

my $section  = "\\section{(.*?)}";
my $index = "\\index{.*?}";
my $bold  = "{\\bf";
my $em = "{\\em";

# }}}
# {{{  transformwh

while ( <>) {
  s/$index//o;
  s/$section/@section{$1}/o;
  s/$bold/@b\{/o;
  s/$em/@emph\{/o;
  s@\\/@@;
  s/\\mdash//;
  s/%$//;
print;
}

# }}}
# {{{ end of file

#local variables:
#folded-file: t
#end:

 # }}}
