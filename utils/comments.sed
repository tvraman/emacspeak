#!/usr/bin/sed -f
# Beware: This ruins autoload cookies --- fixme
/^;;;/{
    1{p;d}
    /^;;;;* [[:upper:]][[:alnum:]]*:[ ]*$/{p;d}
    /^;;;.* ends here[. ]*$/{p;d}
    s/^;;;/;;/
}
