#!/usr/bin/sed -f
/^;;;/{
    1{p;d}
    /^;;;;* [[:upper:]][[:alnum:]]*:[ ]*$/{p;d}
    /^;;;.* ends here[. ]*$/{p;d}
    /^;;;###/{p;d}
    s/^;;;/;;/
}
