"""Dectalk voice definitions using ACSS.

This module encapsulates Dectalk-specific voice definitions.  It
maps device-independent ACSS voice definitions into appropriate
Dectalk voice parameter settings.

"""

__id__ = "$Id$"
__author__ = "$Author$"
__version__ = "$Revision$"
__date__ = "$Date$"
__copyright__ = "Copyright (c) 2005 T. V. Raman"
__license__ = "GPL"


import acss

_defined_voices = {}
#Will hold mapping from ACSS dimensions to Dectalk settings:

_table ={}
#family codes:

_table['family'] = {
    'paul' :  ':np',
    'harry' :  ':nh',
    'dennis' :  ':nd',
    'frank' :  ':nf',
    'betty' :  ':nb',
    'ursula' :  ':nu',
    'wendy' :  ':nw',
    'rita' :  ':nr',
    'kid' :  ':nk'
    }

# average-pitch :
# Average pitch for standard male voice is 122hz --this is mapped to
# a setting of 5.
# Average pitch varies inversely with speaker head size --a child
# has a small head and a higher pitched voice.
# We change parameter head-size in conjunction with average pitch to
# produce a more natural change on the Dectalk.

#paul average pitch
_table[('paul', 'average-pitch')] ={}
for (setting, ap, hs)  in [(0, 96, 115),
             (1, 101, 112),
             (2, 108, 109),
             (3, 112, 106),
             (4, 118, 103, ),
             (5, 122, 100),
             (6, 128, 98),
             (7, 134, 96),
             (8, 140, 94),
             (9, 147, 91)]:
    _table[('paul', 'average-pitch')][setting] = "ap %s hs %s" % (ap, hs)
             
def getvoice(acss):
    """Memoized function that returns  synthesizer code for
    specified  ACSS setting.
    Synthesizer code is a tupple of the form (open,close)
    where open sets the voice, and close resets it."""
    name=acss.name()
    if name in _defined_voices: return _defined_voices[name]
    _defined_voices[name] =acss2voice(acss)
    return _defined_voices[name]
