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

# Map from ACSS dimensions to Dectalk settings:

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

def _update_map(table, key, format, settings):
    """Internal function to update acss->synth mapping."""
    table[key] ={}
    for (s, ap, hs)  in  settings:
        _table[key][s] = format % (ap, hs)

_paul_ap = [
    (0, 96, 115),
    (1, 101, 112),
    (2, 108, 109),
    (3, 112, 106),
    (4, 118, 103, ),
    (5, 122, 100),
    (6, 128, 98),
    (7, 134, 96),
    (8, 140, 94),
    (9, 147, 91)]

_update_map(_table, ('paul', 'average-pitch'),
            " ap %s hs %s ",  _paul_ap)

#Harry  has a big head --and a lower pitch for the middle setting
_harry_ap = [
    (0, 50, 125),
    (1, 59, 123),
    (2, 68, 121),
    (3, 77, 120),
    (4, 83, 118, ),
    (5, 89, 115),
    (6, 95, 112),
    (7, 110, 105),
    (8, 125, 100),
    (9, 140, 95)
    ]

_update_map(_table,('harry', 'average-pitch'),
            " ap %s hs %s ",_harry_ap)

_betty_ap = [
    (0, 160, 115),
    (1, 170, 112),
    (2, 181, 109),
    (3, 192, 106),
    (4, 200, 103, ),
    (5, 208, 100),
    (6, 219, 98),
    (7, 225, 96),
    (8, 240, 94),
    (9, 260, 91)
    ]

_update_map(_table, ('betty', 'average-pitch'),
            " ap %s hs %s ",_betty_ap)

# pitch-range for paul:

#  Standard pitch range is 100 and is  mapped to
# a setting of 5.
# A value of 0 produces a flat monotone voice --maximum value of 250
# produces a highly animated voice.
# Additionally, we also set the assertiveness of the voice so the
# voice is less assertive at lower pitch ranges.

_paul_pr = [
    (0, 0, 0),
    (1, 20, 10),
    (2, 40, 20),
    (3, 60, 30),
    (4, 80, 40, ),
    (5, 100, 50, ),
    (6, 137, 60),
    (7, 174, 70),
    (8, 211, 80),
    (9, 250, 100),
    ]

_update_map(_table, ('paul', 'pitch-range'),
            " pr %s as %s ", _paul_pr)

_harry_pr = [
    (0, 0, 0),
    (1, 16, 20),
    (2, 32, 40),
    (3, 48, 60),
    (4, 64, 80, ),
    (5, 80, 100, ),
    (6, 137, 100),
    (7, 174, 100),
    (8, 211, 100),
    (9, 250, 100)
    ]

_update_map(_table, ('harry', 'pitch-range'),
            " pr %s as %s ", _harry_pr)

_betty_pr = [
    (0, 0, 0),
    (1, 50, 10),
    (2, 80, 20),
    (3, 100, 25),
    (4, 110, 30, ),
    (5, 140, 35),
    (6, 165, 57),
    (7, 190, 75),
    (8, 220, 87),
    (9, 250, 100)
    ]

_update_map(_table, ('betty', 'pitch-range'),
            " pr %s as %s ", _betty_pr)


def getvoice(acss):
    """Memoized function that returns  synthesizer code for
    specified  ACSS setting.
    Synthesizer code is a tupple of the form (open,close)
    where open sets the voice, and close resets it."""
    
    name=acss.name()
    if name in _defined_voices: return _defined_voices[name]
    _defined_voices[name] =acss2voice(acss)
    return _defined_voices[name]

def acss2voice(acss):
    """Return synthesizer code."""
    code = ""
    family ='paul'
    if 'family'in acss:
        family = acss['family']
        code += _table['family'][family]
    if 'rate' in acss: code += " :ra %s" % (180 +4 *
    acss['rate'])
    if 'punctuations' in acss: code += " :punc %s" %acss['punctuations']
    voice = ""
    dv = ""
    for d in ['average-pitch', 'pitch-range',
              'richness', 'stress']:
        if d in acss:voice += _table[(family, d)][acss[d]]
    if voice: dv = " :dv %s" % voice
    if code or voice: code = "[%s  %s]" % (code, dv)
    return (code, " [:np] ")
