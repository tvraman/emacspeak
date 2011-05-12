"""Outloud voice definitions using ACSS.

This module encapsulates Outloud-specific voice definitions.  It
maps device-independent ACSS voice definitions into appropriate
Outloud voice parameter settings.

"""

__id__ = "$Id$"
__author__ = "$Author$"
__version__ = "$Revision$"
__date__ = "$Date$"
__copyright__ = "Copyright (c) 2005 T. V. Raman"
__license__ = "LGPL"

_defined_voices = {}

# Map from ACSS dimensions to Outloud settings:
def _update_map(table, key, format,  settings):
    """Internal function to update acss->synth mapping."""
    table[key] ={}
    for setting  in  settings:
        _table[key][setting[0]] = format % setting[1:]


_table ={}
#family codes:

_table['family'] = {
    'paul' :   " `v1 ",
    'male' :   " `v1 ",
    'harry' :  " `v1 `vh65 `vb50 ",
    'man' :  " `v1 `vh65 `vb50 ",
    'dennis' :  " `v1  `vb0 ",
    'frank' :  " `v1 `vr100 ",
    'betty' :  " `v7 ",
    'female' :  " `v7 ",
    'ursula' :  " `v2 ",
    'rita' :  " `v2 `vr100 ",
    'wendy' :  " `v2 `vy50 ",
    'kit' :  " `v3 ",
'child' :  " `v3 "
}
# Average pitch for standard male voice is 65 --this is mapped to
# a setting of 5.
# Average pitch varies inversely with speaker head size --a child
# has a small head and a higher pitched voice.
# We change parameter head-size in conjunction with average pitch to
# produce a more natural change on the TTS engine.

#male average pitch

_male_ap = [
    (0, 0, 90),
    (1, 13, 81, ),
    (2, 26, 72),
    (3, 39, 63),
    (4, 52, 54, ),
    (5, 65, 50),
    (6, 74, 40),
    (7, 83, 30, ),
    (8, 87, 26),
    (9, 92, 21)
    ]

_update_map(_table, ('male', 'average-pitch'),
            " `vb%s `vh%s ",  _male_ap)

#Harry  has a big head --and a lower pitch for the middle setting
_man_ap = [
    (0, 0, 90),
    (1, 10, 85, ),
    (2, 20, 80),
    (3, 30, 70),
    (4, 40, 60),
    (5, 50, 60),
    (6, 60, 50),
    (7, 70, 40, ),
    (8, 80, 30),
    (9, 90, 20)
    ]

_update_map(_table,('man', 'average-pitch'),
            " `vb%s `vh% s",_man_ap)
#defalt baseline is average pitch of 81

_female_ap = [
    (0, 5, 70),
    (1, 17, 66),
    (2, 33, 62),
    (3, 49, 58),
    (4, 65, 54, ),
    (5, 81, 50),
    (6, 85, 55),
    (7, 89, 60),
    (8, 93, 65),
(9, 97, 69)
    ]

_update_map(_table, ('female', 'average-pitch'),
            " `vb%s `vh% s",_female_ap)

# pitch-range for male:


#  Standard pitch range is 30 and is  mapped to
# a setting of 5.
# A value of 0 produces a flat monotone voice --maximum value of 100
# produces a highly animated voice.


_male_pr = [
    (0,0),
    (1,5),
    (2,15),
    (3,20),
    (4,25),
    (5,30),
    (6,47),
    (7,64),
    (8,81),
    (9,100)
    ]

_update_map(_table, ('male', 'pitch-range'),
            " `vf%s  ", _male_pr)

_man_pr = [
    (0, 0, ),
    (1, 5, ),
    (2, 15),
    (3, 20),
    (4, 25, ),
    (5, 30, ),
    (6, 47),
    (7, 64),
    (8, 81),
(9, 100)
    ]

_update_map(_table, ('man', 'pitch-range'),
            " `vf%s  ", _man_pr)

_female_pr = [
    (0, 0, ),
    (1, 5, ),
    (2, 15),
    (3, 20),
    (4, 25, ),
    (5, 30, ),
    (6, 47),
    (7, 64),
    (8, 81),
    (9, 100)
    ]

_update_map(_table, ('female', 'pitch-range'),
            " `vf%s  ", _female_pr)

# Stress:
# On the outloud we map stress to roughness

_male_stress =[
    (0, 0),
    (1, 5),
    (2, 10),
    (3, 15),
    (4, 20, ),
    (5, 25, ),
    (6, 30),
    (7, 35),
    (8, 40),
    (9, 45)
    ]

_update_map(_table, ('male', 'stress'),
            " `vr%s  ", _male_stress)

#Same stress values work for female and man:

_update_map(_table, ('man', 'stress'),
            " `vr%s  ", _male_stress)
  
_update_map(_table, ('female', 'stress'),
            " `vr%s  ", _male_stress)

#richness

# Smoothness and richness vary inversely.
# a  maximally smooth voice produces a quieter effect
# a rich voice is "bright" in contrast.


_male_richness = [
    (0, 0, 60),
    (1, 4, 78),
    (2, 8, 80),
    (3, 12, 84),
    (4, 16, 88),
    (5, 20, 92),
    (6, 24, 93),
    (7, 28, 95),
    (8, 32, 97, ),
    (9, 36, 100)
    ]
                  
_update_map(_table, ('male', 'richness'),
            " `vy%s  `vv%s " ,_male_richness)

#same settings work for man and female:

_update_map(_table, ('man', 'richness'),
            " `vy%s  `vv%s " , _male_richness)

_update_map(_table, ('female', 'richness'),
            " `vy%s  `vv%s ", _male_richness)

# getrate is here for symmetry with other engines:
# In the case of outloud, we dont need to normalize

def getrate(r): return r

def getvoicelist(): return _table['family'].keys()

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
    family ='male'
    if 'family'in acss:
        family = acss['family']
        code += _table['family'][family]
    if 'rate' in acss: code += " `vs%s" % acss['rate']
    voice = ""
    for d in ['average-pitch', 'pitch-range',
              'richness', 'stress']:
        if d in acss:voice += _table[(family, d)][acss[d]]
    if code or voice: code = "%s %s" % (code, voice)
    return (code, " `v1 ")
