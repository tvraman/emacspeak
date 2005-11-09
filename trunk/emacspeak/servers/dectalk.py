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

def getvoice(acss):
    """Memoized function that returns  synthesizer code for
    specified  ACSS setting.
    Synthesizer code is a tupple of the form (open,close)
    where open sets the voice, and close resets it."""
    name=acss.name()
    if name in _defined_voices: return _defined_voices[name]
    _defined_voices[name] =acss2voice(acss)
    return _defined_voices[name]
    
    
