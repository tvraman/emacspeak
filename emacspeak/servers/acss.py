"""ACSS --- Aural CSS.

Class ACSS defines a simple wrapper for holding ACSS voice
definitions.  Speech engines implement the code for converting
ACSS definitions into engine-specific markup codes.

"""

__id__ = "$Id$"
__author__ = "$Author$"
__version__ = "$Revision$"
__date__ = "$Date$"
__copyright__ = "Copyright (c) 2005 T. V. Raman"
__license__ = "GPL"

class ACSS(dict):

    """Holds ACSS representation of a voice."""

    settings = {
                'family' : None,
                'rate' : 50,
                'gain' : 5,
                'left-volume' : 5,
                'right-volume' : 5,
                'average-pitch' : 5,
                'pitch-range' : 5, 
                'stress' : 5,
                'richness' : 5,
                'punctuations' : 'all'
                }

    def __init__(self,props=None):
        """Create and initialize ACSS structure."""
        self.update(ACSS.settings)
        if props is not None:
            [self[k] = props[k] for k in props if k in ACSS.settings]
