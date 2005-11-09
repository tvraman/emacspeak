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
                'average-pitch' : 5,
                'pitch-range' : 5, 
                'stress' : 5,
                'richness' : 5,
                'punctuations' : 'all'
                }

    def __init__(self,props={}):
        """Create and initialize ACSS structure."""
        for k in props:
            if k in ACSS.settings: self[k] = props[k]
        self._name='acss-'
        names = self.keys()
        if names:
            names.sort()
            for  k in names:
                self._name = self._name + "%s-%s:" % (k, self[k])
        self._name = self._name[:-1]
        
    def name(self): return self._name
        
