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
        self.updateName()

    def __setitem__ (self, key, value):
        "Update name when we change values."
        dict.__setitem__(self, key, value)
        self.updateName()
    
    def updateName(self):
        """Update name based on settings."""
        _name='acss-'
        names = self.keys()
        if names:
            names.sort()
            for  k in names:
                _name += "%s-%s:" % (k, self[k])
        self.__dict__['_name'] = _name[:-1]

    def name(self): return self._name
