"""ACSS --- Aural CSS.

Class ACSS defines a simple wrapper for holding ACSS voice
definitions.
Speech engines  implement the code for converting ACSS
definitions into engine-specific markup codes.

"""

class ACSS(dict):

    """Holds ACSS representation of a voice."""

    settings = {
                'family' : None,
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
        if props is not None: self.update(props)
