"""Interface Speakable --- Generic   Speakable objects.

Producing good quality spoken output requires knowledge of the
content. Such knowledge includes:

    Custom parsing rules for determining clause boundaries.
    Custom pronunciation rules. 

In general, it is not practical for a generic Speaker object to
be aware of every type of speakable content. Examples of
Speakable content include:

    Email messages.
    Web pages.
    Electronic books.
    Program source code in different languages.

Interface Speakable is defined as a light-weight interface that
can be extended and customize by different content-types. The
default implementation provides a generic parser and
pronunciation dictionary.

Objects implementing Interface Speakable can be passed to method
Speaker.render to produce well-formatted auditory output.

Methods defined by Interface Speakable:

Speakable objects implement a generic iterator/generator
interface so that one can write code within method Speaker.render
of the form:

    for clause in SpeakableParagraph:s.say(clause)

This allows object Speaker to chunk content in a manner
appropriate to the content-type being spoken.

Within the implementation of the iterator, one can customize how
a particular 'clause' is rendered by applying custom
pronunciation rules.

"""



class Speakable:

    """Default implementation of Interface Speakable. """


    def __init__(self, content=""):
        """Initialize Speakable with content.

        Content is held by reference."""
        self.content  = content
    

        

        



    
